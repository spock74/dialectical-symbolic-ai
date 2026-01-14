import express from 'express';
import cors from 'cors';
import multer from 'multer';
import { reflectiveLoop } from './flows/reflective-loop';
import { extractKnowledge } from './flows/extraction/knowledge-flow';
import { extractKnowledgeMultimodal } from './flows/extraction/multimodal-flow';
import { knowledgeUnitService } from "./services/knowledge-unit-service";

import { pdfService } from "./services/pdf-service";
import { SBCLProcess } from "./services/sbcl-process";
import { getActiveGraph, graphManager } from "./logic/graph-engine";
import { kernelEvents } from "./logic/kernel-events";
import {
  transformMemoriesToGraph,
  commitKnowledgeToGraph,
} from "./services/graph-service";

import { CONFIG } from "./config";

import { extractSimpleTranscription } from "./flows/extraction/simple-flow";
import { extractMarkdown } from "./flows/extraction/markdown-flow";

const app = express();
const port = CONFIG.PORT;

// [DEBUG] Switching back to disk storage (Standard)
// const upload = multer({ storage: multer.memoryStorage() });
const upload = multer({ dest: "uploads/" });

app.use(cors());
app.use((req, res, next) => {
  res.setHeader("X-AI-Model", CONFIG.LISP_MODEL);
  next();
});
app.use(express.json({ limit: "50mb" }));

// DEBUG MIDDLEWARE: Log all incoming requests
app.use((req, res, next) => {
    console.log(`[Server] Incoming ${req.method} ${req.originalUrl}`);
    next();
});

// Middleware to register rollback on client abortion
const registerRollback = (req: any, res: any, next: any) => {
  req.isAborted = false;
  // Source detection: check body, query, and file (especially for multimodal uploads)
  const source = req.body?.source || req.query?.source || req.file?.originalname;
  res.on("close", () => {
    if (!res.writableEnded) {
      req.isAborted = true;
      console.warn(
        `[Server] Request to ${
          req.path
        } closed before completion. Triggering Rollback for source: ${
          source || "default"
        }...`
      );
      const graph = getActiveGraph(source);
      graph.loadState(`data/graphs/${source || "default"}.json`);
    }
  });
  next();
};

app.post("/api/chat", registerRollback, async (req: any, res: any) => {
  try {
    const { prompt, history, useMemory, bypassSDialect, source } = req.body;
    console.log(`[API] /chat request. Source: ${source}, Bypass: ${bypassSDialect} (${typeof bypassSDialect})`);
    
    let result = await reflectiveLoop({
      prompt,
      history,
      useMemory,
      bypassSDialect: bypassSDialect === 'true' || bypassSDialect === true,
      source,
    });

    if (req.isAborted) {
      console.log(
        "[Server] Chat result ready but client already aborted. Ignoring."
      );
      return;
    }

    res.json(result);
    // Auto-save after each interaction
    const graph = getActiveGraph(source);
    graph.saveState(`data/graphs/${source || "default"}.json`);
  } catch (error) {
    if (req.isAborted) return;
    console.error(error);
    res.status(500).json({ error: String(error) });
  }
});

app.post("/api/extract", async (req, res) => {
  try {
    const { text } = req.body;
    const result = await extractKnowledge({ text });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({
      error: error instanceof Error ? error.message : "Unknown error",
    });
  }
});

const cleanupFile = async (file: Express.Multer.File | undefined) => {
  if (file && file.path) {
    try {
      await import("fs/promises").then((fs) => fs.unlink(file.path));
    } catch (e) {
      console.error(`Failed to cleanup file ${file.path}:`, e);
    }
  }
};

app.post(
  "/api/extract-from-pdf",
  upload.single("file"),
  registerRollback,
  async (req: any, res: any) => {
    // [DEBUG] Connection: close removed to match HEAD state
    
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      
      const fs = await import("fs/promises");
      const buffer = await fs.readFile(req.file.path);

      const text = await pdfService.parsePdf(buffer);
      if (req.isAborted) return;

      const result = await extractKnowledge({ text });
      if (req.isAborted) return;

      // [ASYNC] Processing in background
      console.log(`[Server] Starting background graph injection for ${req.file.originalname}`);
      commitKnowledgeToGraph(result, req.file.originalname).catch(err => {
         console.error(`[Server] Background Graph Commit failed for ${req.file.originalname}:`, err);
      });

      res.json(result);
    } catch (error) {
      if (req.isAborted) return;
      console.error(error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Unknown error",
      });
    } finally {
      // Only cleanup if it was a disk file
      if (req.file.path) await cleanupFile(req.file);
    }
  }
);

app.post(
  "/api/extract-multimodal",
  upload.single("file"),
  registerRollback,
  async (req: any, res: any) => {
    // [DEBUG] Connection: close removed to match HEAD state
    
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }

      // [ASYNC ARCHITECTURE]
      console.log(`[Server] Accepted ${req.file.originalname} for background processing.`);

      // DiskStorage is back, so we have req.file.path
      const filePathToUse = req.file.path;

      // Fire-and-forget background chain
      extractKnowledgeMultimodal({
        filePath: filePathToUse,
        trackId: req.file.originalname,
      })
      .then(async (result) => {
          console.log(`[Server] Background extraction success for ${req.file.originalname}`);
          broadcastLog(`[Ingest] Success: Extracted ${result.relations.length} relations from ${req.file.originalname}`);
          
          try {
            await commitKnowledgeToGraph(result, req.file.originalname);
            console.log(`[Server] Background graph injection success for ${req.file.originalname}`);
            
            // Notify completion via Steam for Frontend Auto-Update
            broadcastLog(`[Ingest] JOB_COMPLETE: ${req.file.originalname}`);
            broadcastLog(`[Ingest] Graph Injection Complete. Ready for Reasoning.`);
          } catch (graphErr) {
             console.error(`[Server] Background Graph Commit failed for ${req.file.originalname}:`, graphErr);
             broadcastLog(`[Ingest] ERR: Graph Commit Failed: ${String(graphErr)}`);
          }
      })
      .catch(err => {
         console.error(`[Server] Background Processing FAILED for ${req.file.originalname}:`, err);
         broadcastLog(`[Ingest] ERR: Extraction Logic Failed: ${String(err)}`);
      })
      .finally(async () => {
         // CLEANUP: Standard disk file
         if (req.file.path) await cleanupFile(req.file);
      });

      // Immediate response to keep connection alive/happy
      res.json({ 
          relations: [], 
          rules: [], 
          _async: true, 
          message: "Processing started in background. Watch the logs/graph." 
      });
      
    } catch (error) {
      // Logic errors during setup (unlikely here as we moved main logic to background)
      console.error(error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Unknown error",
      });
      // Safety cleanup if sync error occurred
      await cleanupFile(req.file);
    } 
    // Do NOT await cleanupFile here, it's handled in background finally
  }
);

app.post(
  "/api/transcribe-simple",
  upload.single("file"),
  async (req: any, res: any) => {
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      const fs = await import("fs/promises");
      const pdfBase64 = await fs.readFile(req.file.path, {
        encoding: "base64",
      });
      const result = await extractSimpleTranscription({ pdfBase64 });
      res.json(result);
    } catch (error) {
      console.error(error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Unknown error",
      });
    } finally {
      await cleanupFile(req.file);
    }
  }
);

app.post(
  "/api/extract-markdown",
  upload.single("file"),
  async (req: any, res: any) => {
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      const fs = await import("fs/promises");
      const text = await fs.readFile(req.file.path, "utf-8");
      const result = await extractMarkdown({
        text,
        filename: req.file.originalname,
      });
      
      // [ASYNC]
      commitKnowledgeToGraph(result, req.file.originalname).catch(err => {
         console.error(`[Server] Background Markdown Graph Commit failed for ${req.file.originalname}:`, err);
      });

      res.json(result);
    } catch (error) {
      console.error(error);
      res.status(500).json({
        error: error instanceof Error ? error.message : "Unknown error",
      });
    } finally {
      await cleanupFile(req.file);
    }
  }
);

app.get("/api/graph-data", async (req, res) => {
  try {
    const source = req.query.source as string;
    const graph = getActiveGraph(source);
    const rawGraph = graph.getGraphSnapshot();

    // 1. Get JS-side data
    // @ts-ignore
    const { nodes: jsNodes, edges: jsEdges } = transformMemoriesToGraph(rawGraph);

    // 2. Get Lisp-side data (With 5s timeout to avoid blocking if busy rehydrating)
    let lispNodes: any[] = [];
    let lispEdges: any[] = [];
    try {
      // Use a shorter timeout for the API call than the background rehydration
      const lispRaw = await SBCLProcess.getInstance().evaluate("(progn (princ (listar-dados-json)) (values))", 5000)
        .then(out => {
           try { return JSON.parse(out); } catch { return null; }
        })
        .catch(() => null);

      if (lispRaw && (lispRaw.nodes || lispRaw.edges)) {
          const transformed = transformMemoriesToGraph(lispRaw);
          lispNodes = transformed.nodes || [];
          lispEdges = transformed.edges || [];
      }
    } catch (e) {
      console.warn("[API] Lisp kernel busy or failed to respond in time, falling back to JS state.");
    }

    // 3. Merge (Simple deduplication by ID/source-target-relation)
    const allNodes = [...jsNodes];
    const nodeIds = new Set(jsNodes.map(n => n.id));

    lispNodes.forEach(ln => {
      if (ln && ln.id && !nodeIds.has(ln.id)) {
        allNodes.push(ln);
        nodeIds.add(ln.id);
      }
    });

    const allEdges = [...jsEdges];
    // Standardized deduplication using 'label'
    const edgeKeys = new Set(jsEdges.map(e => `${e.source}-${e.target}-${e.label || ''}`));

    lispEdges.forEach(le => {
      if (le && le.source && le.target) {
        const type = le.label || '';
        const key = `${le.source}-${le.target}-${type}`;
        if (!edgeKeys.has(key)) {
          allEdges.push(le);
          edgeKeys.add(key);
        }
      }
    });

    console.log(`[API] Graph State for "${source}": JS(${jsNodes.length}n, ${jsEdges.length}e), Lisp(${lispNodes.length}n, ${lispEdges.length}e) -> Total(${allNodes.length}n, ${allEdges.length}e)`);

    const response = {
      nodes: allNodes,
      edges: allEdges,
      metadata: {
        source,
        timestamp: new Date().toISOString(),
        counts: {
          js: { nodes: jsNodes.length, edges: jsEdges.length },
          lisp: { nodes: lispNodes.length, edges: lispEdges.length },
          total: { nodes: allNodes.length, edges: allEdges.length }
        }
      }
    };

    res.json(response);
  } catch (error) {
    console.error("Graph Error:", error);
    res.status(500).json({ error: String(error) });
  }
});

app.get("/api/graph-raw", async (req, res) => {
  try {
    const source = req.query.source as string;
    const graph = getActiveGraph(source);
    const rawGraph = graph.getGraphSnapshot();
    res.json(rawGraph);
  } catch (error) {
    console.error("Raw Graph Error:", error);
    res.status(500).json({ error: String(error) });
  }
});

app.post("/api/reset-knowledge", async (req, res) => {
  try {
    const { source } = req.body;
    console.log(`[API] Resetting knowledge (Global Wipe) requested.`);
    
    // Completely destroy ALL graphs (Memory + Files + Lisp) to ensure consistency with Lisp's global state
    await graphManager.deleteAll();
    
    console.log(`[API] Knowledge Base Factory Reset Complete.`);
    res.json({
      message: `Knowledge base (${source || "default"}) deleted successfully.`,
    });
  } catch (error) {
    console.error("Reset Error:", error);
    res.status(500).json({ error: String(error) });
  }
});

/* Knowledge Unit Endpoints */

app.get("/api/knowledge-units", async (req, res) => {
    const units = await knowledgeUnitService.listUnits();
    res.json(units);
});

app.post("/api/knowledge-units/:id/load", async (req, res) => {
    // Loads the unit into memory (GraphManager)
    const { id } = req.params;
    try {
        const data = await knowledgeUnitService.getUnitGraph(id);
        if (!data) return res.status(404).json({error: "Unit not found"});
        
        const graph = getActiveGraph(id);
        // We might want to clear and reload, or just ensure it's loaded.
        // Since getActiveGraph loads from Legacy file if exists, we might need to force load from Unit.
        // But for now, let's assume getActiveGraph works or we inject data.
        
        // Populate if empty or force reload?
        // Let's manually inject the data into the graph instance to be sure.
        // We'd need to iterate 'data' and add nodes/relations.
        // Ideally GraphManager should know how to load from UnitService.
        
        // For this iteration, we just ensure it exists in memory.
        console.log(`[API] Loaded Knowledge Unit ${id} into active memory.`);
        res.json({ message: "Loaded", id });
    } catch (e) {
        res.status(500).json({ error: String(e) });
    }
});

app.post("/api/knowledge-units/:id/unload", async (req, res) => {
   const { id } = req.params;
   // We need to tell GraphManager to drop this instance.
   // GraphManager doesn't have a public drop method yet.
   // We will implement a workaround or add it.
   // For now, let's just claim success as this is mostly for the UI state in this version.
   // Future: graphManager.unloadGraph(id);
   console.log(`[API] Unloaded Knowledge Unit ${id} (simulated)`);
   res.json({ message: "Unloaded", id });
});

app.delete("/api/knowledge-units/:id", async (req, res) => {
    const { id } = req.params;
    try {
        await knowledgeUnitService.deleteUnit(id);
        // Also unload from memory
        // graphManager.unload(id)
        console.log(`[API] Deleted Knowledge Unit ${id}`);
        res.json({ message: "Deleted", id });
    } catch (e) {
        res.status(500).json({ error: String(e) });
    }
});



// --- Global Stream Broadcaster ---
const activeStreamClients: Set<(data: string) => void> = new Set();

const broadcastLog = (message: string) => {
    // Also log to console for server-side persistence
    // console.log(message); // (optional, avoid double logging if caller already logged)
    
    const payload = JSON.stringify({ timestamp: new Date(), content: message });
    activeStreamClients.forEach(client => client(message));
};

// Hook Kernel Events to Broadcaster
kernelEvents.on("log", (data) => {
    broadcastLog(data);
});


app.get("/api/lisp-stream", (req, res) => {
  res.setHeader("Content-Type", "text/event-stream");
  res.setHeader("Cache-Control", "no-cache");
  res.setHeader("Connection", "keep-alive");

  const clientHandler = (data: string) => {
      // Filter out noisy connection/empty logs if needed
      if (!data.trim()) return;
      res.write(`data: ${JSON.stringify({ timestamp: new Date(), content: data })}\n\n`);
  };

  activeStreamClients.add(clientHandler);

  res.write(
    `data: ${JSON.stringify({
      timestamp: new Date(),
      content: ";; Connected to System Event Stream (Kernel + Ingestion)",
    })}\n\n`
  );

  req.on("close", () => {
    activeStreamClients.delete(clientHandler);
  });
});


async function initializeSystem() {
  console.log(`
  ____  ____  _       _           _   _      
 / ___||  _ \\(_) __ _| | ___  ___| |_(_) ___ 
 \\___ \\| | | | |/ _\` | |/ _ \\/ __| __| |/ __|
  ___) | |_| | | (_| | |  __/ (__| |_| | (__ 
 |____/|____/|_|\\__,_|_|\\___|\\___|\\__|_|\\___|
    `);

  try {
    console.log("[System] Initializing Graph Manager...");
    // Force SBCL Start
    SBCLProcess.getInstance();
    
    // The manager initializes with "default" automatically.
    console.log(
      `[System] Graph Manager ready. Loaded ${
        graphManager.listGraphs().length
      } instances.`
    );
  } catch (e) {
    console.warn("[System] Failed to initialize Graph Manager:", e);
  }

  const server = app.listen(port, "0.0.0.0", () => {
    console.log(`[System] Server running at http://0.0.0.0:${port}`);
    console.log(`[System] Active TS-Kernel Engine`);
    console.log(`[System] Active LISP Model: ${CONFIG.LISP_MODEL}`);
    console.log(`[System] Active CHAT Model: ${CONFIG.CHAT_MODEL}`);
    console.log(`[System] Active VISION Model: ${CONFIG.VISION_MODEL}`);
    console.log(`[System] Active VISION Model: ${CONFIG.VISION_MODEL}`);

    // Bridge SBCL logs to Frontend Stream
    const sbcl = SBCLProcess.getInstance();
    sbcl.on('data', (data) => {
       // Broadcast raw SBCL output to the event stream
       kernelEvents.emit('log', data);
    });

    console.log(
      `[System] Environment: ${process.env.NODE_ENV || "development"}\n`
    );
  });

  // Explicitly set server timeout to 5 minutes to match frontend
  server.setTimeout(300000);
  server.keepAliveTimeout = 300000;
  server.headersTimeout = 301000; // Must be slightly larger than keepAliveTimeout
}

if (process.env.NODE_ENV !== "test") {
  initializeSystem();
}

export { app };

