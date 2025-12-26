import express from 'express';
import cors from 'cors';
import multer from 'multer';
import { reflectiveLoop } from './flows/reflective-loop';
import { extractKnowledge } from './flows/extraction/knowledge-flow';
import { extractKnowledgeMultimodal } from './flows/extraction/multimodal-flow';

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
const upload = multer({ dest: "uploads/" });

app.use(cors());
app.use((req, res, next) => {
  res.setHeader("X-AI-Model", CONFIG.LISP_MODEL);
  next();
});
app.use(express.json({ limit: "50mb" }));

// Middleware to register rollback on client abortion
const registerRollback = (req: any, res: any, next: any) => {
  req.isAborted = false;
  const source = req.body?.source || req.query?.source;
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
    let result = await reflectiveLoop({
      prompt,
      history,
      useMemory,
      bypassSDialect,
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
  registerRollback,
  upload.single("file"),
  async (req: any, res: any) => {
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

      res.json(result);
      const graph = getActiveGraph(req.file.originalname);
      graph.saveState(`data/graphs/${req.file.originalname}.json`);
    } catch (error) {
      if (req.isAborted) return;
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
  "/api/extract-multimodal",
  registerRollback,
  upload.single("file"),
  async (req: any, res: any) => {
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      const result = await extractKnowledgeMultimodal({
        filePath: req.file.path,
      });
      if (req.isAborted) return;
      res.json(result);
      const graph = getActiveGraph(req.file.originalname);
      graph.saveState(`data/graphs/${req.file.originalname}.json`);
    } catch (error) {
      if (req.isAborted) return;
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
      commitKnowledgeToGraph(result, req.file.originalname);
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

    // 2. Get Lisp-side data
    let lispNodes: any[] = [];
    let lispEdges: any[] = [];
    try {
      const lispRaw = await SBCLProcess.getInstance().getLispGraphData();
      if (lispRaw && (lispRaw.nodes || lispRaw.edges)) {
         const transformed = transformMemoriesToGraph(lispRaw);
         lispNodes = transformed.nodes || [];
         lispEdges = transformed.edges || [];
      }
    } catch (e) {
      console.error("[API] Failed to integrate Lisp graph data:", e);
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
    const edgeKeys = new Set(jsEdges.map(e => `${e.source}-${e.target}-${e.label}`));

    lispEdges.forEach(le => {
      if (le && le.source && le.target) {
        const key = `${le.source}-${le.target}-${le.label || ''}`;
        if (!edgeKeys.has(key)) {
          allEdges.push(le);
          edgeKeys.add(key);
        }
      }
    });

    console.log(`[API] Graph State: JS(${jsNodes.length}n, ${jsEdges.length}e), Lisp(${lispNodes.length}n, ${lispEdges.length}e) -> Total(${allNodes.length}n, ${allEdges.length}e)`);

    const response = {
      nodes: allNodes,
      edges: allEdges,
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
    const graph = getActiveGraph(source);
    graph.clear();
    graph.saveState(`data/graphs/${source || "default"}.json`);
    res.json({
      message: `Knowledge base (${source || "default"}) reset successfully.`,
    });
  } catch (error) {
    console.error("Reset Error:", error);
    res.status(500).json({ error: String(error) });
  }
});

app.get("/api/lisp-stream", (req, res) => {
  res.setHeader("Content-Type", "text/event-stream");
  res.setHeader("Cache-Control", "no-cache");
  res.setHeader("Connection", "keep-alive");

  let lastLogTime = 0;
  const onLog = (data: string) => {
    const now = Date.now();
    if (
      now - lastLogTime < 50 &&
      !data.includes("ERR") &&
      !data.includes("Connected")
    ) {
      return;
    }
    lastLogTime = now;

    res.write(
      `data: ${JSON.stringify({ timestamp: new Date(), content: data })}\n\n`
    );
  };

  kernelEvents.on("log", onLog);

  res.write(
    `data: ${JSON.stringify({
      timestamp: new Date(),
      content: ";; Connected to TS-Symbolic-Kernel Stream",
    })}\n\n`
  );

  req.on("close", () => {
    kernelEvents.off("log", onLog);
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
    // The manager initializes with "default" automatically.
    console.log(
      `[System] Graph Manager ready. Loaded ${
        graphManager.listGraphs().length
      } instances.`
    );
  } catch (e) {
    console.warn("[System] Failed to initialize Graph Manager:", e);
  }

  app.listen(port, () => {
    console.log(`[System] Server running at http://localhost:${port}`);
    console.log(`[System] Active TS-Kernel Engine`);
    console.log(`[System] Active LISP Model: ${CONFIG.LISP_MODEL}`);
    console.log(`[System] Active CHAT Model: ${CONFIG.CHAT_MODEL}`);
    console.log(`[System] Active VISION Model: ${CONFIG.VISION_MODEL}`);
    console.log(
      `[System] Environment: ${process.env.NODE_ENV || "development"}\n`
    );
  });
}

if (process.env.NODE_ENV !== "test") {
  initializeSystem();
}

export { app };

