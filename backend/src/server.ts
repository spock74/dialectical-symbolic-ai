import express from 'express';
import cors from 'cors';
import multer from 'multer';
import { reflectiveLoop } from './flows/reflective-loop';
import { extractKnowledge } from './flows/extraction/knowledge-flow';
import { extractKnowledgeMultimodal } from './flows/extraction/multimodal-flow';
import { pdfService } from './services/pdf-service';

import { CONFIG } from "./config";

const app = express();
const port = CONFIG.PORT;
const upload = multer({ dest: "uploads/" }); // Use disk storage for memory safety

app.use(cors());
app.use((req, res, next) => {
  res.setHeader("X-AI-Model", CONFIG.OLLAMA_MODEL_NAME);
  next();
});
app.use(express.json({ limit: "50mb" })); // Increase JSON body limit for Base64 fallbacks

app.post("/api/chat", async (req, res) => {
  try {
    const { prompt, history } = req.body;
    const result = await reflectiveLoop({ prompt, history });
    res.json({ text: result });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: String(error) });
  }
});

// Basic text extraction
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

// Helper to cleanup uploaded file
const cleanupFile = async (file: Express.Multer.File | undefined) => {
  if (file && file.path) {
    try {
      await import("fs/promises").then((fs) => fs.unlink(file.path));
    } catch (e) {
      console.error(`Failed to cleanup file ${file.path}:`, e);
    }
  }
};

// PDF Text Extraction + Lisp Verification
app.post(
  "/api/extract-from-pdf",
  upload.single("file"),
  async (req: any, res: any) => {
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      // Read from disk buffer for text extraction
      const fs = await import("fs/promises");
      const buffer = await fs.readFile(req.file.path);

      const text = await pdfService.parsePdf(buffer);
      const result = await extractKnowledge({ text });
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

// Multimodal PDF Extraction (Visual)
app.post(
  "/api/extract-multimodal",
  upload.single("file"),
  async (req: any, res: any) => {
    try {
      if (!req.file) {
        return res.status(400).json({ error: "No file uploaded" });
      }
      // Pass FILE PATH to multimodal flow to save memory (Zero-Copy)
      const result = await extractKnowledgeMultimodal({
        filePath: req.file.path,
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

// Simple Transcription (Debug/Fallback)
import { extractSimpleTranscription } from "./flows/extraction/simple-flow";
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

// Markdown Extraction
import { extractMarkdown } from "./flows/extraction/markdown-flow";
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

// Graph Data Endpoint
import { transformMemoriesToGraph } from "./services/graph-service";

app.get("/api/graph-data", async (req, res) => {
  try {
    const rawMemories = await lisp.getMemories();
    // Transform into Node/Edge format for ReactFlow
    const { nodes, edges } = transformMemoriesToGraph(rawMemories);

    // Create edges? ideally Lisp should return relationships.
    // For now, implicit central node or just disconnected.
    // Let's verify we just get nodes first.

    res.json({ nodes, edges });
  } catch (error) {
    console.error("Graph Error:", error);
    res.status(500).json({ error: String(error) });
  }
});

// Live Lisp REPL Stream (SSE)
import { lisp } from "./flows/reflective-loop";

app.get("/api/lisp-stream", (req, res) => {
  res.setHeader("Content-Type", "text/event-stream");
  res.setHeader("Cache-Control", "no-cache");
  res.setHeader("Connection", "keep-alive");

  let lastLogTime = 0;
  const onLog = (data: string) => {
    const now = Date.now();
    // Throttle high-frequency logs (e.g. > 20 messages/sec) to save bandwidth
    // Unless it's a critical error or connection message
    if (
      now - lastLogTime < 50 &&
      !data.includes("ERR") &&
      !data.includes("Connected")
    ) {
      return;
    }
    lastLogTime = now;

    // Escape newlines for SSE data protocol
    const sanitized = data.replace(/\n/g, "\\n");
    res.write(
      `data: ${JSON.stringify({ timestamp: new Date(), content: data })}\n\n`
    );
  };

  lisp.on("log", onLog);

  // Send initial connection message
  res.write(
    `data: ${JSON.stringify({
      timestamp: new Date(),
      content: ";; Connected to SDialectic REPL Stream",
    })}\n\n`
  );

  req.on("close", () => {
    lisp.off("log", onLog);
  });
});

app.listen(port, () => {
  console.log(`
  ____  ____  _       _           _   _      
 / ___||  _ \\(_) __ _| | ___  ___| |_(_) ___ 
 \\___ \\| | | | |/ _\` | |/ _ \\/ __| __| |/ __|
  ___) | |_| | | (_| | |  __/ (__| |_| | (__ 
 |____/|____/|_|\\__,_|_|\\___|\\___|\\__|_|\\___|
    `);
  console.log(`[System] Server running at http://localhost:${port}`);
  console.log(`[System] Active AI Model: ${CONFIG.OLLAMA_MODEL_NAME}`);
  console.log(
    `[System] Environment: ${process.env.NODE_ENV || "development"}\n`
  );
});
