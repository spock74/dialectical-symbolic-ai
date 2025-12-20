import express from 'express';
import cors from 'cors';
import multer from 'multer';
import { reflectiveLoop } from './flows/reflective-loop';
import { extractKnowledge } from './flows/extraction/knowledge-flow';
import { extractKnowledgeMultimodal } from './flows/extraction/multimodal-flow';
import { pdfService } from './services/pdf-service';

const app = express();
const port = 3000;
const upload = multer({ storage: multer.memoryStorage() });

app.use(cors());
app.use(express.json());

app.post('/api/chat', async (req, res) => {
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
app.post('/api/extract', async (req, res) => {
  try {
    const { text } = req.body;
    const result = await extractKnowledge({ text });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: error instanceof Error ? error.message : "Unknown error" });
  }
});

// PDF Text Extraction + Lisp Verification
app.post('/api/extract-from-pdf', upload.single('file'), async (req: any, res: any) => {
  try {
    if (!req.file) {
      return res.status(400).json({ error: "No file uploaded" });
    }
    const text = await pdfService.parsePdf(req.file.buffer);
    const result = await extractKnowledge({ text });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: error instanceof Error ? error.message : "Unknown error" });
  }
});

// Multimodal PDF Extraction (Visual)
app.post('/api/extract-multimodal', upload.single('file'), async (req: any, res: any) => {
  try {
    if (!req.file) {
      return res.status(400).json({ error: "No file uploaded" });
    }
    // Convert buffer to base64
    const pdfBase64 = req.file.buffer.toString('base64');
    const result = await extractKnowledgeMultimodal({ pdfBase64 });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: error instanceof Error ? error.message : "Unknown error" });
  }
});

// Simple Transcription (Debug/Fallback)
import { extractSimpleTranscription } from './flows/extraction/simple-flow';
app.post('/api/transcribe-simple', upload.single('file'), async (req: any, res: any) => {
  try {
    if (!req.file) {
      return res.status(400).json({ error: "No file uploaded" });
    }
    const pdfBase64 = req.file.buffer.toString('base64');
    const result = await extractSimpleTranscription({ pdfBase64 });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: error instanceof Error ? error.message : "Unknown error" });
  }
});

// Markdown Extraction
import { extractMarkdown } from './flows/extraction/markdown-flow';
app.post('/api/extract-markdown', upload.single('file'), async (req: any, res: any) => {
  try {
    if (!req.file) {
      return res.status(400).json({ error: "No file uploaded" });
    }
    // Read buffer as UTF-8 textual content
    const text = req.file.buffer.toString('utf-8');
    const result = await extractMarkdown({ 
      text,
      filename: req.file.originalname
    });
    res.json(result);
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: error instanceof Error ? error.message : "Unknown error" });
  }
});

// Live Lisp REPL Stream (SSE)
import { lisp } from './flows/reflective-loop';

app.get('/api/lisp-stream', (req, res) => {
  res.setHeader('Content-Type', 'text/event-stream');
  res.setHeader('Cache-Control', 'no-cache');
  res.setHeader('Connection', 'keep-alive');

  const onLog = (data: string) => {
    // Escape newlines for SSE data protocol
    const sanitized = data.replace(/\n/g, '\\n');
    res.write(`data: ${JSON.stringify({ timestamp: new Date(), content: data })}\n\n`);
  };

  lisp.on('log', onLog);

  // Send initial connection message
  res.write(`data: ${JSON.stringify({ timestamp: new Date(), content: ";; Connected to NeuroLisp REPL Stream" })}\n\n`);

  req.on('close', () => {
    lisp.off('log', onLog);
  });
});

app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);
});
