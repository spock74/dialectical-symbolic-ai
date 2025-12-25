import type { KnowledgeBase } from './types';

export const API_BASE_URL = 'http://localhost:3000/api';
const API_BASE = API_BASE_URL; // Keep local ref for internal use if needed, or just replace usage.

export async function uploadPdf(file: File): Promise<KnowledgeBase> {
  const formData = new FormData();
  formData.append('file', file);

  const response = await fetch(`${API_BASE}/extract-from-pdf`, {
    method: 'POST',
    body: formData,
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Upload failed: ${errorText}`);
  }

  return response.json();
}

export async function uploadPdfMultimodal(file: File): Promise<KnowledgeBase> {
  const formData = new FormData();
  formData.append('file', file);

  const response = await fetch(`${API_BASE}/extract-multimodal`, {
    method: 'POST',
    body: formData,
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Multimodal Upload failed: ${errorText}`);
  }

  return response.json();
}

/**
 * Sends a chat message to the backend reflective loop.
 */
export async function chat(
  prompt: string,
  history: any[] = [],
  useMemory: boolean = true,
  bypassSDialect: boolean = false,
  source?: string
): Promise<{ text: string; reasoningLogs: string }> {
  const response = await fetch(`${API_BASE}/chat`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      prompt,
      history,
      useMemory,
      bypassSDialect,
      source,
    }),
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Chat request failed: ${errorText}`);
  }

  return response.json();
}

/**
 * Uploads a text/markdown file for knowledge extraction.
 */
export async function extractMarkdown(file: File): Promise<KnowledgeBase> {
  const formData = new FormData();
  formData.append("file", file);

  const response = await fetch(`${API_BASE}/extract-markdown`, {
    method: "POST",
    body: formData,
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Markdown extraction failed: ${errorText}`);
  }

  return response.json();
}

/**
 * Fetches the knowledge graph data (nodes) from the backend/Lisp.
 */
export async function fetchGraph(
  source?: string
): Promise<{ nodes: any[]; edges: any[] }> {
  const url = source
    ? `${API_BASE}/graph-data?source=${encodeURIComponent(source)}`
    : `${API_BASE}/graph-data`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error("Failed to fetch graph data");
  }
  return response.json();
}

/**
 * Triggers a full reset of the Lisp knowledge base.
 */
export async function resetKnowledge(
  source?: string
): Promise<{ message: string }> {
  const response = await fetch(`${API_BASE}/reset-knowledge`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ source }),
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Reset failed: ${errorText}`);
  }

  return response.json();
}
export async function fetchGraphRaw(source?: string): Promise<any> {
  const url = source
    ? `${API_BASE}/graph-raw?source=${encodeURIComponent(source)}`
    : `${API_BASE}/graph-raw`;
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error("Failed to fetch raw graph data");
  }
  return response.json();
}
