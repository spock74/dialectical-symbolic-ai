import type { KnowledgeBase } from './types';

const API_BASE = 'http://localhost:3000/api';

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
export async function chat(prompt: string, history: any[] = []): Promise<{ text: string }> {
  const response = await fetch(`${API_BASE}/chat`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ prompt, history }),
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
  formData.append('file', file);

  const response = await fetch(`${API_BASE}/extract-markdown`, {
      method: 'POST',
      body: formData,
  });

  if (!response.ok) {
      const errorText = await response.text();
      throw new Error(`Markdown extraction failed: ${errorText}`);
  }

  return response.json();
}
