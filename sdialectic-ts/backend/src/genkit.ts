/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { genkit } from 'genkit';
import { ollama } from 'genkitx-ollama';
import { googleAI } from "@genkit-ai/google-genai";
import { CONFIG } from "./config";

const plugins = [];

const activeModels = [
  CONFIG.LISP_MODEL,
  CONFIG.VISION_MODEL,
  CONFIG.CHAT_MODEL,
].filter(Boolean);

const ollamaModels = activeModels
  .filter((m) => m.startsWith("ollama/"))
  .map((m) => m.replace("ollama/", ""));

const usesGoogleAI = activeModels.some((m) => m.startsWith("googleai/"));

// Initialize Plugins dynamically
if (ollamaModels.length > 0) {
  console.log(`[Genkit] Registering Ollama Models: ${ollamaModels.join(", ")}`);
  plugins.push(
    ollama({
      models: ollamaModels.map((name) => ({ name })),
      serverAddress: "http://127.0.0.1:11434",
      // @ts-ignore: genkitx-ollama type definition is missing this property but undici supports it
      requestTimeout: 600000, // 10 minutes for slower local multimodal processing
    })
  );
}

if (usesGoogleAI) {
  console.log(`[Genkit] Registering Google AI Plugin`);
  plugins.push(
    googleAI({
      apiKey: CONFIG.GEMINI_API_KEY,
    })
  );
}

export const ai = genkit({
  plugins,
  model: CONFIG.LISP_MODEL,
  promptDir: CONFIG.DOMAIN_SPECIFIC_KNOWLEDGE,
});

console.log(`[Genkit] Initialized with Prompt Directory: '${CONFIG.DOMAIN_SPECIFIC_KNOWLEDGE}'`);
