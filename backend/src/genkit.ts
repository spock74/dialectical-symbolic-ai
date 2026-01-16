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

if (CONFIG.USE_LOCAL_MODELS) {
  plugins.push(
    ollama({
      models: [
        { name: CONFIG.OLLAMA_LISP_MODEL_NAME },
        { name: CONFIG.OLLAMA_VISION_MODEL_NAME },
        { name: CONFIG.OLLAMA_CHAT_MODEL_NAME },
      ],
      serverAddress: "http://127.0.0.1:11434",
      // @ts-ignore: genkitx-ollama type definition is missing this property but undici supports it
      requestTimeout: 600000, // 10 minutes for slower local multimodal processing
    })
  );
} else {
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
