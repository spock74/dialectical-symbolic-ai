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
  promptDir: "prompts",
});
