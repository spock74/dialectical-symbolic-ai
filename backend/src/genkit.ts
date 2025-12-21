import { genkit } from 'genkit';
import { ollama } from 'genkitx-ollama';
import { CONFIG } from './config';

export const ai = genkit({
  plugins: [
    ollama({
      models: [
        { name: CONFIG.OLLAMA_MODEL_NAME },
        { name: CONFIG.OLLAMA_VISION_MODEL_NAME },
      ],
      serverAddress: "http://127.0.0.1:11434",
    }),
  ],
  model: `ollama/${CONFIG.OLLAMA_MODEL_NAME}`,
  promptDir: "prompts",
});
