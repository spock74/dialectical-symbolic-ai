/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { genkit } from 'genkit';
import { ollama } from 'genkitx-ollama';
// import { dotprompt } from '@genkit-ai/dotprompt';

const ai = genkit({
  plugins: [
    ollama({
      models: [{ name: 'gemma3:4b' }],
      serverAddress: 'http://127.0.0.1:11434',
    }),
  ],
  model: 'ollama/gemma3:4b',
  promptDir: './prompts'
});

console.log("AI Configured:", ai);
