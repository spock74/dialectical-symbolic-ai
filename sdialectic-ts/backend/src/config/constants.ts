/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import dotenv from 'dotenv';
import path from 'path';

// Load .env.local from project root or backend root
dotenv.config({ path: path.resolve(process.cwd(), '.env.local') });

/**
 * Retrieves an environment variable or throws a critical error if missing.
 * STRICT MODE: No fallbacks allowed.
 */
function getEnv(key: string): string {
  const value = process.env[key];
  if (value === undefined || value === '') {
    throw new Error(`[CONFIGURATION ERROR] Missing required environment variable: ${key}`);
  }
  return value;
}

export const CONFIG = {
  // AI Model Configuration
  USE_LOCAL_MODELS: process.env.USE_LOCAL_MODELS === "true",
  GEMINI_API_KEY: getEnv("GEMINI_API_KEY"),

  // Local Model Names
  OLLAMA_LISP_MODEL_NAME: getEnv("OLLAMA_LISP_MODEL_NAME"),
  OLLAMA_VISION_MODEL_NAME: getEnv("OLLAMA_VISION_MODEL_NAME"),
  OLLAMA_CHAT_MODEL_NAME: getEnv("OLLAMA_CHAT_MODEL_NAME"),
  
  // Embedding Model
  OLLAMA_EMBED_MODEL_NAME: "nomic-embed-text:latest",

  // Gemini Model Names
  GEMINI_LISP_MODEL_NAME: getEnv("GEMINI_LISP_MODEL_NAME"),
  GEMINI_VISION_MODEL_NAME: getEnv("GEMINI_VISION_MODEL_NAME"),
  GEMINI_CHAT_MODEL_NAME: getEnv("GEMINI_CHAT_MODEL_NAME"),

  OLLAMA_UNLOAD_DELAY_SECONDS: parseInt(
    getEnv("OLLAMA_UNLOAD_DELAY_SECONDS"),
    10
  ),

  // Server Configuration
  PORT: parseInt(getEnv("PORT"), 10),

  // Dynamic Model References
  get LISP_MODEL() {
    return this.USE_LOCAL_MODELS
      ? `ollama/${this.OLLAMA_LISP_MODEL_NAME}`
      : `googleai/${this.GEMINI_LISP_MODEL_NAME}`;
  },
  get VISION_MODEL() {
    return this.USE_LOCAL_MODELS
      ? `ollama/${this.OLLAMA_VISION_MODEL_NAME}`
      : `googleai/${this.GEMINI_VISION_MODEL_NAME}`;
  },
  // Knowledge Domain
  DOMAIN_SPECIFIC_KNOWLEDGE: process.env.DOMAIN_SPECIFIC_KNOWLEDGE || "prompts",

  get CHAT_MODEL() {
    return this.USE_LOCAL_MODELS
      ? `ollama/${this.OLLAMA_CHAT_MODEL_NAME}`
      : `googleai/${this.GEMINI_CHAT_MODEL_NAME}`;
  },
};
