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
  GEMINI_API_KEY: getEnv("GEMINI_API_KEY"),

  // Consolidated Model Definitions (Provider/ModelName)
  LISP_MODEL: getEnv("LISP_MODEL"),
  VISION_MODEL: getEnv("VISION_MODEL"),
  CHAT_MODEL: getEnv("CHAT_MODEL"),
  
  // Embedding Model (Static for now, can be elevated to env if needed)
  OLLAMA_EMBED_MODEL_NAME: "nomic-embed-text:latest",

  OLLAMA_UNLOAD_DELAY_SECONDS: parseInt(
    getEnv("OLLAMA_UNLOAD_DELAY_SECONDS"),
    10
  ),

  // Server Configuration
  PORT: parseInt(getEnv("PORT"), 10),

  // Knowledge Domain
  DOMAIN_SPECIFIC_KNOWLEDGE: process.env.DOMAIN_SPECIFIC_KNOWLEDGE || "prompts",

  // Helpers for identifying model type
  get IS_LOCAL_LISP() { return this.LISP_MODEL.startsWith("ollama/"); },
  get IS_LOCAL_VISION() { return this.VISION_MODEL.startsWith("ollama/"); },
  get IS_LOCAL_CHAT() { return this.CHAT_MODEL.startsWith("ollama/"); },

  // Helpers for raw model names (stripping provider prefix)
  get RAW_LISP_MODEL_NAME() { return this.LISP_MODEL.replace(/^(ollama|googleai)\//, ""); },
  get RAW_VISION_MODEL_NAME() { return this.VISION_MODEL.replace(/^(ollama|googleai)\//, ""); },
  get RAW_CHAT_MODEL_NAME() { return this.CHAT_MODEL.replace(/^(ollama|googleai)\//, ""); },
};
