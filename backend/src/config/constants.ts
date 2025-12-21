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
  OLLAMA_MODEL_NAME: getEnv('OLLAMA_MODEL_NAME'),
  
  // Server Configuration
  PORT: parseInt(getEnv('PORT'), 10), // Strict check for PORT too
};
