/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { CONFIG } from '../config/constants';

/**
 * Schedules a model unload after a specified delay.
 * This is non-blocking (fire and forget) to ensure API response time isn't affected.
 * @param modelName The full name of the model to unload (e.g. 'gemma3:4b')
 * @param delaySeconds How many seconds to wait before unloading
 * @param fetchFn Dependency injection for testing (defaults to global fetch)
 */
export function scheduleModelUnload(
  modelName: string, 
  delaySeconds: number = CONFIG.OLLAMA_UNLOAD_DELAY_SECONDS,
  fetchFn: typeof fetch = fetch
): void {
  const delayMs = delaySeconds * 1000;
  
  if (delayMs <= 0) {
    void executeUnload(modelName, fetchFn);
    return;
  }

  console.log(`[Resource Manager] Scheduling unload for ${modelName} in ${delaySeconds}s...`);
  
  setTimeout(() => {
    void executeUnload(modelName, fetchFn);
  }, delayMs);
}

async function executeUnload(modelName: string, innerFetch: typeof fetch) {
  try {
    console.log(`[Resource Manager] Unloading now: ${modelName}`);

    // Only attempt to unload local Ollama models.
    // If it's a googleai/ model, we can't unload it via local fetch.
    if (!modelName.includes("ollama") && !CONFIG.USE_LOCAL_MODELS) {
      console.log(
        `[Resource Manager] Skipping unload for non-Ollama model: ${modelName}`
      );
      return;
    }

    const cleanName = modelName.replace(/^ollama\//, "");

    await innerFetch("http://127.0.0.1:11434/api/generate", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: cleanName, keep_alive: 0 }),
    });
    console.log(`[Resource Manager] Success: ${modelName} unloaded.`);
  } catch (e) {
    console.warn(`[Resource Manager] Failed to unload ${modelName}:`, e);
  }
}
