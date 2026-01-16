/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 */

import { SBCLProcess } from './src/services/sbcl-process';

async function test() {
  console.log("Starting SBCL...");
  // Cast to any to avoid IDE TypeScript issues with EventEmitter inheritance
  const process = SBCLProcess.getInstance();
  
  // Listen to logs to see bootstrap output
  process.on('log', (msg: string) => {
      console.log(`[SBCL] ${msg.trim()}`);
  });
  
  // Wait a bit for startup (simple check)
  await new Promise(r => setTimeout(r, 2000));

  try {
    console.log("Testing (lisp (+ 1 1))...");
    const result = await process.evaluate('(lisp (+ 1 1))');
    console.log("Result:", result);
    if (result.includes('2')) {
        console.log("SUCCESS: Macro works.");
    } else {
        console.log("FAILURE: Unexpected result.");
    }
  } catch (e) {
    console.error("ERROR:", e);
  } finally {
    // Force exit since SBCLProcess doesn't expose a public kill method
    // and we want to ensure the test script terminates.
    setTimeout(() => Object.getPrototypeOf(process).constructor.instance = null, 100); // Hack to clear singleton if needed, but really just exit.
    // (process as any).process?.kill(); // Access private property if really needed
    global.process.exit(0); 
  }
}

test();
