import { SbclProcess } from './src/lisp/sbcl-process';

async function test() {
  console.log("Starting SBCL...");
  // Cast to any to avoid IDE TypeScript issues with EventEmitter inheritance
  const proc: any = new SbclProcess();
  
  // Listen to logs to see bootstrap output
  proc.on('log', (msg: string) => {
      console.log(`[SBCL] ${msg.trim()}`);
  });
  
  // Wait a bit for startup (simple check)
  await new Promise(r => setTimeout(r, 2000));

  try {
    console.log("Testing (lisp (+ 1 1))...");
    const result = await proc.eval('(lisp (+ 1 1))');
    console.log("Result:", result);
    if (result.includes('2')) {
        console.log("SUCCESS: Macro works.");
    } else {
        console.log("FAILURE: Unexpected result.");
    }
  } catch (e) {
    console.error("ERROR:", e);
  } finally {
    proc.kill();
  }
}

test();
