import { spawn, ChildProcess } from 'child_process';
import crypto from 'crypto';
import { EventEmitter } from 'events';

export class SbclProcess extends EventEmitter {
  private process: ChildProcess | null = null;
  private buffer: string = "";
  private currentResolve: ((result: string) => void) | null = null;
  private isReady: boolean = false;
  private readonly SENTINEL = "|||GEMINI_SENTINEL|||";

  constructor() {
    super();
    this.start();

    // Ensure child process is killed when Node exits
    process.on("exit", () => this.kill());
    process.on("SIGINT", () => {
      this.kill();
      process.exit();
    });
    process.on("SIGTERM", () => {
      this.kill();
      process.exit();
    });
  }

  private start() {
    // Limit heap to 1GB to prevent OOM on user machine
    // Load bootstrap file on startup
    const bootstrapFile = "lisp/bootstrap.lisp";
    this.process = spawn("sbcl", [
      "--dynamic-space-size",
      "1024",
      "--noinform",
      "--interactive",
    ]);

    // Explicitly command SBCL to load the bootstrap file to ensure it happens
    if (this.process.stdin) {
      // Escape backslashes for Windows, though this is Mac.
      // Lisp string needs double backslashes.
      const lispPath = bootstrapFile.replace(/\\/g, "/");
      const loadCommand = `(load "${lispPath}")\n`;
      console.log(`[SBCL] Sending usage command: ${loadCommand}`);
      this.process.stdin.write(loadCommand);
    }

    this.process.stdout?.on("data", (data) => {
      const chunk = data.toString();
      this.emit("log", chunk); // Emit log event
      this.buffer += chunk;
      this.checkBuffer();
    });

    this.process.stderr?.on("data", (data) => {
      const chunk = data.toString();
      this.emit("log", `ERR: ${chunk}`); // Emit error log
      console.error(`SBCL Error: ${data}`);
    });

    this.process.on("close", (code) => {
      console.log(`SBCL process exited with code ${code}`);
      this.process = null;
      this.isReady = false;
    });

    this.isReady = true;
  }

  private checkBuffer() {
    // Detect SBCL Debugger Prompt (e.g., "0] ", "1] ")
    // This indicates an error that wasn't caught by normal error handling
    // EXCLUDE the standard toplevel prompt "*" or it will loop infinitely!
    const debuggerRegex = /^([0-9]+\])\s*$/m;
    
    if (this.buffer.includes(this.SENTINEL)) {
      const parts = this.buffer.split(this.SENTINEL);
      const result = parts[0].trim();
      this.buffer = parts.slice(1).join(this.SENTINEL);

      if (this.currentResolve) {
        const resolve = this.currentResolve;
        this.currentResolve = null;
        this.clearTimeout(); // Clear timeout on success
        resolve(result);
      }
    } else if (debuggerRegex.test(this.buffer)) {
        console.error("[SBCL] Debugger detected! Aborting to toplevel...");
        
        // Log the error buffer for debugging
        this.emit("log", `\n[Use 'abort' to exit debugger]\nBuffer capture:\n${this.buffer}\n`);

        // Fail the current request
        if (this.currentReject) {
            const reject = this.currentReject;
            const resolve = this.currentResolve; // Capture reference
            this.currentReject = null;
            this.currentResolve = null;
            this.clearTimeout();
            
            // Reject with the buffer content as error message
            reject(new Error(`SBCL Debugger Invoked. Buffer:\n${this.buffer}`));
            
            // IMPORTANT: We must NOT leave the promise hanging.
            // If we resolved, we wouldn't be here.
        }

        // Try to recover process: send abort to return to toplevel
        if (this.process?.stdin) {
            this.process.stdin.write("(abort)\n");
            // Clear buffer to prevent re-triggering immediately
            this.buffer = ""; 
        }
        
        // Ensure processing continues for subsequent items
        this.isProcessing = false;
        setTimeout(() => this.processQueue(), 100);
    }
  }

  private queue: Array<{ code: string; resolve: (value: string) => void; reject: (reason?: any) => void }> = [];
  private isProcessing = false;
  private currentReject: ((reason?: any) => void) | null = null;
  private timeoutId: NodeJS.Timeout | null = null;
  private readonly TIMEOUT_MS = 30000; // 30s timeout

  async eval(code: string): Promise<string> {
    if (!this.process || !this.isReady) {
      throw new Error("SBCL process is not running");
    }
    
    // Wrap in promise and push to queue
    return new Promise((resolve, reject) => {
      this.queue.push({ code, resolve, reject });
      this.processQueue();
    });
  }

  private clearTimeout() {
      if (this.timeoutId) {
          clearTimeout(this.timeoutId);
          this.timeoutId = null;
      }
  }

  private async processQueue() {
    if (this.isProcessing || this.queue.length === 0) return;

    this.isProcessing = true;
    const item = this.queue.shift();
    if (!item) {
      this.isProcessing = false;
      return;
    }

    try {
      this.currentResolve = item.resolve;
      this.currentReject = item.reject; // Store reject to fail on debugger/timeout

      // Echo the command
      this.emit("log", `> ${item.code}\n`);

      if (this.process?.stdin) {
        this.process.stdin.write(`${item.code}\n`);
        this.process.stdin.write(`(format t "${this.SENTINEL}~%")\n`);
      } else {
        throw new Error("SBCL stdin not available");
      }

      // Set Safety Timeout
      this.timeoutId = setTimeout(() => {
        if (this.currentReject) {
          console.error(
            `[SBCL] Timeout waiting for: ${item.code.substring(0, 50)}...`
          );
          const reject = this.currentReject;
          this.currentReject = null;
          this.currentResolve = null;
          reject(new Error("SBCL Execution Timeout"));

          // If timed out, process might be stuck.
          // We could try (abort) or just kill/restart?
          // For now, try (abort)
          if (this.process?.stdin)
            this.process.stdin.write("(abort)\n(sb-ext:gc :full t)\n");

          this.isProcessing = false;
          this.processQueue();
        }
      }, this.TIMEOUT_MS);

      // Modifying checkBuffer strategy:
      // We can wrap the original resolve to also call processQueue()
      const originalResolve = this.currentResolve;
      this.currentResolve = (result: string) => {
        if (originalResolve) originalResolve(result);
        this.isProcessing = false;
        this.processQueue();
      };

      // Wait for checkBuffer to handle resolution
    } catch (e) {
      this.clearTimeout();
      item.reject(e);
      this.isProcessing = false;
      this.processQueue();
    }
  }

  // Helper to load a large text into a variable effectively
  async loadText(variableName: string, text: string): Promise<string> {
    // Escaping double quotes for Lisp string
    const escapedText = text.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
    return this.eval(`(defparameter ${variableName} "${escapedText}")`);
  }

  async verifyQuote(textVar: string, quote: string): Promise<boolean> {
    const escapedQuote = quote.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
    // (search "needle" "haystack") returns 0-based index or NIL
    const result = await this.eval(
      `(if (search "${escapedQuote}" ${textVar}) "FOUND" "NIL")`
    );
    return result.includes("FOUND");
  }

  async getMemories(): Promise<Array<{ key: string; value: string }>> {
    // Returns format: (("concept" "definition") ("concept2" "def2"))
    const rawOutput = await this.eval('(s-dialectic:listar-memorias)');
    
    // Simple S-expression parser for list of string pairs
    // Removes outer parens, then finds inner parens
    const memories: Array<{ key: string; value: string }> = [];

    // Normalize: remove newlines, consolidate spaces
    const cleanFn = rawOutput.replace(/\n/g, " ").replace(/\s+/g, " ").trim();

    // If empty or NIL
    if (cleanFn === "NIL" || cleanFn === "()") return [];

    // Regex to match ("key" "value") pairs
    // Note: This is a basic parser and might fail on complex nested strings with escaped quotes
    const regex = /\("([^"]+)"\s+"([^"]+)"\)/g;
    let match;

    while ((match = regex.exec(cleanFn)) !== null) {
      memories.push({ key: match[1], value: match[2] });
    }

    return memories;
  }

  kill() {
    this.process?.kill();
  }
}
