import { spawn, ChildProcess } from 'child_process';
import crypto from 'crypto';
import { EventEmitter } from 'events';


export class SbclProcess extends EventEmitter {
  protected process: ChildProcess | null = null;
  private buffer: string = "";
  private currentResolve:
    | ((value: { result: string; output: string }) => void)
    | null = null;
  protected isReady: boolean = false;
  private readonly SENTINEL = "|||GEMINI_SENTINEL|||";

  private static instance: SbclProcess | null = null;

  public static getInstance(): SbclProcess {
    if (!SbclProcess.instance) {
      SbclProcess.instance = new SbclProcess();
    }
    return SbclProcess.instance;
  }

  private constructor() {
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

  protected start() {
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
      this.currentOutputAccumulator += chunk;
      this.checkBuffer();
    });

    this.process.stderr?.on("data", (data) => {
      const chunk = data.toString();
      this.emit("log", `ERR: ${chunk}`); // Emit error log
      console.error(`SBCL Error: ${data}`);
    });

    this.process.on("close", (code) => {
      console.log(`[SBCL] Process exited with code ${code}`);
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
        const output = this.currentOutputAccumulator
          .split(this.SENTINEL)[0]
          .trim();
        this.currentOutputAccumulator = this.currentOutputAccumulator
          .split(this.SENTINEL)
          .slice(1)
          .join(this.SENTINEL);

        const resolve = this.currentResolve;
        this.currentResolve = null;
        this.clearTimeout(); // Clear timeout on success
        resolve({ result, output });
      }
    } else if (debuggerRegex.test(this.buffer)) {
      console.error("[SBCL] Debugger detected! Aborting to toplevel...");
      const fullBuffer = this.buffer;
      const fullOutput = this.currentOutputAccumulator;

      // Log the error buffer for debugging
      this.emit(
        "log",
        `\n[Use 'abort' to exit debugger]\nBuffer capture:\n${fullBuffer}\n`
      );

      // Try to recover process: send abort to return to toplevel
      if (this.process?.stdin) {
        // Clear buffer and state BEFORE sending abort to prevent race conditions
        this.buffer = "";
        this.currentOutputAccumulator = "";

        // In SBCL, sending '0' or 'abort' usually gets out.
        // We'll send (abort) but also a literal '0' if that fails.
        this.process.stdin.write("(abort)\n");
        // Also send a newline +Sentinel to ensure we catch up if the abort worked
        this.process.stdin.write(`(format t "~%${this.SENTINEL}~%~%")\n`);
      }

      if (this.currentResolve) {
        const reject = this.currentReject;
        this.currentResolve = null;
        this.currentReject = null;
        this.clearTimeout();

        if (reject) {
          reject(
            new Error(
              `SBCL Debugger Invoked. Buffer:\n${fullBuffer}\nOutput:\n${fullOutput}`
            )
          );
        }
      }

      // Ensure processing continues for subsequent items
      this.isProcessing = false;
      setTimeout(() => this.processQueue(), 100);
    }
  }

  private queue: Array<{
    code: string;
    resolve: (value: { result: string; output: string }) => void;
    reject: (reason?: any) => void;
  }> = [];
  private isProcessing = false;
  private currentReject: ((reason?: any) => void) | null = null;
  private timeoutId: NodeJS.Timeout | null = null;
  private readonly TIMEOUT_MS = 30000; // 30s timeout

  private currentOutputAccumulator: string = "";

  async eval(code: string): Promise<{ result: string; output: string }> {
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
      this.currentResolve = (value: { result: string; output: string }) => {
        if (originalResolve) originalResolve(value);
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
    const { result } = await this.eval(
      `(defparameter ${variableName} "${escapedText}")`
    );
    return result;
  }

  async verifyQuote(textVar: string, quote: string): Promise<boolean> {
    const escapedQuote = quote.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
    // (search "needle" "haystack") returns 0-based index or NIL
    const { result } = await this.eval(
      `(if (search "${escapedQuote}" ${textVar}) "FOUND" "NIL")`
    );
    return result.includes("FOUND");
  }

  async getGraphData(): Promise<
    import("../services/graph-service").LispGraphExport
  > {
    const { result: rawOutput } = await this.eval(
      "(s-dialectic:listar-dados-json)"
    );

    // SBCL returns strings wrapped in quotes if they are return values.
    // The format logic in Lisp is (format nil ...), which returns a string.
    // The REPL output for a string is "the string content".
    // We need to unwrap the outer quotes if they exist and unescape specific Lisp escapes.

    // Regex: Match the first occurrence of { ... } that looks like our JSON structure.
    // Lisp returns quotes escaped like \" so we need to handle that.
    // We look for everything between the first { and the last }
    // The previous regex was too strict about surrounding quotes.
    const match = rawOutput.match(/(\{.*\})/s);

    let jsonString = "";
    if (match && match[1]) {
      jsonString = match[1];
      // When SBCL returns a string via eval, it might escape internal quotes.
      // If the string starts with \" and ends with \", it's double-encoded.
      // However, usually `format nil` inside `eval` just returns the string content if not printed to stdout.
      // Let's heuristically clean it.

      // 1. Remove escaped quotes \" -> "
      jsonString = jsonString.replace(/\\"/g, '"');

      // 2. Remove double backslashes \\ -> \
      jsonString = jsonString.replace(/\\\\/g, "\\");

      // 3. Sometimes the outer Lisp string quotes are included in the capture if we aren't careful,
      // but our regex captures from { to }.
    } else {
      console.warn("Could not find JSON in SBCL output. Raw:", rawOutput);
      return { nodes: [], edges: [] };
    }

    try {
      const data = JSON.parse(jsonString);
      return {
        nodes: data.nodes || [],
        edges: data.edges || [],
      };
    } catch (e) {
      console.error("Failed to parse Lisp JSON output:", jsonString);
      console.error("Error:", e);
      return { nodes: [], edges: [] };
    }
  }

  async saveState(filepath: string): Promise<string> {
    // Escape path for Lisp
    const escapedPath = filepath.replace(/\\/g, "/");
    const { result } = await this.eval(
      `(s-dialectic:salvar-estado "${escapedPath}")`
    );
    return result;
  }

  async addRule(
    name: string,
    conditions: string,
    consequences: string
  ): Promise<string> {
    // conditions/consequences should be Lisp list strings, e.g. "((?x is-a ?y))"
    const { result } = await this.eval(
      `(s-dialectic:adicionar-regra '${name} '${conditions} '${consequences})`
    );
    return result;
  }

  async runInference(): Promise<string> {
    const { result } = await this.eval(`(s-dialectic:inferir)`);
    return result;
  }

  async loadState(filepath: string): Promise<string> {
    const escapedPath = filepath.replace(/\\/g, "/");
    const { result } = await this.eval(
      `(s-dialectic:carregar-estado "${escapedPath}")`
    );
    return result;
  }

  // Ensure kill resets the instance so a new one can be created if needed (though unlikely in a running app)
  kill() {
    if (this.process) {
      this.process.kill();
      this.process = null;
    }
    SbclProcess.instance = null;
  }
}

