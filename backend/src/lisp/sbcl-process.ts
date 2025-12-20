import { spawn, ChildProcess } from 'child_process';
import crypto from 'crypto';
import { EventEmitter } from 'events';

export class SbclProcess extends EventEmitter {
  private process: ChildProcess | null = null;
  private buffer: string = '';
  private currentResolve: ((result: string) => void) | null = null;
  private isReady: boolean = false;
  private readonly SENTINEL = '|||GEMINI_SENTINEL|||';

  constructor() {
    super();
    this.start();
    
    // Ensure child process is killed when Node exits
    process.on('exit', () => this.kill());
    process.on('SIGINT', () => { this.kill(); process.exit(); });
    process.on('SIGTERM', () => { this.kill(); process.exit(); });
  }

  private start() {
    // Limit heap to 1GB to prevent OOM on user machine
    // Load bootstrap file on startup
    const bootstrapFile = 'lisp/bootstrap.lisp';
    this.process = spawn('sbcl', [
      '--dynamic-space-size', '1024', 
      '--noinform', 
      '--interactive'
    ]);
    
    // Explicitly command SBCL to load the bootstrap file to ensure it happens
    if (this.process.stdin) {
        // Escape backslashes for Windows, though this is Mac. 
        // Lisp string needs double backslashes.
        const lispPath = bootstrapFile.replace(/\\/g, '/');
        const loadCommand = `(load "${lispPath}")\n`;
        console.log(`[SBCL] Sending usage command: ${loadCommand}`);
        this.process.stdin.write(loadCommand);
    }

    this.process.stdout?.on('data', (data) => {
      const chunk = data.toString();
      this.emit('log', chunk); // Emit log event
      this.buffer += chunk;
      this.checkBuffer();
    });

    this.process.stderr?.on('data', (data) => {
      const chunk = data.toString();
      this.emit('log', `ERR: ${chunk}`); // Emit error log
      console.error(`SBCL Error: ${data}`);
    });

    this.process.on('close', (code) => {
      console.log(`SBCL process exited with code ${code}`);
      this.process = null;
      this.isReady = false;
    });

    this.isReady = true;
  }

  private checkBuffer() {
    if (this.buffer.includes(this.SENTINEL)) {
      const parts = this.buffer.split(this.SENTINEL);
      const result = parts[0].trim();
      this.buffer = parts.slice(1).join(this.SENTINEL);

      if (this.currentResolve) {
        const resolve = this.currentResolve;
        this.currentResolve = null;
        resolve(result);
      }
    }
  }

  async eval(code: string): Promise<string> {
    if (!this.process || !this.isReady) {
      throw new Error('SBCL process is not running');
    }
    if (this.currentResolve) {
      // Simple retry mechanism or queue could be better, but throwing ensures manual handling
      throw new Error('SBCL is busy executing another command');
    }

    return new Promise((resolve) => {
      this.currentResolve = resolve;
      // Echo the command to the log stream so the user sees their input
      this.emit('log', `> ${code}\n`);
      
      // We escape the code slightly if needed, but assuming 'code' is valid lisp string or usage
      // We rely on standard IO.
      this.process?.stdin?.write(`${code}\n`);
      this.process?.stdin?.write(`(format t "${this.SENTINEL}~%")\n`);
    });
  }

  // Helper to load a large text into a variable effectively
  async loadText(variableName: string, text: string): Promise<string> {
    // Escaping double quotes for Lisp string
    const escapedText = text.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    return this.eval(`(defparameter ${variableName} "${escapedText}")`);
  }

  async verifyQuote(textVar: string, quote: string): Promise<boolean> {
     const escapedQuote = quote.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
     // (search "needle" "haystack") returns 0-based index or NIL
     const result = await this.eval(`(if (search "${escapedQuote}" ${textVar}) "FOUND" "NIL")`);
     return result.includes("FOUND");
  }

  kill() {
    this.process?.kill();
  }
}
