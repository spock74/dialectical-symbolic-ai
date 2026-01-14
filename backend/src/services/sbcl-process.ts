import { spawn, ChildProcessWithoutNullStreams } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import { EventEmitter } from 'events';

interface SBCLCommand {
  resolve: (value: string) => void;
  reject: (reason: any) => void;
  command: string;
  isBootstrap?: boolean;
  timeoutMs: number;
}

export class SBCLProcess extends EventEmitter {
  private static instance: SBCLProcess;
  private process: ChildProcessWithoutNullStreams | null = null;
  private isReady: boolean = false;
  private commandQueue: SBCLCommand[] = [];
  private activeCommand: SBCLCommand | null = null;
  private isProcessing: boolean = false;
  
  // Marcadores robustos para isolar o output
  private static readonly START_MARKER = '---START-OUTPUT---';
  private static readonly END_MARKER = '---END-OUTPUT---';
  private currentBuffer: string = '';
  private initialized: boolean = false;

  private constructor() {
    super();
    this.spawnProcess();
  }

  public static getInstance(): SBCLProcess {
    if (!SBCLProcess.instance) {
      SBCLProcess.instance = new SBCLProcess();
    }
    return SBCLProcess.instance;
  }

  private spawnProcess() {
    console.log('[SBCL] Spawning child process...');
    this.process = spawn('sbcl', ['--noinform', '--disable-debugger']);

    this.process.on('error', (err) => {
        console.error('[SBCL] Failed to start subprocess:', err);
    });

    if (!this.process) {
      console.error('[SBCL] Failed to spawn process!');
      return;
    }

    this.process.stdout.on('data', (data) => this.handleStdout(data));
    this.process.stderr.on('data', (data) => this.handleStderr(data));
    
    this.process.on('close', (code) => {
      console.warn(`[SBCL] Process exited with code ${code}. Restarting...`);
      
      // Reject any pending commands to avoid hanging promises
      if (this.activeCommand) {
        this.activeCommand.reject(new Error(`SBCL process exited unexpectedly with code ${code}`));
      }
      
      this.commandQueue.forEach(cmd => cmd.reject(new Error('SBCL process closed')));
      this.commandQueue = [];
      
      this.isReady = false;
      this.process = null;
      this.activeCommand = null;
      this.isProcessing = false;
      this.initialized = false; // Reset init flag so bootstrap is re-loaded on restart
      setTimeout(() => this.spawnProcess(), 1000);
    });
  }

  private async loadBootstrap() {
    const bootstrapPath = path.resolve(__dirname, '../../lisp/bootstrap.lisp');
    if (fs.existsSync(bootstrapPath)) {
        console.log(`[SBCL] Loading bootstrap from ${bootstrapPath}...`);
        
        try {
            const content = fs.readFileSync(bootstrapPath, 'utf8');
            const parts = content.split(';;; --- SECTION-BARRIER ---');
            
            for (let i = 0; i < parts.length; i++) {
                const part = parts[i].trim();
                if (part) {
                    console.log(`[SBCL] Loading Bootstrap Part ${i + 1}/${parts.length}...`);
                    // Timeout maior para bootstrap (120s)
                    await this.evaluate(part, 120000, true);
                }
            }

            console.log('[SBCL] Bootstrap Complete.'); 
            this.isReady = true;
            this.emit('ready');
            // Tenta processar comandos que ficaram parados
            this.processQueue();
        } catch (err) {
             console.error('[SBCL] Bootstrap Failed:', err);
        }
    } else {
        console.error('[SBCL] bootstrap.lisp NOT FOUND!');
    }
  }

  private sanitizeOutput(raw: string): string {
    // Remove aspas iniciais/finais se o Lisp retornou como string literal (raro com princ)
    let cleaned = raw.trim();
    if (cleaned.startsWith('"') && cleaned.endsWith('"')) {
      cleaned = cleaned.substring(1, cleaned.length - 1);
    }

    // Remove artefatos comuns do REPL do SBCL que podem ter vazado
    let result = cleaned
      .replace(/^\*+ /gm, '')             // Prompt principal "* "
      .replace(/\r?\n\*+ ?$/g, '')        // Prompt no final da string com newline
      .replace(/ \*+ ?$/g, '')            // Prompt no final da string com espaço
      .replace(/^\[[0-9]+\] /gm, '')      // Prompt do debugger "[1] "
      .replace(/^[0-9]+\[[0-9]+\] /gm, '') // Prompt do debugger aninhado "0[3] "
      .replace(/^#<PACKAGE [^>]+>/gm, '') // Info de pacote "#<PACKAGE S-DIALECTIC>"
      .replace(/\r?\nNIL$/g, '')          // Remove NIL resultante de (values) no final
      .trim();

    // Se o resultado ainda parecer conter o prompt do debugger no início, remova-o
    if (result.match(/^[0-9]+\[[0-9]+\]/)) {
        result = result.replace(/^[0-9]+\[[0-9]+\]\s*/, '');
    }

    return result;
  }

  private handleStdout(data: Buffer) {
    const chunk = data.toString();
    // [DEBUG] Pipe to backend console for observability
    process.stdout.write(chunk); 
    
    this.checkInitialization(chunk);
    this.emit('data', chunk);
    this.currentBuffer += chunk;

    if (this.currentBuffer.includes(SBCLProcess.END_MARKER)) {
        // console.log('[SBCLProcess] DEBUG: Found END_MARKER in buffer.');
        const startIndex = this.currentBuffer.indexOf(SBCLProcess.START_MARKER);
        const endIndex = this.currentBuffer.indexOf(SBCLProcess.END_MARKER);

        if (this.activeCommand) {
            let result = "";
            if (startIndex !== -1 && endIndex !== -1 && endIndex > startIndex) {
                const contentStart = startIndex + SBCLProcess.START_MARKER.length;
                const rawResult = this.currentBuffer.substring(contentStart, endIndex).trim();
                result = this.sanitizeOutput(rawResult);
            } else {
                result = "NIL"; 
            }

            const resolve = this.activeCommand.resolve;
            this.activeCommand = null;
            this.isProcessing = false;
            resolve(result);
        } else {
            // Se não há comando ativo (ex: timeout ja ocorreu), limpamos apenas
            this.isProcessing = false;
        }

        // Limpa o buffer indepentente de ter comando ativo ou não (evita acúmulo de markers órfãos)
        if (endIndex !== -1) {
            this.currentBuffer = this.currentBuffer.substring(endIndex + SBCLProcess.END_MARKER.length);
        }
        
        this.processQueue();
    }
  }

  private handleStderr(data: Buffer) {
    const output = data.toString();
    console.error('[SBCL stderr]', output);
    this.emit('data', `[STDERR] ${output}`);

    if (output.includes('] ') || output.includes('break')) {
        console.warn('[SBCL] Debugger or error detected! Attempting recovery...');
        if (this.process) {
            this.process.stdin.write('(abort)\n');
        }
        
        if (this.activeCommand) {
            const reject = this.activeCommand.reject;
            const msg = `SBCL Debugger invoked: ${output}`;
            this.activeCommand = null;
            this.isProcessing = false;
            reject(new Error(msg));
            this.processQueue();
        }
    }
  }

  private checkInitialization(chunk: string) {
    if (!this.initialized && (chunk.includes('* ') || chunk.includes('> '))) {
        console.log('[SBCL] Process ready for input.');
        this.initialized = true;
        this.loadBootstrap();
    }
  }

  public evaluate(code: string, timeoutMs: number = 60 * 1000, isBootstrap: boolean = false): Promise<string> {
    return new Promise((resolve, reject) => {
        if (!this.process) {
            return reject(new Error('SBCL Process not active'));
        }
        this.commandQueue.push({ resolve, reject, command: code, isBootstrap, timeoutMs });
        this.processQueue();
    });
  }

  private async processQueue() {
    // console.log(`[SBCLProcess] processQueue check. Queue: ${this.commandQueue.length}, Ready: ${this.isReady}, Processing: ${this.isProcessing}`);
    if (this.isProcessing || this.commandQueue.length === 0) return;

    // Procura o primeiro comando executável (prioriza bootstrap se !isReady)
    let targetIndex = -1;
    for (let i = 0; i < this.commandQueue.length; i++) {
        if (this.commandQueue[i].isBootstrap || this.isReady) {
            targetIndex = i;
            break;
        }
    }

    if (targetIndex === -1) {
        if (this.commandQueue.length > 0) console.log(`[SBCLProcess] Waiting for Bootstrap to complete before executing ${this.commandQueue.length} commands...`);
        return;
    }

    this.isProcessing = true;
    const current = this.commandQueue.splice(targetIndex, 1)[0];
    this.activeCommand = current;

    const timer = setTimeout(() => {
        if (this.activeCommand === current) {
            console.error(`[SBCL] TIMEOUT detected for command: ${current.command.substring(0, 100)}...`);
            this.activeCommand = null;
            this.isProcessing = false;
            current.reject(new Error(`SBCL Execution Timeout (${current.timeoutMs}ms) for command: ${current.command.slice(0, 50)}...`));
            this.processQueue();
        }
    }, current.timeoutMs);

    const originalResolve = current.resolve;
    current.resolve = (value) => {
        clearTimeout(timer);
        originalResolve(value);
    };
    
    const originalReject = current.reject;
    current.reject = (err) => {
        clearTimeout(timer);
        originalReject(err);
    };

    const wrappedCode = current.isBootstrap 
      ? current.command.trim() 
      : `(handler-case (progn (in-package :s-dialectic) ${current.command.trim()}) (error (c) (format nil "LISP-ERROR: ~A" c)))`;

    const envelope = `
(progn (fresh-line) (format t "${SBCLProcess.START_MARKER}~%") (force-output) (values))
${wrappedCode}
(progn (fresh-line) (format t "~%${SBCLProcess.END_MARKER}~%") (force-output) (values))
`;
    
    if (this.process) {
      console.log(`[SBCLProcess] Writing to stdin:\n${envelope}`);
      this.process.stdin.write(envelope + '\n');
    }
  }

  async getLispGraphData(): Promise<any> {
    // Log do estado estrutural antes de extrair JSON para debug
    try {
      const stateInfo = await this.evaluate(`(format nil "LB-COUNT: ~a, REL-COUNT: ~a" (hash-table-count *knowledge-graph*) (length *relations*))`);
      console.log(`[SBCLProcess] Internal state: ${stateInfo}`);
    } catch (e) {
      console.error("[SBCLProcess] Failed to get internal state info:", e);
    }

    // Usamos princ + values para garantir que o JSON saia cru, sem aspas de escape do REPL
    const output = await this.evaluate("(progn (princ (listar-dados-json)) (values))");
    
    // Tenta primeiro o parse direto
    try {
      return JSON.parse(output);
    } catch (e) {
      // Se falhar por causa de caracteres extras (como o prompt '*' no final), tenta extrair o bloco JSON
      console.warn("[SBCLProcess] Direct JSON parse failed, attempting extraction...");
      
      const firstBrace = output.indexOf('{');
      const lastBrace = output.lastIndexOf('}');
      
      if (firstBrace !== -1 && lastBrace !== -1 && lastBrace > firstBrace) {
        const extracted = output.substring(firstBrace, lastBrace + 1);
        try {
          return JSON.parse(extracted);
        } catch (e2) {
          console.error("[SBCLProcess] Extraction also failed to parse:", e2);
        }
      }

      console.error("[SBCLProcess] Failed to parse Lisp graph JSON:", e);
      // Log extra details to help debug
      console.log("[SBCLProcess] Length:", output.length, "First 20 chars:", output.substring(0, 20));
      const lastPart = output.substring(Math.max(0, output.length - 50));
      console.log("[SBCLProcess] Last 50 chars:", lastPart);
      return { nodes: [], edges: [] };
    }
  }
}
