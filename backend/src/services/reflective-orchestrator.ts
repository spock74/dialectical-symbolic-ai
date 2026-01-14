import { ai } from '../genkit';
import { prompt } from "@genkit-ai/dotprompt";
import { CONFIG } from "../config/constants";
import { scheduleModelUnload } from "./model-cleanup";
import { lispInterpreter } from "../logic/lisp-interpreter";
import { getActiveGraph } from "../logic/graph-engine";
import { kernelEvents } from "../logic/kernel-events";
import { embeddingService } from "./embedding-service";

export interface WorkspaceState {
  userRequest: string;
  history: any[];
  sensedContext: string[];
  reasoningLogs: string;
  lispProducts: any[];
  factPackage: string;
  sourceName?: string;
}

export class ReflectiveOrchestrator {
  private events = kernelEvents;
  private state: WorkspaceState;

  constructor(input: string, history: any[] = [], sourceName?: string, useMemory: boolean = false) {
    this.state = {
      userRequest: input,
      history: useMemory ? [...history] : [], // Use history only if useMemory is true
      sensedContext: [],
      reasoningLogs: "",
      lispProducts: [],
      factPackage: "",
      sourceName: sourceName || "default",
    };
  }

  async think(): Promise<string> {
    console.log(
      `[Orchestrator] Thinking initiated: "${this.state.userRequest.substring(
        0,
        50
      )}..."`
    );

    // 1. Context Sensing (System 1.5 Early)
    await this.recognizeContext();

    // 2. Retrieve Relevant Knowledge (Pre-computation)
    const knowledgeContext = await this.retrieveContext();

    // 3. Logic Processing (System 2 Loop)
    await this.processLogic(knowledgeContext);

    // 4. Knowledge Refinement
    await this.refineKnowledge(knowledgeContext);

    return this.state.factPackage;
  }

  /**
   * Senses relevant keywords in the Knowledge Graph to prime the Thinking phase.
   */
  private async recognizeContext() {
    this.events.emit(
      "log",
      ";; [Orchestrator] Recognition: Sensing relevant nodes..."
    );

    // Sense entities: Proper nouns (uppercase) with at least 2 letters.
    const potentialNodes =
      this.state.userRequest.match(/\b[A-Z][a-z0-9_-]{1,}\b/g) || [];

    // [FIX] Filter out common Question Words / Stop Words that might be capitalized at start of sentence
    const STOP_WORDS = new Set([
        "Qual", "Quem", "Como", "Onde", "Quando", "Por que", "Porque", 
        "O", "A", "Os", "As", "Um", "Uma", "De", "Da", "Do", "Em", "Na", "No",
        "E", "Mas", "Porem", "Se", "Entao", "Para", "Por", "Com", "Sem",
        "What", "Who", "How", "Where", "When", "Why", "The", "A", "An"
    ]);

    if (potentialNodes.length > 0) {
      // Remove duplicates & Stop Words
      this.state.sensedContext = [...new Set(potentialNodes)].filter(w => !STOP_WORDS.has(w));
      
      this.state.reasoningLogs += `\n[Recognition]: Sensed potential entities: ${this.state.sensedContext.join(
        ", "
      )}`;

      // --- NEW: Topological Injection ---
      this.events.emit("log", ";; [Orchestrator] Topology: Injecting vectors for sensed entities...");
      const injectionPromises = this.state.sensedContext.map(async (entity) => {
        try {
          const vector = await embeddingService.getEmbedding(entity);
          if (vector) {
            const vecStr = embeddingService.formatVectorForLisp(vector);
            const cmd = `(atualizar-vetor '${entity} ${vecStr})`;
            // Execute silently to update graph state
            await lispInterpreter.executeAsync(cmd);
            return entity; 
          }
        } catch (e) {
          console.warn(`[Orchestrator] Failed to inject vector for ${entity}`, e);
        }
        return null;
      });

      const injected = await Promise.all(injectionPromises);
      const successCount = injected.filter(x => x !== null).length;
      this.events.emit("log", `;; [Orchestrator] Topology: ${successCount} vectors injected.`);
    }
  }

  /**
   * Retrieves relevant context (Nodes/Relations) from Lisp/JS.
   */
  private async retrieveContext(): Promise<{ mems: string; rels: string }> {
     this.events.emit("log", ";; [Orchestrator] Retrieval: Fetching grounded knowledge...");
     const graph = getActiveGraph(this.state.sourceName);
     let mems = "";
     let rels = "";

     /* Hybrid Retrieval Logic */
     if (this.state.sensedContext.length > 0) {
        try {
           const contextCmd = `(recuperar-foco-semantico "${this.state.sensedContext.join(" ")}" :limit 10)`; 
           let refinedContext = await lispInterpreter.executeAsync(contextCmd);

           if (refinedContext.trim().startsWith("(") && !refinedContext.includes("NIL")) {
                const edges = await lispInterpreter.executeAsync(`(recuperar-foco-atencional '(${this.state.sensedContext.join(" ")}) :limit-neighbors 15)`);
                
                const relsList = edges
                 .replace(/^\(/, "")
                 .replace(/\)$/, "")
                 .split(') (')
                 .map(s => s.replace(/[()"]/g, "").trim())
                 .filter(s => s.length > 0);
                rels = relsList.join("\n");

                const semIds = refinedContext
                   .replace(/^\(/, "")
                   .replace(/\)$/, "")
                   .split(" ")
                   .map(s => s.replace(/"/g, "").trim())
                   .filter(s => s.length > 0 && s !== "NIL");
                   
               mems = semIds.map(id => {
                    const n = graph.getNode(id);
                    return n ? `(${n.id} :type ${n.type} :value "${n.value || ""}")` : `(${id})`;
               }).join("\n");
           }
        } catch(e) {
            console.warn("[Orchestrator] Semantic Retrieval Failed:", e);
        }
     }

     // Use Full Dump as Fallback if Semantic failed
     if (!mems || !rels || (this.state.sensedContext.length === 0)) {
        try {
           this.events.emit("log", ";; [Orchestrator] Retrieval: Fetching FULL Graph Context from Lisp...");
           const lispJson = await lispInterpreter.executeAsync("(progn (princ (listar-dados-json)) (values))");
            
           let parsed = { nodes: [], edges: [] };
           try { parsed = JSON.parse(lispJson); } catch(e) { /* ignore parse error */ }
           
           if (parsed.nodes && parsed.nodes.length > 0) {
              // @ts-ignore
              mems = parsed.nodes.map(n => `(${n.key} :type ${n.type} :value "${n.value}")`).join("\n").substring(0, 5000);
              // @ts-ignore
              rels = parsed.edges.map(e => `(${e.source} "${e.relation}" ${e.target})`).join("\n").substring(0, 5000);
           } else {
              mems = graph.listNodes().substring(0, 5000) + "...";
              rels = graph.listRelations().substring(0, 5000) + "...";
           }
        } catch (e) {
           console.warn("[Orchestrator] Lisp Full Dump Failed, falling back to JS:", e);
           mems = graph.listNodes().substring(0, 5000) + "...";
           rels = graph.listRelations().substring(0, 5000) + "...";
        }
     }
     
     return { mems, rels };
  }

  /**
   * Executing the multi-turn logic loop (Gemini + TS-Kernel).
   */
  private async processLogic(knowledge: { mems: string; rels: string }) {
    const maxTurns = 3;
    let turn = 0;
    let currentTask = this.state.userRequest;

    // @ts-ignore
    const refPrompt = await prompt(ai.registry, "reflectiveLoop");

    let lastResponseText = "";
    
    // Inject Knowledge into Context String
    const knowledgeString = `
### RELEVANT KNOWLEDGE BASE
CONCEPTS:
${knowledge.mems}

RELATIONS:
${knowledge.rels}
    `.trim();

    while (turn < maxTurns) {
      this.events.emit("log", `;; [Orchestrator] Logic Turn ${turn + 1}...`);

      const contextString = this.state.history
        .map((h) => {
          let contentText = "";
          if (Array.isArray(h.content)) {
            contentText = h.content.map((p: any) => p.text || "").join("");
          } else if (typeof h.content === "string") {
            contentText = h.content;
          } else {
            contentText = JSON.stringify(h.content);
          }
          return `${h.role.toUpperCase()}: ${contentText}`;
        })
        .join("\n");
      
      const fullContext = `${knowledgeString}\n\n${contextString}`;

      const inputVars = {
        userRequest: currentTask,
        context: fullContext,
        sensedContext: this.state.sensedContext.join(", "),
      };

      // Render total context for observability
      const rendered = await refPrompt.render({ input: inputVars });
      console.log(
        `--- [DEBUG] TOTAL CONTEXT (reflectiveLoop - Turn ${turn + 1}) ---`
      );
      try {
        const text = rendered.messages
          ?.map((m) =>
            m.content?.map((p) => p.text || JSON.stringify(p)).join("")
          )
          .join("\n---\n");
        if (text) {
          console.log(text);
        } else {
          console.log(
            "Empty text extraction. Full object:",
            JSON.stringify(rendered, null, 2)
          );
        }
      } catch (e) {
        console.log(
          "Extraction Error. Full object:",
          JSON.stringify(rendered, null, 2)
        );
      }
      console.log(
        "----------------------------------------------------------------"
      );

      const llmResponse = await refPrompt.generate({
        model: CONFIG.LISP_MODEL,
        input: inputVars,
      });

      const responseText = llmResponse.text;

      console.log(
        `[Orchestrator] LLM Thought (Turn ${turn + 1}):\n${responseText}\n---`
      );

      // Loop Protection
      if (responseText.trim() === lastResponseText.trim()) {
        this.events.emit(
          "log",
          ";; [Orchestrator] No progress in turn. Breaking loop."
        );
        break;
      }
      lastResponseText = responseText;

      this.state.reasoningLogs += `\n\n--- Step ${
        turn + 1
      } (Logic) ---\n${responseText}`;

      const lispRegex =
        /<lisp>([\s\S]*?)<\/lisp>|```(?:lisp|cl|common-lisp)?\s*([\s\S]*?)```/gi;
      let match;
      let executedSomething = false;
      const batchCommands: string[] = [];

      // Helper for Parenthesis Balance
      const isBalanced = (str: string): boolean => {
        let depth = 0;
        let inString = false;
        let escaped = false;
        
        for (let i = 0; i < str.length; i++) {
          const char = str[i];
          if (escaped) { escaped = false; continue; }
          if (char === '\\') { escaped = true; continue; }
          if (char === '"') { inString = !inString; }
          if (!inString) {
            if (char === '(') depth++;
            if (char === ')') depth--;
          }
          if (depth < 0) return false;
        }
        return depth === 0 && !inString;
      };

      // 1. Collect all candidates
      while ((match = lispRegex.exec(responseText)) !== null) {
        let code = (match[1] || match[2] || "").trim();
        // Remove common REPL prefixes if present
        code = code.replace(/^\s*([*>]|cl-user>|[0-9]+\])+\s*/, "").trim();

        if (code && code.includes("(")) {
            if (isBalanced(code)) {
                batchCommands.push(code);
            } else {
                console.warn(`[Safety] Dropping unbalanced Lisp command: ${code}`);
                this.state.reasoningLogs += `\n;; [Safety] Dropped Unbalanced: ${code}`;
            }
        }
      }

      // 2. Execute Batch (O(1) IPC round-trip)
      if (batchCommands.length > 0) {
        try {
          const joinedCode = `(progn ${batchCommands.join(" ")} )`;
          console.log(`[Orchestrator] Batch Executing ${batchCommands.length} commands...`);
          
          const output = await lispInterpreter.executeAsync(joinedCode);

          this.state.history.push({
            role: "model",
            content: `Tool Call (Batch): ${batchCommands.length} operations`,
          });
          this.state.history.push({ role: "tool", content: output });
          
          // Log individually for UI readability or use summary
          this.state.reasoningLogs += `\n[Lisp Batch]: Executed ${batchCommands.length} commands. Result: ${output}`;

          this.state.lispProducts.push({ code: "batch-ops", result: output });
          executedSomething = true;
          this.events.emit("log", `;; [Lisp] Batch success: ${output}`);
        } catch (e) {
          this.state.reasoningLogs += `\n[Execution Error]: ${e}`;
          console.error(`[Orchestrator] Batch Eval failed:`, e);
        }
      }

      if (!executedSomething) {
          // Debug check: did we miss something that LOOKS like lisp?
          if (responseText.includes('<lisp>') || responseText.includes('```')) {
              console.warn(`[Orchestrator] Warning: Found tags but Lisp extraction failed matching regex.`);
          }
          break;
      }
      turn++;
    }

    if (CONFIG.USE_LOCAL_MODELS)
      scheduleModelUnload(CONFIG.OLLAMA_LISP_MODEL_NAME);
  }

  /**
   * Package all results into a coherent Fact Package for the Synthesis Layer.
   */
  private async refineKnowledge(knowledge: { mems: string; rels: string }) {
    // 3. Knowledge Refinement
    this.events.emit(
      "log",
      ";; [Orchestrator] Refinement: Packaging knowledge for System 1..."
    );

    const resultsSummary = this.state.lispProducts
      .map((p, i) => `Turn ${i + 1}: "${p.code}" -> Result: "${p.result}"`)
      .join("\n");

    this.state.factPackage = `
### KERNEL DATA (NEW OR RETRIEVED)
${resultsSummary || "No direct facts were extracted or inferred in this turn."}

### GROUNDED KNOWLEDGE BASE (FOCUSED SUBGRAPH)
Concepts: 
${knowledge.mems}

Relations: 
${knowledge.rels}

### FULL REASONING TRACE (INTERNAL)
${this.state.reasoningLogs}
    `.trim();

    // Append the synthesis package to reasoning logs for UI visibility
    this.state.reasoningLogs += `\n\n--- Final Knowledge State ---\n${this.state.factPackage}`;
  }

  getReasoningLogs(): string {
    return this.state.reasoningLogs;
  }
}
