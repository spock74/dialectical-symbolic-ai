import { ai } from '../genkit';
import { prompt } from '@genkit-ai/dotprompt';
import { SbclProcess } from '../lisp/sbcl-process';
import { CONFIG } from '../config/constants';
import { scheduleModelUnload } from "./model-cleanup";

export interface WorkspaceState {
  userRequest: string;
  history: any[];
  sensedContext: string[];
  reasoningLogs: string;
  lispProducts: any[];
  factPackage: string;
}

export class ReflectiveOrchestrator {
  private lisp = SbclProcess.getInstance();
  private state: WorkspaceState;

  constructor(input: string, history: any[] = []) {
    this.state = {
      userRequest: input,
      history: history,
      sensedContext: [],
      reasoningLogs: "",
      lispProducts: [],
      factPackage: "",
    };
  }

  async think(): Promise<string> {
    console.log(`[Orchestrator] Thinking initiated: "${this.state.userRequest.substring(0, 50)}..."`);
    
    // 1. Context Sensing (System 1.5 Early)
    await this.recognizeContext();

    // 2. Logic Processing (System 2 Loop)
    await this.processLogic();

    // 3. Knowledge Refinement
    await this.refineKnowledge();

    return this.state.factPackage;
  }

  /**
   * Senses relevant keywords in the Knowledge Graph to prime the Thinking phase.
   */
  private async recognizeContext() {
    this.lisp.emit(
      "log",
      ";; [Orchestrator] Recognition: Sensing relevant nodes..."
    );

    // Sense entities: Proper nouns (uppercase) OR single uppercase letters (A, B, X, Y)
    const potentialNodes =
      this.state.userRequest.match(/\b[A-Z][a-z]*\b/g) || [];

    if (potentialNodes.length > 0) {
      // Remove duplicates
      this.state.sensedContext = [...new Set(potentialNodes)];
      this.state.reasoningLogs += `\n[Recognition]: Sensed potential entities: ${this.state.sensedContext.join(
        ", "
      )}`;
    }
  }

  /**
   * Executing the multi-turn logic loop (Qwen + SBCL).
   */
  private async processLogic() {
    const maxTurns = 3;
    let turn = 0;
    let currentTask = this.state.userRequest;

    // @ts-ignore
    const refPrompt = await prompt(ai.registry, "reflectiveLoop");

    let lastResponseText = "";

    while (turn < maxTurns) {
      this.lisp.emit("log", `;; [Orchestrator] Logic Turn ${turn + 1}...`);

      const llmResponse = await refPrompt.generate({
        model: `ollama/${CONFIG.OLLAMA_LISP_MODEL_NAME}`,
        input: {
          userRequest: currentTask,
          context: this.state.history
            .map((h) => `${h.role === "user" ? "USER" : "AI"}: ${h.content}`)
            .join("\n"),
          sensedContext: this.state.sensedContext.join(", "),
        },
      });

      const responseText = llmResponse.text;

      // Loop Protection: if AI repeats itself exactly, stop the loop
      if (responseText.trim() === lastResponseText.trim()) {
        this.lisp.emit(
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
        /<lisp>([\s\S]*?)<\/lisp>|```(?:lisp|cl|common-lisp)\b\s*([\s\S]*?)```/gi;
      let match;
      let executedSomething = false;

      while ((match = lispRegex.exec(responseText)) !== null) {
        let code = (match[1] || match[2] || "").trim();

        // Remove common REPL prompts and junk if AI accidentally included them
        // This regex matches lines starting with >, *, cl-user>, 0] etc.
        code = code.replace(/^([*>\s]|cl-user>|[0-9]+\])+/, "").trim();

        // If it's just garbage like ">" after cleaning, skip it
        if (!code || code === ">" || code === "*" || !code.includes("(")) {
          continue;
        }
        // Heuristic to skip raw data blocks (lists of lists) that AI sometimes outputs
        if (
          code.startsWith("((") ||
          (code.startsWith("(") &&
            code.length > 5 &&
            !/^\(\s*[a-zA-Z0-9$!%&*+_./:<>?@^~-]+/.test(code))
        ) {
          console.warn(
            `[Orchestrator] Skipping potential data block: ${code.substring(
              0,
              50
            )}...`
          );
          this.state.reasoningLogs += `\n[Info]: Skipped data-only block: ${code.substring(
            0,
            100
          )}`;
          continue;
        }

        try {
          const { result, output } = await this.lisp.eval(code);
          const toolOutput = output || result;

          this.state.history.push({
            role: "model",
            content: `Tool Call: ${code}`,
          });
          this.state.history.push({ role: "tool", content: toolOutput });
          this.state.reasoningLogs += `\n[Lisp]: ${code} -> ${toolOutput}`;

          this.state.lispProducts.push({ code, result: toolOutput });
          executedSomething = true;

          // Proactive Inference: if adding a relation, run inference to stay updated
          if (code.includes("adicionar-relacao")) {
            const inferResult = await this.lisp.eval("(inferir)");
            this.state.reasoningLogs += `\n[Inference]: ${
              inferResult.output || inferResult.result
            }`;
          }
        } catch (e) {
          this.state.reasoningLogs += `\n[Execution Error]: ${e}`;
          console.error(`[Orchestrator] Eval failed:`, e);
        }
      }

      if (!executedSomething) break;
      turn++;
    }

    // Schedule unload for the Logic Model after reasoning turns are complete
    scheduleModelUnload(CONFIG.OLLAMA_LISP_MODEL_NAME);
  }

  /**
   * Package all results into a coherent Fact Package for the Synthesis Layer.
   */
  private async refineKnowledge() {
    this.lisp.emit(
      "log",
      ";; [Orchestrator] Refinement: Packaging products for System 1..."
    );

    // Get final grounded state from the kernel
    const { result: finalMems } = await this.lisp.eval("(listar-memorias)");
    const { result: finalRels } = await this.lisp.eval("(listar-relacoes)");

    // Create a structured summary of what was deduced
    const resultsSummary = this.state.lispProducts
      .map((p, i) => `Turn ${i + 1}: "${p.code}" -> Result: "${p.result}"`)
      .join("\n");

    this.state.factPackage = `
### KERNEL DATA (NEW OR RETRIEVED)
${resultsSummary || "No direct facts were extracted or inferred in this turn."}

### GROUNDED KNOWLEDGE BASE (FINAL STATE)
Concepts: ${finalMems}
Relations: ${finalRels}

### FULL REASONING TRACE (INTERNAL)
${this.state.reasoningLogs}
    `.trim();
  }

  getReasoningLogs(): string {
    return this.state.reasoningLogs;
  }
}
