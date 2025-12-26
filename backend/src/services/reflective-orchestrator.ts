import { ai } from '../genkit';
import { prompt } from "@genkit-ai/dotprompt";
import { CONFIG } from "../config/constants";
import { scheduleModelUnload } from "./model-cleanup";
import { lispInterpreter } from "../logic/lisp-interpreter";
import { getActiveGraph } from "../logic/graph-engine";
import { kernelEvents } from "../logic/kernel-events";

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

  constructor(input: string, history: any[] = [], sourceName?: string) {
    this.state = {
      userRequest: input,
      history: [...history], // Clone to prevent polluting main history
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
    this.events.emit(
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
   * Executing the multi-turn logic loop (Gemini + TS-Kernel).
   */
  private async processLogic() {
    const maxTurns = 3;
    let turn = 0;
    let currentTask = this.state.userRequest;

    // @ts-ignore
    const refPrompt = await prompt(ai.registry, "reflectiveLoop");

    let lastResponseText = "";

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

      const inputVars = {
        userRequest: currentTask,
        context: contextString,
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

      while ((match = lispRegex.exec(responseText)) !== null) {
        let code = (match[1] || match[2] || "").trim();
        // Remove common REPL prefixes if present
        code = code.replace(/^([*>\s]|cl-user>|[0-9]+\])+/, "").trim();

        if (!code || !code.includes("(")) continue;

        try {
          // Execute via SBCL Process
          const output = await lispInterpreter.executeAsync(code);

          this.state.history.push({
            role: "model",
            content: `Tool Call: ${code}`,
          });
          this.state.history.push({ role: "tool", content: output });
          this.state.reasoningLogs += `\n[Lisp]: ${code} -> ${output}`;

          this.state.lispProducts.push({ code, result: output });
          executedSomething = true;

          console.log(`[Orchestrator] Lisp Exec: ${code} -> ${output}`);
          this.events.emit("log", `;; [Lisp] ${code} -> ${output}`);
        } catch (e) {
          this.state.reasoningLogs += `\n[Execution Error]: ${e}`;
          console.error(`[Orchestrator] Eval failed:`, e);
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
  private async refineKnowledge() {
    this.events.emit(
      "log",
      ";; [Orchestrator] Refinement: Packaging products for System 1..."
    );

    const graph = getActiveGraph(this.state.sourceName);
    const finalMems = graph.listNodes();
    const finalRels = graph.listRelations();

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

    // Append the synthesis package to reasoning logs for UI visibility
    this.state.reasoningLogs += `\n\n--- Final Knowledge State ---\n${this.state.factPackage}`;
  }

  getReasoningLogs(): string {
    return this.state.reasoningLogs;
  }
}
