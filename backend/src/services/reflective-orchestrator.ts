import { ai } from '../genkit';
import { prompt } from '@genkit-ai/dotprompt';
import { SbclProcess } from '../lisp/sbcl-process';
import { CONFIG } from '../config/constants';

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
    this.lisp.emit("log", ";; [Orchestrator] Recognition: Sensing relevant nodes...");
    
    // Simple mock for now: extract nouns and check if they exist in Lisp
    // Future: Use a small LLM call to extract entities for lookup
    const potentialNodes = this.state.userRequest.match(/[A-Z][a-z]+/g) || [];
    
    if (potentialNodes.length > 0) {
      this.state.sensedContext = potentialNodes;
      this.state.reasoningLogs += `\n[Recognition]: Sensed potential entities: ${potentialNodes.join(", ")}`;
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

    while (turn < maxTurns) {
      this.lisp.emit("log", `;; [Orchestrator] Logic Turn ${turn + 1}...`);
      
      const llmResponse = await refPrompt.generate({
        model: `ollama/${CONFIG.OLLAMA_LISP_MODEL_NAME}`,
        input: {
          userRequest: currentTask,
          context: JSON.stringify(this.state.history),
          sensedContext: this.state.sensedContext.join(", "),
        },
      });

      const responseText = llmResponse.text;
      this.state.reasoningLogs += `\n\n--- Step ${turn + 1} (Logic) ---\n${responseText}`;

      const lispRegex = /```(?:lisp|cl|common-lisp)\b\s*([\s\S]*?)```/gi;
      let match;
      let executedSomething = false;

      if ((match = lispRegex.exec(responseText)) !== null) {
        const code = match[1].trim();
        if (code.length > 0) {
          try {
            const { result, output } = await this.lisp.eval(code);
            const toolOutput = output || result;
            
            this.state.history.push({ role: "model", content: responseText });
            this.state.history.push({ role: "tool", content: toolOutput });
            this.state.reasoningLogs += `\n[Execution Output]: ${toolOutput}`;
            
            this.state.lispProducts.push({ code, result: toolOutput });
            executedSomething = true;
          } catch (e) {
            this.state.reasoningLogs += `\n[Execution Error]: ${e}`;
          }
        }
      }

      if (!executedSomething) break;
      turn++;
    }
  }

  /**
   * Package all results into a coherent Fact Package for the Synthesis Layer.
   */
  private async refineKnowledge() {
    this.lisp.emit("log", ";; [Orchestrator] Refinement: Packaging products for System 1...");
    
    // Create a structured summary of what was deduced
    const resultsSummary = this.state.lispProducts.map((p, i) => 
      `Fact ${i+1}: Executing "${p.code}" returned "${p.result}"`
    ).join("\n");

    this.state.factPackage = `
### LOGIC ENGINE RESULTS (VERIFIED)
${resultsSummary || "No direct facts were extracted or inferred in this turn."}

### INTERNAL REASONING TRACE (CONTEXT)
${this.state.reasoningLogs}
    `.trim();
  }

  getReasoningLogs(): string {
    return this.state.reasoningLogs;
  }
}
