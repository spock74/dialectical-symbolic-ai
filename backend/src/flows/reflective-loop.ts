// Reflective Loop with SBCL integration


import { z } from 'genkit'; // 'zod' is exported as 'z' from genkit usually, or just use 'zod' package.
// Re-reading docs (memory): genkit 1.x exports z.

import { SbclProcess } from '../lisp/sbcl-process';
import { ai } from '../genkit';
import { prompt } from '@genkit-ai/dotprompt';
import { CONFIG } from "../config/constants.js";

const nameModel = CONFIG.OLLAMA_LISP_MODEL_NAME;

// Initialize SBCL process
export const lisp = SbclProcess.getInstance();

// Helper to save state efficiently
const saveKnowledgeGraph = async () => {
  try {
    const result = await lisp.saveState("knowledge.lisp");
    lisp.emit("log", `;; [System] Auto-Saved Knowledge Graph: ${result}`);
  } catch (e) {
    console.error("Failed to auto-save KG:", e);
  }
};

const InputSchema = z.object({
  prompt: z.string(),
  history: z.array(z.any()).optional(),
});

export const reflectiveLoop = ai.defineFlow(
  {
    name: "reflectiveLoop",
    inputSchema: InputSchema,
  },
  async (input) => {
    let context = input.history || [];
    const originalPrompt = input.prompt;
    let currentPrompt = input.prompt;
    const maxTurns = 5;
    let turn = 0;

    // Accumulate the "Stream of Thought" to pass to the Chat Model
    let reasoningLogs = "";

    console.log(
      `[Flow] Starting Neuro-Symbolic Loop. Logic Model: Qwen 2.5 Coder`
    );

    while (turn < maxTurns) {
      console.log(`--- Turn ${turn} ---`);

      // 1. LOGIC PHASE (Middleware)
      // Uses reflectiveLoop.prompt (Hardcoded to qwen2.5-coder:3b)
      // @ts-ignore
      const refPrompt = await prompt(ai.registry, "reflectiveLoop");

      const llmResponse = await refPrompt.generate({
        model: `ollama/${CONFIG.OLLAMA_LISP_MODEL_NAME}`,
        input: {
          userRequest: currentPrompt,
          context: JSON.stringify(context),
        },
      });

      const responseText = llmResponse.text;
      console.log("Logic Model:", responseText);
      reasoningLogs += `\n\n--- Step ${turn + 1} (Logic) ---\n${responseText}`;

      // 2. EXECUTION PHASE (Lisp)
      const lispRegex = /```(?:lisp|cl|common-lisp)\b\s*([\s\S]*?)```/gi;
      let match;
      let executedSomething = false;
      lispRegex.lastIndex = 0;

      if ((match = lispRegex.exec(responseText)) !== null) {
        const code = match[1].trim();
        if (code.length > 0 && code !== "(inferir)") {
          console.log("Executing Lisp:", code);
          lisp.emit("log", `;; [Logic] Executing Symbolic Code...`);
          try {
            const { result, output } = await lisp.eval(code);
            console.log("Result:", result);
            console.log("Output:", output);

            const toolOutput = output || result;
            context.push({ role: "model", content: responseText });
            context.push({ role: "tool", content: toolOutput });

            reasoningLogs += `\n[Execution Output]: ${toolOutput}`;
            currentPrompt = "Continue reasoning based on the Lisp result.";
            executedSomething = true;
          } catch (e) {
            console.error("Lisp Error:", e);
            const errorMsg = String(e);
            lisp.emit("log", `;; [Logic] Error: ${errorMsg}`);
            context.push({ role: "tool", content: `Error: ${errorMsg}` });
            reasoningLogs += `\n[Execution Error]: ${errorMsg}`;
            executedSomething = true;
          }
        } else if (code === "(inferir)") {
          // Allow ONE final inference but don't loop again unless it produces something (future)
          // For now, treat pure inferir as a terminal step in the loop
          await lisp.eval("(inferir)");
          reasoningLogs += `\n[Step]: Logic Engine invoked (inferir).`;
          executedSomething = false; // Terminal
        }
      } else {
        lisp.emit("log", `;; [Logic] Logical reasoning complete.`);
      }

      if (!executedSomething) {
        break;
      }
      turn++;
    }

    // 3. SYNTHESIS PHASE (Chat)
    // Uses chatSynthesis.prompt (Hardcoded to gemma3:270m)
    console.log(
      `[Flow] Synthesizing final response with Chat Model (Gemma ${CONFIG.OLLAMA_CHAT_MODEL_NAME})...`
    );
    lisp.emit(
      "log",
      `;; [Synthesis] Generating final natural language response...`
    );

    // @ts-ignore
    const synthesisPrompt = await prompt(ai.registry, "chatSynthesis");

    // We pass the full accumulated reasoning logs so Gemma knows what happened
    const finalResponse = await synthesisPrompt.generate({
      model: `ollama/${CONFIG.OLLAMA_CHAT_MODEL_NAME}`,
      input: {
        userRequest: originalPrompt,
        reasoningLogs: reasoningLogs,
      },
    });

    let finalText = finalResponse.text;
    // Fallback Mechanism
    if (!finalText || finalText.trim().length === 0) {
      console.warn(
        "Chat Model returned empty. Falling back to Logic Model output."
      );

      // Fallback: Use the last valid response from the logic loop (basic parsing)
      // or a generic message if logic was silent (unlikely).
      finalText = `The Logic Engine processed your request, but the Synthesis Layer (Gemma model: ${CONFIG.OLLAMA_CHAT_MODEL_NAME}) returned an empty response. Please check the Reasoning Console for the full trace.`;
    }

    // Auto-Save at the end of the turn
    await saveKnowledgeGraph();

    console.log("Chat Model:", finalText);
    return finalText;
  }
);
