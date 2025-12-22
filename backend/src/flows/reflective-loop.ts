// Reflective Loop with SBCL integration


import { z } from 'genkit'; // 'zod' is exported as 'z' from genkit usually, or just use 'zod' package.
// Re-reading docs (memory): genkit 1.x exports z.

import { SbclProcess } from '../lisp/sbcl-process';
import { ai } from '../genkit';
import { prompt } from '@genkit-ai/dotprompt';
import { CONFIG } from "../config/constants.js";
import { ReflectiveOrchestrator } from "../services/reflective-orchestrator";

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

    // 1. INITIALIZE COGNITIVE WORKSPACE (System 1.5)
    const orchestrator = new ReflectiveOrchestrator(
      input.prompt,
      input.history || []
    );

    // 2. THINKING PHASE (Orchestration of sensing, logic, and refinement)
    const factPackage = await orchestrator.think();
    const reasoningLogs = orchestrator.getReasoningLogs();

    // 3. SYNTHESIS PHASE (Voice of S-Dialectic)
    console.log(
      `[Flow] Synthesizing final response with Chat Model (Gemma ${CONFIG.OLLAMA_CHAT_MODEL_NAME})...`
    );
    lisp.emit(
      "log",
      `;; [Synthesis] Generating final natural language response...`
    );

    // @ts-ignore
    const synthesisPrompt = await prompt(ai.registry, "chatSynthesis");

    const finalResponse = await synthesisPrompt.generate({
      model: `ollama/${CONFIG.OLLAMA_CHAT_MODEL_NAME}`,
      input: {
        userRequest: originalPrompt,
        history: context, // Passing the history here
        factPackage: factPackage, // Updated key name to match prompt
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
