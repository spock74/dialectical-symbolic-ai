// Reflective Loop with TS-Symbolic-Kernel integration

import { z } from "genkit";
import { ai } from "../genkit";
import { prompt } from "@genkit-ai/dotprompt";
import { CONFIG } from "../config/constants.js";
import { ReflectiveOrchestrator } from "../services/reflective-orchestrator";
import { scheduleModelUnload } from "../services/model-cleanup";
import { getActiveGraph } from "../logic/graph-engine";
import { kernelEvents } from "../logic/kernel-events";

// Helper to save state efficiently
const saveKnowledgeGraph = (source?: string) => {
  try {
    const graph = getActiveGraph(source);
    graph.saveState(`data/graphs/${source || "default"}.json`);
    kernelEvents.emit(
      "log",
      `;; [System] Auto-Saved Knowledge Graph (${source || "default"})`
    );
  } catch (e) {
    console.error("Failed to auto-save KG:", e);
  }
};

const InputSchema = z.object({
  prompt: z.string(),
  history: z.array(z.any()).optional(),
  useMemory: z.boolean().optional(),
  bypassSDialect: z.boolean().optional(),
  source: z.string().optional(),
});

export const reflectiveLoop = ai.defineFlow(
  {
    name: "reflectiveLoop",
    inputSchema: InputSchema,
  },
  async (input) => {
    let context = input.history || [];
    const originalPrompt = input.prompt;

    let factPackage = "";
    let reasoningLogs = "";

    if (input.bypassSDialect) {
      console.log(
        "[Flow] S-Dialect Bypass ENABLED. Skipping symbolic reasoning."
      );
      kernelEvents.emit(
        "log",
        ";; [System] S-Dialect Bypass ENABLED. Direct LLM interaction."
      );

      try {
        // @ts-ignore
        const directPrompt = await prompt(ai.registry, "directChat");
        // Render total context for observability
        const rendered = await directPrompt.render({
          input: {
            userRequest: originalPrompt,
            history: input.useMemory === true ? context : [],
          },
        });
        console.log("--- [DEBUG] TOTAL CONTEXT (directChat) ---");
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
        console.log("------------------------------------------");

        const response = await directPrompt.generate({
          model: CONFIG.CHAT_MODEL,
          input: {
            userRequest: originalPrompt,
            history: input.useMemory === true ? context : [],
          },
        });

        return {
          text: response.text,
          reasoningLogs: "S-Dialect bypassed. No symbolic trace available.",
        };
      } finally {
        if (CONFIG.USE_LOCAL_MODELS)
          scheduleModelUnload(CONFIG.OLLAMA_CHAT_MODEL_NAME);
      }
    } else {
      // 1. INITIALIZE COGNITIVE WORKSPACE (System 1.5)
      const orchestrator = new ReflectiveOrchestrator(
        input.prompt,
        input.history || [],
        input.source,
        input.useMemory === true
      );

      // 2. THINKING PHASE
      try {
        factPackage = await orchestrator.think();
        reasoningLogs = orchestrator.getReasoningLogs();
      } catch (error) {
        console.error("[Flow] ReflectiveOrchestrator crashed:", error);
        reasoningLogs = `;; [System] The Thinking Layer encountered a critical error: ${error}\n;; Falling back to intuitive response.`;
        factPackage = "No direct facts were extracted due to a Thinking Layer failure.";
      }
    }

    // 3. SYNTHESIS PHASE
    console.log(
      `[Flow] Synthesizing final response with Chat Model (${CONFIG.CHAT_MODEL})...`
    );

    try {
      // @ts-ignore
      const synthesisPrompt = await prompt(ai.registry, "chatSynthesis");

      // Render total context for observability
      const rendered = await synthesisPrompt.render({
        input: {
          userRequest: originalPrompt,
          history: input.useMemory === true ? context : [],
          factPackage: factPackage,
        },
      });
      console.log("--- [DEBUG] TOTAL CONTEXT (chatSynthesis) ---");
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
      console.log("----------------------------------------------");

      const finalResponse = await synthesisPrompt.generate({
        model: CONFIG.CHAT_MODEL,
        input: {
          userRequest: originalPrompt,
          history: input.useMemory === true ? context : [],
          factPackage: factPackage,
        },
      });

      let finalText = finalResponse.text;
      if (!finalText || finalText.trim().length === 0) {
        finalText = `The Logic Engine processed your request, but the Synthesis Layer returned an empty response.`;
      }

      saveKnowledgeGraph(input.source);

      return {
        text: finalText,
        reasoningLogs: reasoningLogs,
      };
    } finally {
      if (CONFIG.USE_LOCAL_MODELS)
        scheduleModelUnload(CONFIG.OLLAMA_CHAT_MODEL_NAME);
    }
  }
);
