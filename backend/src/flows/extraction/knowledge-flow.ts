import { ai } from '../../genkit';
import { z } from "genkit";
import { KnowledgeBaseOutputSchema } from "./schemas";
import { prompt } from "@genkit-ai/dotprompt";
import { safeParseJSON } from "../../logic/json-util";
import { scheduleModelUnload } from "../../services/model-cleanup";
import { CONFIG } from "../../config/constants";

const ExtractionInput = z.object({
  text: z.string(),
  filename: z.string().optional(),
});

/**
 * Native TypeScript implementation of quote verification.
 * Replaces the old SBCL-based symbolic verification.
 */
const verifyQuoteNative = (text: string, quote: string): boolean => {
  if (!quote) return true;
  // Basic optimization: normalize whitespace for comparison
  const normalizedText = text.replace(/\s+/g, " ");
  const normalizedQuote = quote.replace(/\s+/g, " ");
  return normalizedText.includes(normalizedQuote);
};

export const extractKnowledge = ai.defineFlow(
  {
    name: "extractKnowledge",
    inputSchema: ExtractionInput,
    outputSchema: KnowledgeBaseOutputSchema,
  },
  async (input) => {
    // Load Prompt Template
    // @ts-ignore
    const extractionPrompt = await prompt(ai.registry, "knowledgeExtraction");

    let attempts = 0;
    const maxAttempts = 3;
    let feedback = "";

    while (attempts < maxAttempts) {
      console.log(`Extraction Attempt ${attempts + 1}`);

      const inputVars = {
        text: input.text,
        feedback: feedback || null,
      };

      // Render total context for observability
      const rendered = await extractionPrompt.render({ input: inputVars });
      console.log(
        `--- [DEBUG] TOTAL CONTEXT (knowledgeExtraction - Attempt ${
          attempts + 1
        }) ---`
      );
      const promptText =
        rendered.messages
          ?.map((m) =>
            m.content?.map((p) => p.text || JSON.stringify(p)).join("")
          )
          .join("\n---\n") || "No message content";
      console.log(promptText);
      console.log(
        "-------------------------------------------------------------------------"
      );

      const response = await extractionPrompt.generate({
        input: inputVars,
      });

      let knowledge;
      try {
        knowledge = response.output || safeParseJSON(response.text);
      } catch (e) {
        console.warn("JSON Parse Error, attempting manual sanitization recovery:", e);
        knowledge = safeParseJSON(response.text);
        
        if (!knowledge) {
          feedback = `Your previous response was not valid JSON. Error: ${
            e instanceof Error ? e.message : String(e)
          }. \n\nEnsure you return ONLY valid JSON.`;
          attempts++;
          continue;
        }
      }

      if (!knowledge) {
        throw new Error("No output from model");
      }

      console.log(
        `Extracted ${
          (knowledge as any)?.knowledgeBase?.length
        } concepts. Validating...`
      );

      let allValid = true;
      let newFeedbackItems: string[] = [];

      const concepts = (knowledge as any).knowledgeBase || [];

      for (const concept of concepts) {
        for (const nugget of concept.knowledgeNuggets) {
          if (nugget.source_quote) {
            const isValid = verifyQuoteNative(input.text, nugget.source_quote);
            if (!isValid) {
              allValid = false;
              newFeedbackItems.push(
                `Quote not found: "${nugget.source_quote}". MUST exist verbatim.`
              );
            }
          }
        }
      }

      if (allValid) {
        return knowledge;
      }

      feedback = newFeedbackItems.slice(0, 5).join("\n");
      attempts++;
    }

    try {
      throw new Error(
        `Failed to extract valid knowledge after ${maxAttempts} attempts. Feedback: ${feedback}`
      );
    } finally {
      if (CONFIG.USE_LOCAL_MODELS)
        scheduleModelUnload(CONFIG.OLLAMA_LISP_MODEL_NAME);
    }
  }
);
