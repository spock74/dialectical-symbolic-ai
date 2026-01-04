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

    // Parse S-Expression output manually since it's not JSON
    const parseLispRelations = (text: string) => {
      const regex = /\(make-relation\s+:source\s+"([^"]+)"\s+:target\s+"([^"]+)"\s+:label\s+"([^"]+)"\s+:category\s+:([A-Z]+)\)/g;
      const relations = [];
      let match;
      while ((match = regex.exec(text)) !== null) {
        relations.push({
          source: match[1],
          target: match[2],
          label: match[3],
          category: match[4]
        });
      }
      return relations;
    };

    const response = await extractionPrompt.generate({
      model: CONFIG.LISP_MODEL,
      input: inputVars,
    });

    try {
        const relations = parseLispRelations(response.text);
        if (relations.length === 0) {
             console.warn("No relations parsed via Regex. Raw text:", response.text);
             // Fallback or retry logic could go here
        }

        // Transform to legacy structure for compatibility if needed, OR just return raw relations
        // For now, let's map it to a structure that fits the KnowledgeBase expectation slightly,
        // or just return the relations directly if the caller handles it.
        // Assuming the caller expects { knowledgeBase: [...] }
        
        // Construct minimal concepts from relations to satisfy schema if strict,
        // but typically we just want the edges now.
        const conceptsMap = new Map<string, any>();
        
        relations.forEach(r => {
            if (!conceptsMap.has(r.source)) conceptsMap.set(r.source, { concept: r.source, definition: "Extracted Entity", knowledgeNuggets: [] });
            if (!conceptsMap.has(r.target)) conceptsMap.set(r.target, { concept: r.target, definition: "Extracted Entity", knowledgeNuggets: [] });
        });

        return {
            knowledgeBase: Array.from(conceptsMap.values()),
            relations: relations 
        };

    } catch (e) {
      console.error("Error parsing Lisp response:", e);
      throw e;
    }
    }
  }
);
