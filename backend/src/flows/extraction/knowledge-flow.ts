import { ai } from '../../genkit';
import { z } from 'genkit';
import { SbclProcess } from '../../lisp/sbcl-process';
import { KnowledgeBaseOutputSchema } from './schemas';
import crypto from 'crypto';
import { prompt } from '@genkit-ai/dotprompt';
import { scheduleModelUnload } from "../../services/model-cleanup";
import { CONFIG } from "../../config/constants";

// Singleton SBCL for now. In prod, maybe one per request or a pool.
const lisp = SbclProcess.getInstance();

const ExtractionInput = z.object({
  text: z.string(),
  filename: z.string().optional(),
});

export const extractKnowledge = ai.defineFlow(
  {
    name: 'extractKnowledge',
    inputSchema: ExtractionInput,
    outputSchema: KnowledgeBaseOutputSchema,
  },
  async (input) => {
    // 1. Load text into Lisp
    // specific variable name to avoid collisions if we had concurrency (we don't yet)
    const textVar = `*doc-${Date.now()}*`;
    await lisp.loadText(textVar, input.text);
    
    // Load Prompt Template
    // @ts-ignore
    const extractionPrompt = await prompt(ai.registry, 'knowledgeExtraction');

    let attempts = 0;
    const maxAttempts = 3;
    let feedback = "";
    
    while (attempts < maxAttempts) {
      console.log(`Extraction Attempt ${attempts + 1}`);

      const response = await extractionPrompt.generate({
        input: {
            text: input.text,
            feedback: feedback || null
        }
      });

      let knowledge;
      try {
        knowledge = response.output; 
      } catch (e) {
        console.warn("JSON Parse Error:", e);
        const rawText = response.text; 
        console.log("Raw Output:", rawText);
        
        feedback = `Your previous response was not valid JSON. Error: ${e instanceof Error ? e.message : String(e)}. \n\nEnsure you return ONLY valid JSON inside \`\`\`json blocks if necessary, or just plain JSON.`;
        attempts++;
        continue;
      }

      if (!knowledge) {
         throw new Error("No output from model");
      }
      // Actually Dotprompt response.output might be the raw object if we specified schema in prompt file OR flow.
      // Since schema is in flow, Genkit validates it.
      // But prompt file didn't specify output schema in code, so response.output is unvalidated unless flow/prompt enforces it.
      // However, ai.generate output is the data.

      console.log(`Extracted ${knowledge?.knowledgeBase?.length} concepts. Validating...`);
      
      // 4. Validate with Lisp (Quotes)
      let allValid = true;
      let newFeedbackItems: string[] = [];

      // We need to cast knowledge to expected type or trust it
      const concepts = (knowledge as any).knowledgeBase || [];

      for (const concept of concepts) {
         for (const nugget of concept.knowledgeNuggets) {
            if (nugget.source_quote) {
               const isValid = await lisp.verifyQuote(textVar, nugget.source_quote);
               if (!isValid) {
                  allValid = false;
                  newFeedbackItems.push(`Quote not found: "${nugget.source_quote}". MUST exist verbatim.`);
               }
            }
         }
      }

      if (allValid) {
        return knowledge;
      }

      feedback = newFeedbackItems.slice(0, 5).join("\n"); // Limit feedback length
      attempts++;
    }

    try {
      throw new Error(
        `Failed to extract valid knowledge after ${maxAttempts} attempts. Feedback: ${feedback}`
      );
    } finally {
      scheduleModelUnload(CONFIG.OLLAMA_LISP_MODEL_NAME);
    }
  }
);
