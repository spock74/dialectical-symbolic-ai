import { ai } from '../../genkit';
import { z } from "genkit";
import { ExtractionOutputSchema } from "./schemas";
import { prompt } from "@genkit-ai/dotprompt";
import { scheduleModelUnload } from "../../services/model-cleanup";
import { CONFIG } from "../../config/constants";

const ExtractionInput = z.object({
  text: z.string(),
  filename: z.string().optional(),
});

const REGEX_RELATION = /\(adicionar-relacao\s+"([^"]+)"\s+"([^"]+)"\s+"([^"]+)"\s+:category\s+:([A-Z]+)\)/g;
const REGEX_RULE = /\(adicionar-regra\s+'([^\s\n]+)\s+'(\([\s\S]*?\))\s+'(\([\s\S]*?\))\)/g;

export const extractKnowledge = ai.defineFlow(
  {
    name: "extractKnowledge",
    inputSchema: ExtractionInput,
    outputSchema: ExtractionOutputSchema,
  },
  async (input) => {
    // Load Prompt Template
    // @ts-ignore
    const extractionPrompt = await prompt(ai.registry, "knowledgeExtraction");

    console.log(`[Flow] Performing Text-based Knowledge Extraction...`);

    const inputVars = {
      text: input.text,
      feedback: null,
    };

    const response = await extractionPrompt.generate({
      model: CONFIG.LISP_MODEL,
      input: inputVars,
    });

    const rawText = response.text;
    const relations: any[] = [];
    const rules: any[] = [];
    let match;

    // Extract Relations
    while ((match = REGEX_RELATION.exec(rawText)) !== null) {
      relations.push({
        source: match[1],
        label: match[2],
        target: match[3],
        category: match[4]
      });
    }

    // Extract Rules
    while ((match = REGEX_RULE.exec(rawText)) !== null) {
      // Clean up the conditions block and split into individual clauses if it's ((c1)(c2))
      const rawConditions = match[2].trim();
      // Simple heuristic to extract clauses: look for inner parens if they exist, otherwise treat as one
      const conditions = rawConditions.startsWith("((") 
        ? rawConditions.slice(2, -2).split(/\)\s*\(/).map(c => `(${c.trim()})`)
        : [rawConditions];

      rules.push({
        name: match[1],
        conditions: conditions,
        consequence: match[3].trim()
      });
    }

    console.log(`[Flow] Text Extraction complete: ${relations.length} relations, ${rules.length} rules.`);

    return {
      relations,
      rules,
      lisp_raw: rawText
    };
  }
);
