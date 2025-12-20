import { ai } from '../../genkit';
import { z } from 'genkit';
import { extractKnowledge } from './knowledge-flow';
import { KnowledgeBaseOutputSchema } from './schemas';

const MarkdownInput = z.object({
  text: z.string(),
  filename: z.string().optional(),
});

export const extractMarkdown = ai.defineFlow(
  {
    name: 'extractMarkdown',
    inputSchema: MarkdownInput,
    outputSchema: KnowledgeBaseOutputSchema,
  },
  async (input) => {
    console.log(`Processing Markdown extraction for ${input.filename || 'unknown file'}`);

    // Preprocessing: Remove image references to avoid confusing the model or wasting context
    // Matches ![alt text](url "optional title")
    const imageRegex = /!\[.*?\]\(.*?\)/g;
    const cleanedText = input.text.replace(imageRegex, '[IMAGE REMOVED]');

    // We could add more specific markdown cleanup here if needed.
    // E.g. removing specific incompatible HTML tags if they exist.

    // Delegate to the main knowledge extraction flow
    // This reuses the prompt, validation loop, and Lisp verification.
    return await extractKnowledge({
      text: cleanedText,
      filename: input.filename
    });
  }
);
