import { ai } from '../../genkit';
import { z } from 'genkit';
import { KnowledgeBaseOutputSchema } from './schemas';
import { imageService } from '../../services/image-service';

import { retry } from 'genkit/model/middleware';
import { prompt } from '@genkit-ai/dotprompt';

export const extractKnowledgeMultimodal = ai.defineFlow(
  {
    name: 'extractKnowledgeMultimodal',
    inputSchema: z.object({
      pdfBase64: z.string(),
    }),
    outputSchema: KnowledgeBaseOutputSchema,
  },
  async ({ pdfBase64 }) => {
    // 1. Convert PDF Base64 -> Buffer -> Images (PNG Base64)
    // We limit to 1 page for now to fit in context window and prevent Node OOM (Exit 137)
    const buffer = Buffer.from(pdfBase64, 'base64');
    const images = await imageService.convertPdfToImages(buffer, 1);

    console.log(`Converted PDF to ${images.length} images for visual processing.`);

    // @ts-ignore
    const multimodalPrompt = await prompt(ai.registry, 'knowledgeExtractionMultimodal');

    // 3. Call Model via Prompt
    const response = await multimodalPrompt.generate({
      input: {
        images: images.map(img => ({
          url: `data:image/png;base64,${img}`,
          contentType: 'image/png'
        }))
      }
    });

    if (!response.output) {
      throw new Error("Failed to generate knowledge from PDF images.");
    }

    return response.output;
  }
);
