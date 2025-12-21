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
      pdfBase64: z.string().optional(),
      filePath: z.string().optional(),
    }),
    outputSchema: KnowledgeBaseOutputSchema,
  },
  async ({ pdfBase64, filePath }) => {
    // 1. Convert PDF -> Images
    // We limit to 1 page for now to fit in context window and prevent Node OOM (Exit 137)
    let images: string[] = [];

    if (filePath) {
        console.log(`Processing PDF from disk: ${filePath}`);
        images = await imageService.convertPdfToImages(filePath, 1);
    } else if (pdfBase64) {
        console.log(`Processing PDF from memory buffer`);
        const buffer = Buffer.from(pdfBase64, 'base64');
        images = await imageService.convertPdfToImages(buffer, 1);
    } else {
        throw new Error("Either pdfBase64 or filePath must be provided");
    }

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
