import { ai } from '../../genkit';
import { prompt } from '@genkit-ai/dotprompt';
import { z } from 'genkit';
import { imageService } from '../../services/image-service';
import { scheduleModelUnload } from "../../services/model-cleanup";
import { CONFIG } from "../../config/constants";

export const extractSimpleTranscription = ai.defineFlow(
  {
    name: 'extractSimpleTranscription',
    inputSchema: z.object({
      pdfBase64: z.string(),
    }),
    outputSchema: z.object({
      transcription: z.string(),
    }),
  },
  async ({ pdfBase64 }) => {
    console.log("Starting simple transcription flow...");
    
    // 1. Convert PDF -> 1 Image (Page 1 only)
    const buffer = Buffer.from(pdfBase64, 'base64');
    const images = await imageService.convertPdfToImages(buffer, 1);
    
    if (images.length === 0) {
      throw new Error("No images generated from PDF");
    }

    console.log(`Processing 1 page visually...`);

    // 2. Load Prompt
    // @ts-ignore
    const transcriptionPrompt = await prompt(ai.registry, 'simpleTranscription');

    // 2. Call Model
    const response = await transcriptionPrompt.generate({
      input: {
        image: {
            url: `data:image/png;base64,${images[0]}`,
            contentType: 'image/png'
        }
      }
    });

    if (!response.output) {
      throw new Error("Failed to transcribe image.");
    }

    try {
      return response.output;
    } finally {
      scheduleModelUnload(CONFIG.OLLAMA_LISP_MODEL_NAME);
    }
  }
);
