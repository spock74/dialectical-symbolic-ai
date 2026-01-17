/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

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
    const inputVars = {
      image: {
        url: `data:image/png;base64,${images[0]}`,
        contentType: "image/png",
      },
    };

    // Render total context for observability
    const rendered = await transcriptionPrompt.render({ input: inputVars });
    const promptText =
      rendered.messages
        ?.map((m) =>
          m.content
            ?.map((p) => p.text || (p.media ? "[Media]" : JSON.stringify(p)))
            .join("")
        )
        .join("\n---\n") || "No message content";
    const cleanLog = promptText.replace(
      /data:image\/png;base64,[^"\s]+/g,
      "[BASE64_IMAGE_DATA]"
    );
    console.log("--- [DEBUG] TOTAL CONTEXT (simpleTranscription) ---");
    console.log(cleanLog);
    console.log("--------------------------------------------------");

    const response = await transcriptionPrompt.generate({
      model: CONFIG.VISION_MODEL,
      input: inputVars,
    });

    if (!response.output) {
      throw new Error("Failed to transcribe image.");
    }

    try {
      return response.output;
    } finally {
      if (CONFIG.USE_LOCAL_MODELS)
        scheduleModelUnload(CONFIG.OLLAMA_VISION_MODEL_NAME);
    }
  }
);
