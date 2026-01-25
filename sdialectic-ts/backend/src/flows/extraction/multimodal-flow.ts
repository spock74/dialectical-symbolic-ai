/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

import { ai } from '../../genkit';
import { z } from 'genkit';
import { ExtractionOutputSchema } from './schemas';
import { CONFIG } from "../../config/constants";
import { prompt } from "@genkit-ai/dotprompt";
import { GoogleGenAI, FileState } from "@google/genai";
import { scheduleModelUnload } from "../../services/model-cleanup";
import { imageService } from "../../services/image-service";

// Initialize Google Gen AI client with developer key
const genAI = new GoogleGenAI({ apiKey: CONFIG.GEMINI_API_KEY });

const REGEX_RELATION = /\(adicionar-relacao\s+"([^"]+)"\s+"([^"]+)"\s+"([^"]+)"\s+:category\s+:([a-zA-Z0-9_\-]+)\)/g;
const REGEX_RULE = /\(adicionar-regra\s+'([^\s\n]+)\s+'(\([\s\S]*?\))\s+'(\([\s\S]*?\))\)/g;

export const extractKnowledgeMultimodal = ai.defineFlow(
  {
    name: "extractKnowledgeMultimodal",
    inputSchema: z.object({
      filePath: z.string(), // Disk path for streaming upload
      trackId: z.string().optional(),
    }),
    outputSchema: ExtractionOutputSchema,
  },
  async ({ filePath, trackId }) => {
    console.log(`[Flow] Starting Multimodal PDF Extraction for Track: ${trackId} | Mode: ${CONFIG.IS_LOCAL_VISION ? 'LOCAL' : 'CLOUD'}`);
    let uploadedFile: any;
    let promptInput: any = { trackId: trackId || "default" };

    try {

      // === BRANCH: LOCAL vs CLOUD ===
      if (CONFIG.IS_LOCAL_VISION) {
          console.log(`[Flow] Local mode detected. Converting PDF to images for ${CONFIG.VISION_MODEL}...`);
          
          // 1. Convert PDF to Images (Up to 20 pages), RETURN RAW BASE64 (false)
          // imageService returns raw base64 strings without prefix.
          // LOG: Confirming page range
          console.log(`[Flow] requesting up to 20 pages from PDF...`);
          const base64Images = await imageService.convertPdfToImages(filePath, { maxPages: 20 }, false);
          
          if (base64Images.length === 0) {
              throw new Error("PDF conversion yielded no images.");
          }
           console.log(`[Flow] Converted PDF to ${base64Images.length} images (Base64). Formatting as Data URI...`);

          // 2. Format as Data URIs for Genkit
          // Genkit/Dotprompt/Ollama plugin generally supports data: URIs for media.
          // test_vision.ts confirms this pattern works.
          promptInput.images = base64Images.map(b64 => `data:image/png;base64,${b64}`);

      } else {
          // === CLOUD (Google GenAI) ===
          // 1. NATIVE BINARY UPLOAD (File API)
          uploadedFile = await genAI.files.upload({
            file: filePath,
            config: {
              mimeType: "application/pdf",
              displayName: `StudyTrack_${trackId || 'unknown'}_${Date.now()}`
            }
          });

          console.log(`[Flow] Document uploaded to Gemini Cache. Name: ${uploadedFile.name} | URI: ${uploadedFile.uri}`);
          
          const fileName = uploadedFile.name;
          if (!fileName) throw new Error("[Flow] Upload failed: Gemini returned no file name.");

          // 2. POLLING: Wait for PDF processing (ACTIVE state)
          let fileStatus = await genAI.files.get({ name: fileName });
          while (fileStatus.state === FileState.PROCESSING) {
            console.log(`[Flow] File state: ${fileStatus.state}. Waiting 3s...`);
            await new Promise((resolve) => setTimeout(resolve, 3000));
            fileStatus = await genAI.files.get({ name: fileName });
          }

          if (fileStatus.state === FileState.FAILED) {
            throw new Error(`[Flow] Gemini PDF Processing failed for file: ${fileName}`);
          }

          console.log(`[Flow] PDF is now ACTIVE. Triggering extraction...`);
          promptInput.pdfFile = uploadedFile.uri;
      }

      // 3. GENERATION
      // @ts-ignore
      const multimodalPrompt = await prompt(ai.registry as any, "knowledgeExtractionMultimodal");

      const response = await multimodalPrompt.generate({
        model: CONFIG.VISION_MODEL,
        input: promptInput,
      });

      const rawText = response.text;
      console.log(`[Flow] Extraction complete. Raw output length: ${rawText.length}`);
      console.log(`[DEBUG] RAW MODEL OUTPUT:\n${rawText}\n[DEBUG] END RAW OUTPUT`);


      // 4. PARSING (Strict Lisp Structures)
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

      console.log(`[Flow] Sucessfully parsed ${relations.length} relations and ${rules.length} rules.`);

      return {
        relations,
        rules,
        lisp_raw: rawText
      };

    } catch (error) {
      console.error("[Flow] Multimodal Flow Error:", error);
      throw error;
    } finally {
      // 5. CLEANUP: Delete remote file to avoid clutter/costs
      if (uploadedFile?.name) {
        try {
          await genAI.files.delete({ name: uploadedFile.name });
          console.log(`[Flow] Remote file cleanup successful: ${uploadedFile.name}`);
        } catch (cleanupError) {
          console.warn(`[Flow] Failed to cleanup remote file: ${uploadedFile.name}`, cleanupError);
        }
      }

      // Local model maintenance
      if (CONFIG.IS_LOCAL_VISION) {
        scheduleModelUnload(CONFIG.RAW_VISION_MODEL_NAME);
      }
    }
  }
);
