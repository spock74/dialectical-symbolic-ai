/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

const pdf = require('pdf-parse');

export class PdfService {
  /**
   * Extracts raw text from a PDF buffer.
   * @param buffer The PDF file buffer.
   * @returns The extracted text content.
   */
  async parsePdf(buffer: Buffer): Promise<string> {
    try {
      const data = await pdf(buffer);
      // Clean the text: replace newlines with spaces and collapse multiple spaces
      // This is crucial because PDFParse often leaves line breaks mid-sentence,
      // which causes exact quote verification in Lisp/LLM to fail.
      const cleanText = data.text.replace(/\n/g, ' ').replace(/\s+/g, ' ').trim();
      return cleanText;
    } catch (error) {
      console.error("Error parsing PDF:", error);
      throw new Error("Failed to parse PDF document.");
    }
  }
}

export const pdfService = new PdfService();
