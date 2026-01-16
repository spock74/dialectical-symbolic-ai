/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 */

import { spawn } from 'child_process';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

export class ImageService {
  /**
   * Converts a PDF buffer to an array of Base64 encoded PNG images.
   * @param options Object containing page range options.
   * @param returnPaths If true, returns absolute file paths instead of base64 and DOES NOT clean up.
   */
  async convertPdfToImages(
      input: Buffer | string, 
      options: { maxPages?: number; firstPage?: number; lastPage?: number } | number = 5, 
      returnPaths: boolean = false
  ): Promise<string[]> {
    // Evaluate options
    let maxPages = 5;
    let firstPage = 1;
    let lastPage: number | undefined;

    if (typeof options === 'number') {
        maxPages = options;
    } else {
        if (options.maxPages) maxPages = options.maxPages;
        if (options.firstPage) firstPage = options.firstPage;
        if (options.lastPage) lastPage = options.lastPage;
    }

    // If input is a path (string), use it. If buffer, write to temp.
    let tempDir: string | null = null;
    let pdfPath: string;

    if (Buffer.isBuffer(input)) {
        tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'neurolisp-pdf-'));
        pdfPath = path.join(tempDir, 'input.pdf');
        await fs.writeFile(pdfPath, input);
    } else {
        pdfPath = input; // Input is already a file path
    }
    
    // We still need a temp dir for outputting images even if input is path
    const outputDir = await fs.mkdtemp(path.join(os.tmpdir(), 'neurolisp-img-'));
    const outputPathPrefix = path.join(outputDir, 'page');

    try {
      // 2. Run pdftoppm
      // Construct args: -png -r 72
      const args = ['-png', '-r', '72'];
      
      if (firstPage) args.push('-f', firstPage.toString());
      
      // -l is last page to print. If user specified lastPage, use it.
      // If not, use maxPages logic if firstPage is default.
      // E.g. default: first=1, max=5 => last=5.
      // Usage: first=2, last=2 => explicit range.
      if (lastPage) {
          args.push('-l', lastPage.toString());
      } else if (typeof options === 'number' || (options as any).maxPages) {
          // If using maxPages logic (legacy)
           args.push('-l', (firstPage + maxPages - 1).toString());
      }

      args.push(pdfPath, outputPathPrefix);

      await new Promise<void>((resolve, reject) => {
        const process = spawn('pdftoppm', args);
        
        // Timeout protection (e.g., 60 seconds for heavy files)
        const timeout = setTimeout(() => {
            process.kill();
            reject(new Error('pdftoppm timed out after 60s'));
        }, 60000);

        let stderr = '';
        if (process.stderr) {
            process.stderr.on('data', (data) => stderr += data.toString());
        }

        process.on('close', (code) => {
          clearTimeout(timeout);
          if (code === 0) resolve();
          else reject(new Error(`pdftoppm exited with code ${code}. Stderr: ${stderr.trim()}`));
        });
        
        process.on('error', (err) => {
            clearTimeout(timeout);
            if ((err as any).code === 'ENOENT') {
                reject(new Error('pdftoppm not found. Please install poppler.'));
            } else {
                reject(err);
            }
        });
      });

      // 3. Read generated images
      const files = await fs.readdir(outputDir);
      const imageFiles = files.filter(f => f.startsWith('page-') && f.endsWith('.png')).sort();
      
      if (returnPaths) {
          // Return absolute paths
          return imageFiles.map(f => path.join(outputDir, f));
      }

      const images: string[] = [];
      for (const file of imageFiles) {
        const filePath = path.join(outputDir, file);
        const buffer = await fs.readFile(filePath);
        images.push(buffer.toString('base64'));
      }

      return images;
    } finally {
      // Cleanup
      if (tempDir) await fs.rm(tempDir, { recursive: true, force: true }).catch(console.error);
      // Only delete outputDir if we are NOT returning paths
      if (!returnPaths) {
        await fs.rm(outputDir, { recursive: true, force: true }).catch(console.error);
      }
    }
  }
  
  /**
   * Helper to clean up image directories returned by convertPdfToImages(..., true)
   */
  async cleanup(paths: string[]) {
      if (!paths || paths.length === 0) return;
      try {
        const dir = path.dirname(paths[0]);
        // Safety check: ensure we are deleting a neurolisp-img temp dir
        if (dir.includes('neurolisp-img-')) {
            await fs.rm(dir, { recursive: true, force: true });
        }
      } catch (e) {
          console.error("Error cleaning up image temp dir:", e);
      }
  }
}

export const imageService = new ImageService();
