import { spawn } from 'child_process';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

export class ImageService {
  /**
   * Converts a PDF buffer to an array of Base64 encoded PNG images.
   * Limits to specific number of pages to avoid context overflow.
   */
  async convertPdfToImages(pdfBuffer: Buffer, maxPages: number = 5): Promise<string[]> {
    const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'neurolisp-pdf-'));
    const pdfPath = path.join(tempDir, 'input.pdf');
    const outputPathPrefix = path.join(tempDir, 'page');

    try {
      // 1. Write PDF to temp file
      await fs.writeFile(pdfPath, pdfBuffer);

      // 2. Run pdftoppm
      // pdftoppm -png -r 72 -l <maxPages> input.pdf prefix (Reduced DPI for memory safety)
      await new Promise<void>((resolve, reject) => {
        const process = spawn('pdftoppm', ['-png', '-r', '72', '-l', maxPages.toString(), pdfPath, outputPathPrefix]);
        
        // Timeout protection (e.g., 30 seconds)
        const timeout = setTimeout(() => {
            process.kill();
            reject(new Error('pdftoppm timed out after 30s'));
        }, 30000);

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
      const files = await fs.readdir(tempDir);
      const imageFiles = files.filter(f => f.startsWith('page-') && f.endsWith('.png')).sort();
      
      const images: string[] = [];
      for (const file of imageFiles) {
        const filePath = path.join(tempDir, file);
        const buffer = await fs.readFile(filePath);
        images.push(buffer.toString('base64'));
      }

      return images;
    } finally {
      // Cleanup
      await fs.rm(tempDir, { recursive: true, force: true });
    }
  }
}

export const imageService = new ImageService();
