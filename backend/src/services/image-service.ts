import { spawn } from 'child_process';
import fs from 'fs/promises';
import path from 'path';
import os from 'os';

export class ImageService {
  /**
   * Converts a PDF buffer to an array of Base64 encoded PNG images.
   * Limits to specific number of pages to avoid context overflow.
   */
  async convertPdfToImages(input: Buffer | string, maxPages: number = 5): Promise<string[]> {
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
      // pdftoppm -png -r 72 -l <maxPages> input.pdf prefix
      await new Promise<void>((resolve, reject) => {
        const process = spawn('pdftoppm', ['-png', '-r', '72', '-l', maxPages.toString(), pdfPath, outputPathPrefix]);
        
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
      await fs.rm(outputDir, { recursive: true, force: true }).catch(console.error);
    }
  }
}

export const imageService = new ImageService();
