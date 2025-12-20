import { imageService } from './src/services/image-service';
import fs from 'fs/promises';
import path from 'path';

async function testPdfConversion() {
  const pdfPath = '/Users/moraes/Documents/PROJETOS/local-gemma3-4b-genkit/labs/neurolisp/pdf-sources/Digoxin_and_30-Day_All-Cause_Hospital_Admission_in_Older_Patients_with_Chronic_Diastolic_Heart_Failure.pdf';
  
  try {
    console.log(`Reading PDF from: ${pdfPath}`);
    const pdfBuffer = await fs.readFile(pdfPath);
    console.log(`PDF Size: ${pdfBuffer.length} bytes`);

    console.log("Converting PDF to images (max 5 pages)...");
    const images = await imageService.convertPdfToImages(pdfBuffer, 5);

    console.log(`\nSuccess! Converted ${images.length} pages.`);
    
    // Verify content by saving the first page back to disk for manual inspection if needed
    if (images.length > 0) {
        const testOut = 'test_output_page1.png';
        await fs.writeFile(testOut, Buffer.from(images[0], 'base64'));
        console.log(`Saved first page to ${testOut} for verification.`);
    }

  } catch (error) {
    console.error("Error executing PDF conversion test:", error);
  }
}

testPdfConversion();
