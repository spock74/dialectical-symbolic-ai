import { ai } from './src/genkit';
import fs from 'fs/promises';
import path from 'path';

async function describeImage() {
  const imagePath = '...........[give ot a right path]...........09.png';
  
  try {
    const imageBuffer = await fs.readFile(imagePath);
    const base64Image = imageBuffer.toString('base64');
    const contentType = 'image/png'; // Assuming png based on file extension

    console.log(`Analyzing image: ${imagePath}...`);

    const response = await ai.generate({
      model: 'ollama/gemma3:4b', 
      prompt: [
        { text: "Identify the main objects in this photo and describe their relationship." },
        {
          media: {
            url: `data:${contentType};base64,${base64Image}`,
            contentType: contentType,
          },
        },
      ],
    });

    console.log("\n--- Model Response ---");
    console.log(response.text);
    console.log("----------------------\n");

  } catch (error) {
    console.error("Error executing vision test:", error);
  }
}

describeImage();
