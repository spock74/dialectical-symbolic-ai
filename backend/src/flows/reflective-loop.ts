// Reflective Loop with SBCL integration


import { z } from 'genkit'; // 'zod' is exported as 'z' from genkit usually, or just use 'zod' package.
// Re-reading docs (memory): genkit 1.x exports z.

import { SbclProcess } from '../lisp/sbcl-process';
import { ai } from '../genkit';
import { prompt } from '@genkit-ai/dotprompt';

// Initialize SBCL process
export const lisp = new SbclProcess();

const InputSchema = z.object({
  prompt: z.string(),
  history: z.array(z.any()).optional(),
});

export const reflectiveLoop = ai.defineFlow(
  {
    name: 'reflectiveLoop',
    inputSchema: InputSchema,
  },
  async (input) => {
    let context = input.history || [];
    let currentPrompt = input.prompt;
    const maxTurns = 5;
    let turn = 0;
    let finalOutput = "";

    while (turn < maxTurns) {
      console.log(`--- Turn ${turn} ---`);
      
      // @ts-ignore
      const refPrompt = await prompt(ai.registry, 'reflectiveLoop');
      
      const llmResponse = await refPrompt.generate({
        input: {
            userRequest: currentPrompt,
            context: JSON.stringify(context)
        }
      });

      const responseText = llmResponse.text; // .text() or .text getter? usually .text in 1.x
      console.log("LLM:", responseText);
      finalOutput = responseText;

      // Improved regex to capture lisp, cl, common-lisp blocks case-insensitively
      const lispRegex = /```(?:lisp|cl|common-lisp)\b\s*([\s\S]*?)```/gi;
      let match;
      let executedSomething = false;

      // Reset regex state just in case (though it's re-created each loop)
      lispRegex.lastIndex = 0;

      if ((match = lispRegex.exec(responseText)) !== null) {
        const code = match[1];
        console.log("Executing Lisp:", code);
        lisp.emit('log', `;; Found Lisp Code Block. Executing...`);
        try {
          const result = await lisp.eval(code);
          console.log("Result:", result);
          context.push({ role: 'model', content: responseText });
          context.push({ role: 'tool', content: `Lisp Result: ${result}` });
          currentPrompt = "Continue reasoning based on the Lisp result.";
          executedSomething = true;
        } catch (e) {
          console.error("Lisp Error:", e);
          const errorMsg = String(e);
          lisp.emit('log', `;; Lisp Error: ${errorMsg}`);
          context.push({ role: 'model', content: responseText });
          context.push({ role: 'tool', content: `Error: ${errorMsg}` });
          executedSomething = true;
        }
      } else {
        lisp.emit('log', `;; No Lisp code block detected in response.`);
      }

      if (!executedSomething) {
        break;
      }
      turn++;
    }
    return finalOutput;
  }
);
