import { SbclProcess } from '../lisp/sbcl-process';
import path from 'path';
import fs from 'fs';

async function testMetaprogramming() {
  const lisp = SbclProcess.getInstance();
  const testKb = path.resolve(process.cwd(), 'test_meta.lisp');
  
  if (fs.existsSync(testKb)) fs.unlinkSync(testKb);

  try {
    console.log("--- Initializing Lisp ---");
    lisp.on('log', (msg) => {
      if (msg.includes('STYLE-WARNING') || msg.includes('Error')) {
        console.log(`[SBCL LOG]: ${msg.trim()}`);
      }
    });

    // Wait a bit for SBCL to start and load bootstrap
    await new Promise(r => setTimeout(r, 5000));
    
    console.log("--- Defining a Custom Function ---");
    const funcDef = '(definir-funcao TEST-FUNC (x) (format nil "HELLO-~a" x))';
    const res1 = await lisp.eval(funcDef);
    console.log("Result 1:", res1.result);

    console.log("--- Calling Custom Function ---");
    const res2 = await lisp.eval('(TEST-FUNC "WORLD")');
    console.log("Result 2:", res2.result);

    console.log("--- Defining a Custom Macro ---");
    const macroDef = '(definir-macro TEST-MACRO (var val) `(adicionar-memoria ,var ,val))';
    const res3 = await lisp.eval(macroDef);
    console.log("Result:", res3.result);

    console.log("--- Calling Custom Macro ---");
    const res4 = await lisp.eval('(TEST-MACRO "META-KEY" "META-VAL")');
    console.log("Result:", res4.result);

    console.log("--- Saving State ---");
    await lisp.eval(`(salvar-estado "${testKb}")`);

    console.log("--- Restarting and Loading State ---");
    // In a real singleton, we don't 'restart' easily without killing the process
    // For the test, we just clear memory and load
    await lisp.eval('(limpar-memoria)');
    await lisp.eval(`(carregar-estado "${testKb}")`);

    console.log("--- Verifying Function Persistence ---");
    const res5 = await lisp.eval('(TEST-FUNC "AGAIN")');
    console.log("Result:", res5.result);

    console.log("--- Verifying Macro Persistence ---");
    const res6 = await lisp.eval('(TEST-MACRO "PERSIST-KEY" "PERSIST-VAL")');
    console.log("Result:", res6.result);

    console.log("--- Verifying Data from Macro ---");
    const res7 = await lisp.eval('(recuperar-memoria "PERSIST-KEY")');
    console.log("Result:", res7.result);

  } catch (e) {
    console.error("Test Failed:", e);
  } finally {
    // if (fs.existsSync(testKb)) fs.unlinkSync(testKb);
  }
}

testMetaprogramming();
