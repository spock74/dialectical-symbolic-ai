import { KnowledgeGraph } from './graph-engine';
import { jsclAdapter } from './jscl-adapter';
import { kernelEvents } from './kernel-events';

/**
 * LispInterpreter: A wrapper around the JSCL native Lisp engine.
 * Passo 3: O Interpretador 'JSCL-Symbolic'
 */
export class LispInterpreter {
  /**
   * Executes a Lisp-style command using the JSCL native runtime.
   */
  execute(input: string, graph: KnowledgeGraph): string {
    // Robust regex to extract top-level parentheses (S-Expressions)
    const sExpressions = input.match(/\((?:[^)(]+|\((?:[^)(]+|\([^)(]*\))*\))*\)/g);
    
    if (!sExpressions) return "Erro: Nenhum comando Lisp detectado.";

    const outputs: string[] = [];

    for (const expr of sExpressions) {
      kernelEvents.emit('log', `> ${expr}`);
      try {
        // Delegate execution to JSCL
        const result = jsclAdapter.eval(expr, graph);
        
        let output = "";
        // Format result: if it's a JS object from our bridge, we might want a string
        if (result === undefined || result === null) {
          output = `OK: ${expr}`;
        } else if (Array.isArray(result)) {
           output = result.join('\n');
        } else {
           output = String(result);
        }
        outputs.push(output);
        kernelEvents.emit('log', output);
      } catch (error: any) {
        const errMsg = `Erro de Lisp Nativo: ${error.message}`;
        outputs.push(errMsg);
        kernelEvents.emit('log', errMsg);
      }
    }

    return outputs.join('\n');
  }
}

export const lispInterpreter = new LispInterpreter();


