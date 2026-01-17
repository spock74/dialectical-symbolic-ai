import { describe, it, expect, vi, beforeAll, afterAll } from 'vitest';
import { lispInterpreter } from '../logic/lisp-interpreter';
import { ReflectiveOrchestrator } from '../services/reflective-orchestrator';
import { embeddingService } from '../services/embedding-service';
import { kernelEvents } from '../logic/kernel-events';
import { CONFIG } from '../config/constants';

// We mock the embedding service for consistent results
vi.mock('../services/embedding-service', async () => {
    return {
        embeddingService: {
            getEmbedding: vi.fn(async (text: string) => {
                // Return consistent deterministic vectors for testing
                if (text === "Alpha") return [1.0, 0.0];
                if (text === "Beta") return [0.9, 0.1]; // Close to A
                if (text === "Gamma") return [0.0, 1.0]; // Orthogonal to A
                return [0.1, 0.1]; // Default
            }),
            formatVectorForLisp: (v: number[]) => `#(${v.join(' ')})`,
            getInstance: () => ({ /* mock */ })
        }
    }
});

describe('Topological Lisp Engine', () => {

    it('should inject vectors via Orchestrator (Integration)', async () => {
        // Spy on Lisp Interpreter
        const executeSpy = vi.spyOn(lispInterpreter, 'executeAsync');
    
        const orch = new ReflectiveOrchestrator("Alpha e Beta existem.", [], "test-source");
        
        try {
            await orch.think();
        } catch (e) {
            // Expected catch
        }

        // Verify that 'atualizar-vetor' was called for Alpha and Beta
        // Check calls
        const calls = executeSpy.mock.calls.map(c => c[0]);
        const injectA = calls.find(c => c.includes("Alpha") && c.includes("atualizar-vetor"));
        
        expect(injectA).toBeDefined();
        // Alpha vector is [1.0, 0.0] -> #(1.0 0.0) or #(1 0)
        expect(injectA).toContain("#(1"); 
    });

    it('should perform semantic search in Lisp Kernel', async () => {
        // This runs against the REAL SBCL process (integration)
        
        // 1. Setup Data directly in Lisp
        await lispInterpreter.executeAsync(`(adicionar-memoria "Alpha" "Concept A" :vector #(1.0 0.0))`);
        await lispInterpreter.executeAsync(`(adicionar-memoria "Beta" "Concept B" :vector #(0.9 0.1))`);
        await lispInterpreter.executeAsync(`(adicionar-memoria "Gamma" "Concept C" :vector #(0.0 1.0))`);

        // 2. Query
        const result = await lispInterpreter.executeAsync(`(buscar-similares "Alpha" :threshold 0.5)`);
        
        // 3. Assert
        // SBCL returns uppercase symbols
        expect(result).toContain("BETA"); // Close
        expect(result).not.toContain("GAMMA"); // Distant
    });

    it('should execute boolean-fuzzy composition (King not Queen)', async () => {
        // Goal: Find ?x similar to "King" AND NOT similar to "Woman" -> "MaleMonarch"
        // Setup:
        // "King" anchor [1, 0, 0]
        // "Woman" anchor [0, 1, 0]
        // "Queen" = King + Woman = [0.7, 0.7, 0] (Sim to King ~0.7, Sim to Woman ~0.7)
        // "Emperor" = [0.9, 0.0, 0] (Sim to King ~0.9, Sim to Woman 0)
        
        await lispInterpreter.executeAsync('(limpar-memoria)');
        await lispInterpreter.executeAsync(`(adicionar-memoria "King" "A male ruler" :vector #(1.0 0.0 0.0))`);
        await lispInterpreter.executeAsync(`(adicionar-memoria "Woman" "A female" :vector #(0.0 1.0 0.0))`);
        
        // Candidates
        await lispInterpreter.executeAsync(`(adicionar-memoria "Queen" "Female ruler" :vector #(0.7 0.7 0.0))`);
        await lispInterpreter.executeAsync(`(adicionar-memoria "Emperor" "Male ruler" :vector #(0.9 0.0 0.0))`);

        // Rule: Near King (0.6), Not Near Woman (0.5)
        // Note: Queen is near King (0.7 > 0.6) BUT also near Woman (0.7 > 0.5), so should be excluded by NOT.
        // Emperor is near King (0.9 > 0.6) AND NOT near Woman (0.0 < 0.5), so should be included.
        const ruleCmd = `(adicionar-regra "MaleMonarchRule" 
                            '((similar-p ?x "King" 0.6) (not (similar-p ?x "Woman" 0.5))) 
                            '((?x "is-a" "MaleMonarch")))`;
        await lispInterpreter.executeAsync(ruleCmd);

        const infRes = await lispInterpreter.executeAsync('(inferir)');
        console.log("Boolean Inference Result:", infRes);

        const rels = await lispInterpreter.executeAsync('(listar-relacoes)');
        console.log("Boolean Relations:", rels);

        // Expect EMPEROR is-a MaleMonarch
        expect(rels).toContain("EMPEROR");
        expect(rels).toContain("MALEMONARCH");
        
        // Expect QUEEN NOT is-a MaleMonarch
        // Because she matches the first condition but fails the second (NOT sim-woman).
        expect(rels).not.toContain("QUEEN"); 
    });

    it('should execute fuzzy inference rules', async () => {
         // 1. Define Rule: If similar to "Alpha" (0.8), then is-a "AlphaType"
         // Note: We use "similar-p" with ?x unbound first to test generator mode.
         // MUST QUOTE THE LISTS in Lisp command string!
         const ruleCmd = `(adicionar-regra "FuzzyAlpha" '((similar-p ?x "Alpha" 0.8)) '((?x "e_um" "AlphaType")))`;
         await lispInterpreter.executeAsync(ruleCmd);

         // 2. Inject "Beta" (Sim=0.9 approx) which should trigger rule
         // Alpha is [1,0], Beta is [0.9, 0.1]. Dot=0.9.
         // Inject Gamma [0,1]. Dot=0.0. Should not trigger.
         // We reuse mocks from previous tests.
         
         // Clear memory first to be clean
         await lispInterpreter.executeAsync('(limpar-memoria)');
         await lispInterpreter.executeAsync(`(adicionar-memoria "Alpha" "Anchor" :vector #(1.0 0.0))`);
         await lispInterpreter.executeAsync(`(adicionar-memoria "Beta" "Candidate" :vector #(0.9 0.1))`); 
         await lispInterpreter.executeAsync(`(adicionar-memoria "Gamma" "Distractor" :vector #(0.0 1.0))`);

         // Re-add rule because cleaning memory might wipe rules? 
         // implementation check: Limpar-memoria keeps rules?
         // Let's check logic: *rules* is likely preserved or we re-add.
         // bootstrap.lisp: (defun limpar-memoria () ... (setf *rules* nil) ...) usually?
         // Actually bootstrap says (setf *knowledge-graph* ...) and (setf *relations* nil). 
         // Assuming rules persist. If not, we re-add.
         // Inspecting bootstrap: usually it does not wipe rules unless reset-total.
         // Let's safe re-add just in case or check bootstrap.
         await lispInterpreter.executeAsync(ruleCmd);

         // 3. Infer
         const infRes = await lispInterpreter.executeAsync('(inferir)');
         console.log("Inference Result:", infRes);

         // 4. Verify relations
         // Expect: (BETA e_um ALPHATYPE)
         // Expect NOT: (GAMMA e_um ALPHATYPE)
         const rels = await lispInterpreter.executeAsync('(listar-relacoes)');
         console.log("Relations:", rels);
         
         expect(rels).toContain("BETA");
         expect(rels).toContain("ALPHATYPE");
         expect(rels).not.toContain("GAMMA");
    });

});
