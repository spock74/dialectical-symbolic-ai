import { describe, it, expect, vi, beforeEach } from 'vitest';
import { reflectiveLoop } from '../flows/reflective-loop';
import { SbclProcess } from '../lisp/sbcl-process';
import { CONFIG } from '../config/constants';

// We want to test against the ACTUAL Lisp if possible, or at least a very good mock.
// Since we are in verification, let's try to run against the real getInstance().

describe('Transitive Inference Verification', () => {
    let lisp: SbclProcess;

    beforeEach(async () => {
        lisp = SbclProcess.getInstance();
        await lisp.eval("(s-dialectic:limpar-memoria)");
    });

    it('should derive transitive relationship (A is-a C)', async () => {
        const input = {
            prompt: "A é um B. B é um C. Qual a relação entre A e C?",
            bypassSDialect: false,
            useMemory: false
        };

        // Execution
        const result = await reflectiveLoop(input);

        // Verification
        // The synthesis should mention that A is a C
        expect(result.text).toMatch(/A.*C/);
        
        // Deep verification in the kernel
        const { result: relations } = await lisp.eval("(s-dialectic:listar-relacoes)");
        
        // Look for the specific struct or formatted string
        // The inferir call in orchestrator should have added this already.
        expect(relations).toContain('É UM');
        
        // Check for specific deduction
        const { result: searchResult } = await lisp.eval('(s-dialectic:buscar-relacoes "A")');
        expect(searchResult).toContain('C'); 
    });
});
