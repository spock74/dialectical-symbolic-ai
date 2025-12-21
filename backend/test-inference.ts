
import { SbclProcess } from "./src/lisp/sbcl-process";

async function testInference() {
    const lisp = SbclProcess.getInstance();
    
    // Allow time for bootstrap
    await new Promise(r => setTimeout(r, 2000));

    console.log("--- Strating Inference Test ---");

    try {
        // 1. Add Fact: Socrates is a Man
        console.log("Adding Fact...");
        await lisp.eval(`(s-dialectic:adicionar-relacao 'Socrates 'is-a 'Man)`);

        // 2. Add Rule: Transitivity / Inheritance
        // If ?x is-a ?y AND ?y is-a ?z -> ?x is-a ?z
        // Wait, for Socrates, we want: (?x is-a Man) -> (?x is-a Mortal)
        console.log("Adding Rule...");
        await lisp.addRule(
            'MortalRule', 
            '((?x is-a Man))', 
            '((?x is-a Mortal))'
        );

        // 3. Verify BEFORE inference
        const jsonBefore = await lisp.getGraphData();
        console.log("Edges Before:", jsonBefore.edges);

        // 4. Run Inference
        console.log("Running Inference...");
        const result = await lisp.runInference();
        console.log("Inference Result:", result);

        // 5. Verify AFTER inference
        const jsonAfter = await lisp.getGraphData();
        console.log("Edges After:", jsonAfter.edges);

        const found = jsonAfter.edges.find((e: any) => 
            e.source === "SOCRATES" && 
            e.relation === "IS-A" && 
            e.target === "MORTAL"
        );

        if (found) {
            console.log("SUCCESS: Deduced Socrates is Mortal!");
            if (found.provenance === "INFERENCE" || found.provenance === "inference") {
                 console.log("SUCCESS: Provenance correctly marked as INFERENCE");
            } else {
                 console.error("FAILURE: Provenance NOT marked correctly. Got:", found.provenance);
            }
        } else {
            console.error("FAILURE: Did not deduce Socrates is Mortal.");
        }

    } catch (e) {
        console.error("Test Failed:", e);
    } finally {
        lisp.kill();
        process.exit(0);
    }
}

testInference();
