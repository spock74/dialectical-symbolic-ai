import { KnowledgeGraph } from '../logic/graph-engine';
import { lispInterpreter } from '../logic/lisp-interpreter';
import * as path from 'path';
import * as fs from 'fs';

async function testKernel() {
  console.log("--- Testing TS-Symbolic-Kernel ---");
  const graph = new KnowledgeGraph("test-source");

  // 1. Test Node Addition
  console.log("\n1. Testing Node Addition:");
  graph.addNode("A", "concept");
  graph.addNode("B", "concept");
  console.log(graph.listNodes());

  // 2. Test Relation Addition (Step 1)
  console.log("\n2. Testing Relation Addition:");
  graph.addRelation("A", "e_um", "B");
  graph.addRelation("B", "e_um", "C");
  console.log("Relations:", graph.listRelations());

  // 3. Test Inference (Step 3 Mapping)
  console.log("\n3. Testing Inference:");
  const inferences = graph.infer();
  console.log("Inferences:", inferences);
  console.log("Final Relations:", graph.listRelations());

  // 4. Test Causal Intervention (Step 2)
  console.log("\n4. Testing Causal Intervention (do A forçado para true):");
  graph.addRelation("X", "causa", "A");
  console.log("Before Intervention:", graph.listRelations());
  graph.doIntervention("A", true);
  console.log("After Intervention (Incoming edges to A should be gone):", graph.listRelations());
  console.log("Node A Value:", graph.getNode("A")?.value);

  // 5. Test Reset Intervention (Step 2)
  console.log("\n5. Testing Reset Intervention:");
  graph.resetIntervention();
  console.log("After Reset (Incoming edges to A should return):", graph.listRelations());

  // 6. Test Interpreter (Step 3)
  console.log("\n6. Testing Interpreter:");
  const cmd = '(adicionar-relacao "D" "mora_em" "E") (inferir)';
  const output = lispInterpreter.execute(cmd, graph);
  console.log("Interpreter Output:\n", output);
  console.log("Relations after interpreter:", graph.listRelations());

  // 7. Test Save/Load (Step 1)
  console.log("\n7. Testing Persistence:");
  const testPath = path.join(process.cwd(), "data", "test-graph.json");
  graph.saveState(testPath);
  console.log(`Saved to ${testPath}`);
  
  const newGraph = new KnowledgeGraph("loader-test");
  newGraph.loadState(testPath);
  console.log("Loaded Relations:", newGraph.listRelations());

  // Cleanup
  if (fs.existsSync(testPath)) fs.unlinkSync(testPath);
  console.log("\n--- Verification Complete ---");
}

testKernel().catch(console.error);
