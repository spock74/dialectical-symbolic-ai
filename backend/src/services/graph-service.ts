/*
 * Copyright (c) 2025 - 2026 J E Moraes.
 * All rights reserved.
 * 
 * Author: J E Moraes
 */

export interface Memory {
  key: string;
  value: string;
}

export interface Relation {
  source: string;
  target: string;
  relation: string;
  provenance?: "user" | "inference" | "INFERENCE";
}

export interface LispGraphExport {
  nodes: Memory[];
  edges: Relation[];
}

export interface GraphNode {
  id: string;
  position: { x: number; y: number };
  data: { label: string; details: string };
  type: string;
}

export interface GraphEdge {
  id: string;
  source: string;
  target: string;
  label?: string;
  type?: string;
  animated?: boolean;
  style?: { stroke?: string; strokeDasharray?: string };
}

export interface GraphData {
  nodes: GraphNode[];
  edges: GraphEdge[];
}

export function transformMemoriesToGraph(data: LispGraphExport): GraphData {
  const nodes = data.nodes.map((mem: any) => ({
    id: mem.key || mem.id,
    // We use {0,0} because the frontend (Workspace.tsx) will handle the layout using Dagre.
    position: { x: 0, y: 0 },
    data: { label: mem.key || mem.id, details: mem.value },
    type: "default",
  }));

  const edges = data.edges.map((rel, index) => ({
    id: `e${index}-${rel.source}-${rel.target}`,
    source: rel.source,
    target: rel.target,
    label: rel.relation,
    animated: true,
    type: "default", // Curved Bezier lines
    markerEnd: {
      type: "arrowclosed", // ReactFlow marker type as string for backend-frontend portability
      color:
        rel.provenance === "INFERENCE" || rel.provenance === "inference"
          ? "#a855f7"
          : "#b1b1b7",
    },
    style:
      rel.provenance === "INFERENCE" || rel.provenance === "inference"
        ? { stroke: "#a855f7", strokeDasharray: "5,5" } // Purple dashed for inferred
        : { stroke: "#b1b1b7" }, // Default gray
  }));

  return { nodes, edges };
}


// Helper to sanitize logic strings for Lisp
function escapeLispString(str: string): string {
    return str.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
}

import { getActiveGraph } from "../logic/graph-engine";
import { knowledgeUnitService } from "./knowledge-unit-service";
import { SBCLProcess } from "./sbcl-process";

export async function commitKnowledgeToGraph(
  knowledge: any,
  trackId?: string
): Promise<void> {
  if (!knowledge) return;

  const relations = knowledge.relations || [];
  const rules = knowledge.rules || [];

  const graph = getActiveGraph(trackId);

  // 1. Mark Track as Source
  if (trackId) {
    graph.addNode(trackId, "source");
  }

  // 2. THE BRIDGE: Batched Lisp Kernel Injection
  console.log(`[Service] Queueing knowledge for Lisp Kernel: ${trackId || 'default'}`);
  const lispCommands: string[] = [];

  relations.forEach((rel: any) => {
    lispCommands.push(`(adicionar-relacao "${escapeLispString(rel.source)}" "${escapeLispString(rel.label)}" "${escapeLispString(rel.target)}" :category :${rel.category || "ONTOLOGY"})`);
  });

  // 3. Sync Relations (Facts) in JS Engine
  relations.forEach((rel: any) => {
    graph.addNode(rel.source, "concept");
    graph.addNode(rel.target, "concept");
    graph.addRelation(rel.source, rel.label, rel.target, "user", rel.category || "ONTOLOGY");

    if (trackId) {
      graph.addRelation(rel.source, "tem fonte", trackId, "user", "BIBLIOGRAPHIC");
    }
  });

  // 4. Robust Rule Extraction
  if (knowledge.lisp_raw) {
    const REGEX_RULE = /\(adicionar-regra\s+'([^\s\n]+)\s+'(\([\s\S]*?\))\s+'(\([\s\S]*?\))\)/g;
    let match;
    while ((match = REGEX_RULE.exec(knowledge.lisp_raw)) !== null) {
      const fullRuleCode = match[0];
      lispCommands.push(fullRuleCode);
      graph.addRule(fullRuleCode);
    }
  }

  if (lispCommands.length > 0) {
    // CHUNK BATCHES to avoid timeouts on large PDFs
    const CHUNK_SIZE = 50; 
    for (let i = 0; i < lispCommands.length; i += CHUNK_SIZE) {
        const chunk = lispCommands.slice(i, i + CHUNK_SIZE);
        const batchedCmd = `(progn ${chunk.join(' ')} (values))`;
        
        try {
            await SBCLProcess.getInstance().evaluate(batchedCmd, 120000);
            console.log(`[Service] Injected chunk ${Math.floor(i/CHUNK_SIZE) + 1}/${Math.ceil(lispCommands.length/CHUNK_SIZE)} (${chunk.length} commands)`);
        } catch (e) {
            console.error(`[Service] Chunk injection failed:`, e);
        }
    }
  }

  // 5. Persistence
  // @ts-ignore
  const graphData = graph.exportState(); 
  await knowledgeUnitService.createOrUpdateUnit(trackId || "default", graphData);
  
  // Also save to global graphs directory for standard visualization
  const graphPath = `data/graphs/${trackId || "default"}.json`;
  // @ts-ignore
  await graph.saveState(graphPath);
}

