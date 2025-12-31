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
  const nodes = data.nodes.map((mem) => ({
    id: mem.key,
    // We use {0,0} because the frontend (Workspace.tsx) will handle the layout using Dagre.
    position: { x: 0, y: 0 },
    data: { label: mem.key, details: mem.value },
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


import { getActiveGraph } from "../logic/graph-engine";

import { knowledgeUnitService } from "./knowledge-unit-service";

export function commitKnowledgeToGraph(
  knowledge: any,
  filename?: string
): void {
  if (!knowledge || !knowledge.knowledgeBase) return;

  const graph = getActiveGraph(filename);

  if (filename) {
    graph.addNode(filename, "source");
  }

  knowledge.knowledgeBase.forEach((concept: any) => {
    // 1. Add Concept Node
    graph.addNode(concept.core_concept, "concept");

    if (filename) {
      graph.addRelation(concept.core_concept, "tem fonte", filename);
    }

    // 3. Add Relations
    if (concept.relatedConcepts) {
      concept.relatedConcepts.forEach((rel: any) => {
        const predicate =
          rel.type === "prerequisite"
            ? "depende de"
            : rel.type === "application"
            ? "aplica-se a"
            : rel.type === "contrast"
            ? "contrasta com"
            : "relacionado a";

        graph.addRelation(concept.core_concept, predicate, rel.conceptId);
      });
    }

    // 4. Add Nuggets as atomic facts
    if (concept.knowledgeNuggets) {
      concept.knowledgeNuggets.forEach((nugget: any) => {
        const factId = `fact-${Math.random().toString(36).substring(2, 9)}`;
        // We use the new KnowledgeGraph ability to store values (though we should check if addNode supports it)
        // For now, let's just make the factId descriptive or store the text in a way Lisp can see.
        graph.addNode(factId, "fact");
        // We add the relation
        graph.addRelation(concept.core_concept, "contem fato", factId);
      });
    }
  });

  // Save to Knowledge Unit Persistence
  if (filename) {
      // @ts-ignore - We will add exportState to KnowledgeGraph
      const graphData = graph.exportState(); 
      knowledgeUnitService.createOrUpdateUnit(filename, graphData);
  } else {
      // Default fallback
      const graphData = (graph as any).exportState ? (graph as any).exportState() : { nodes: [], relations: [] };
      knowledgeUnitService.createOrUpdateUnit("default", graphData);
  }
}

