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
    // Ideally we would layout these properly, e.g. using dagre or elkjs,
    // but for now random positioning is the placeholder.
    // We explicitly leave Math.random() here for now as per original implementation.
    position: { x: Math.random() * 500, y: Math.random() * 500 },
    data: { label: mem.key, details: mem.value },
    type: "default",
  }));

  const edges = data.edges.map((rel, index) => ({
    id: `e${index}-${rel.source}-${rel.target}`,
    source: rel.source,
    target: rel.target,
    label: rel.relation,
    animated: true,
    type: "smoothstep", // or 'straight', 'default'
    style:
      rel.provenance === "INFERENCE" || rel.provenance === "inference"
        ? { stroke: "#a855f7", strokeDasharray: "5,5" } // Purple dashed for inferred
        : { stroke: "#b1b1b7" }, // Default gray
  }));

  return { nodes, edges };
}

