export interface Memory {
    key: string;
    value: string;
}

export interface GraphNode {
  id: string;
  position: { x: number; y: number };
  data: { label: string; details: string };
  type: string;
}

export interface GraphData {
    nodes: GraphNode[];
    edges: any[];
}

export function transformMemoriesToGraph(memories: Memory[]): GraphData {
    const nodes = memories.map((mem) => ({
      id: mem.key,
      // Ideally we would layout these properly, e.g. using dagre or elkjs, 
      // but for now random positioning is the placeholder.
      // We explicitly leave Math.random() here for now as per original implementation.
      position: { x: Math.random() * 500, y: Math.random() * 500 }, 
      data: { label: mem.key, details: mem.value },
      type: "default",
    }));

    return { nodes, edges: [] };
}
