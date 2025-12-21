import { useEffect, useState } from 'react';
import ReactFlow, { Background, Controls } from 'reactflow';
import type { Node, Edge } from 'reactflow';
import 'reactflow/dist/style.css';
import type { KnowledgeBase } from '../types';
import { fetchGraph } from '../api';

interface WorkspaceProps {
  knowledgeBase: KnowledgeBase | null;
}

export function Workspace({ knowledgeBase: _ }: WorkspaceProps) {
  const [nodes, setNodes] = useState<Node[]>([]);
  const [edges, setEdges] = useState<Edge[]>([]);

  // Fetch graph data on mount
  useEffect(() => {
    fetchGraph()
      .then(data => {
        setNodes(data.nodes);
        setEdges(data.edges);
      })
      .catch(err => console.error("Failed to fetch graph nodes:", err));
  }, []); // Run once on mount

  // Optionally merge local knowledgeBase stats if needed, or just rely on backend Lisp memory

  return (
    <div className="flex flex-col h-full bg-background relative">
      <div className="flex-1 w-full h-full"> 
        <ReactFlow 
          nodes={nodes}
          edges={edges}
          onNodesChange={(_changes) => {
             // We need to implement onNodesChange if we want interactivity (dragging)
             // For static view, this is optional but recommended.
          }}
          nodesDraggable={true}
          fitView
        >
          <Background />
          <Controls />
        </ReactFlow>
      </div>
    </div>
  );
}
