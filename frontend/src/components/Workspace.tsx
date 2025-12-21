

import { useEffect } from 'react';
import ReactFlow, { Background, Controls, useNodesState, useEdgesState } from 'reactflow';
import 'reactflow/dist/style.css';

import type { KnowledgeBase } from '../types';
import { fetchGraph } from '../api';
import { useDialecticStore } from '../store/useStore';

interface WorkspaceProps {
  knowledgeBase: KnowledgeBase | null;
}

export function Workspace({ knowledgeBase: _ }: WorkspaceProps) {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const graphVersion = useDialecticStore((state) => state.graphVersion);

  // Fetch graph data on mount AND when version changes
  useEffect(() => {
    fetchGraph()
      .then(data => {
        // @ts-ignore - mismatch between API types and ReactFlow types needs more explicit mapping or casting
        setNodes(data.nodes);
        // @ts-ignore
        setEdges(data.edges);
      })
      .catch(err => console.error("Failed to fetch graph nodes:", err));
  }, [graphVersion, setNodes, setEdges]); // Trigger re-fetch when graphVersion increments

  return (
    <div className="flex flex-col h-full bg-background relative">
      <div className="flex-1 w-full h-full"> 
        <ReactFlow 
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          nodesDraggable={true}
          fitView
          attributionPosition="bottom-left"
        >
          <Background color="#444" gap={16} />
          <Controls />
        </ReactFlow>
      </div>
    </div>
  );
}

