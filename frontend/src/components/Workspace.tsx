import ReactFlow, { Background, Controls } from 'reactflow';
import 'reactflow/dist/style.css';
import type { KnowledgeBase } from '../types';

interface WorkspaceProps {
  knowledgeBase: KnowledgeBase | null;
}

export function Workspace({ knowledgeBase }: WorkspaceProps) {
  // Use knowledgeBase to generate nodes (mock usage to avoid unused var error)
  const nodes = knowledgeBase ? [
    { id: '1', position: { x: 0, y: 0 }, data: { label: `KB Nodes: ${knowledgeBase.knowledgeBase.length}` } }
  ] : [
    { id: '1', position: { x: 0, y: 0 }, data: { label: 'Empty Knowledge Base' } }
  ];



  return (
    <div className="flex flex-col h-full bg-background relative">
      <div className="flex-1 w-full h-full"> 
        <ReactFlow 
          nodes={nodes}
          defaultEdges={[]}
          fitView
        >
          <Background />
          <Controls />
        </ReactFlow>
      </div>
    </div>
  );
}
