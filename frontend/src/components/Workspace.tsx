

import { useEffect, useState } from 'react';
import ReactFlow, { 
  Background, 
  Controls, 
  useNodesState, 
  useEdgesState,
  ConnectionLineType,
  Panel,
  ReactFlowProvider,
  useReactFlow
} from 'reactflow';
import { 
  Download, 
  Filter, 
  Eye, 
  EyeOff, 
  Layers, 
  Hash,
  ChevronDown
} from 'lucide-react';
import dagre from 'dagre';
import 'reactflow/dist/style.css';

import type { KnowledgeBase } from '../types';
import { fetchGraph, fetchGraphRaw } from '../api';
import { useDialecticStore } from '../store/useStore';
import { CardContent, CardHeader, CardTitle } from './ui/card';
import { Checkbox } from './ui/checkbox';
import { ScrollArea } from './ui/scroll-area';
import { Button } from './ui/button';
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "./ui/popover";

const nodeWidth = 172;
const nodeHeight = 40;

const getLayoutedElements = (nodes: any[], edges: any[], direction = 'TB') => {
  const isHorizontal = direction === 'LR';
  
  // Initialize a fresh graph every time 
  const dagreGraph = new dagre.graphlib.Graph();
  dagreGraph.setDefaultEdgeLabel(() => ({}));
  
  // Increased spacing: nodesep 120, ranksep 150
  dagreGraph.setGraph({ rankdir: direction, nodesep: 120, ranksep: 150 });

  nodes.forEach((node) => {
    dagreGraph.setNode(node.id, { width: nodeWidth, height: nodeHeight });
  });

  edges.forEach((edge) => {
    dagreGraph.setEdge(edge.source, edge.target);
  });

  dagre.layout(dagreGraph);

  const layoutedNodes = nodes.map((node) => {
    const nodeWithPosition = dagreGraph.node(node.id);
    node.targetPosition = isHorizontal ? 'left' : 'top';
    node.sourcePosition = isHorizontal ? 'right' : 'bottom';

    // We are shifting the dagre node position (which is center-based) to top-left
    node.position = {
      x: nodeWithPosition.x - nodeWidth / 2,
      y: nodeWithPosition.y - nodeHeight / 2,
    };

    return node;
  });

  return { nodes: layoutedNodes, edges };
};

interface WorkspaceProps {
  knowledgeBase: KnowledgeBase | null;
}

function GraphView({ activeSource }: { activeSource: any }) {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [availableRelationTypes, setAvailableRelationTypes] = useState<string[]>([]);
  const { fitView } = useReactFlow();
  
  const {
    graphVersion,
    showEntities,
    showRelations,
    graphDirection,
    selectedRelationTypes,
    setShowEntities,
    setShowRelations,
    setGraphDirection,
    toggleRelationType,
    setSelectedRelationTypes,
  } = useDialecticStore();

  const handleDownloadRaw = async () => {
    try {
      const data = await fetchGraphRaw(activeSource?.name);
      const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = `knowledge-graph-${new Date().toISOString().split('T')[0]}.json`;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
    } catch (err) {
      console.error("Failed to download graph JSON:", err);
    }
  };

  // Fetch and Filter Logic
  useEffect(() => {
    fetchGraph(activeSource?.name)
      .then(data => {
        // Collect ALL relation types from raw data
        const types = Array.from(new Set(data.edges.map((e: any) => e.label))).filter(Boolean) as string[];
        setAvailableRelationTypes(types);

        let filteredEdges = data.edges;

        // 1. Filter edges first
        if (!showRelations) {
          filteredEdges = [];
        } else if (selectedRelationTypes.length > 0) {
          filteredEdges = filteredEdges.filter((e: any) => selectedRelationTypes.includes(e.label));
        }

        // 2. Filter nodes: 
        // - Always show nodes that are connected by visible edges.
        // - Only show "isolated" nodes if showEntities is TRUE.
        const connectedNodeIds = new Set([
            ...filteredEdges.map((e: any) => e.source),
            ...filteredEdges.map((e: any) => e.target)
        ]);

        let filteredNodes = data.nodes;
        if (!showEntities) {
            filteredNodes = filteredNodes.filter((n: any) => connectedNodeIds.has(n.id));
        }

        // 3. Layout
        const { nodes: layoutedNodes, edges: layoutedEdges } = getLayoutedElements(
          filteredNodes,
          filteredEdges,
          graphDirection
        );
        
        setNodes(layoutedNodes);
        setEdges(layoutedEdges);

        // 4. Auto-fit centering
        setTimeout(() => fitView({ duration: 800 }), 50);
      })
      .catch(err => console.error("Failed to fetch graph nodes:", err));
  }, [
    graphVersion, 
    setNodes, 
    setEdges, 
    activeSource?.name, 
    showEntities, 
    showRelations, 
    selectedRelationTypes,
    graphDirection,
    fitView
  ]); 

  const activeFiltersCount = selectedRelationTypes.length + (showEntities ? 0 : 1) + (showRelations ? 0 : 1);

  return (
    <div className="flex-1 w-full h-full relative"> 
      <ReactFlow 
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        connectionLineType={ConnectionLineType.Bezier}
        fitView
        minZoom={0.05}
        maxZoom={2}
        attributionPosition="bottom-left"
        className="bg-dot-pattern"
        key={`${activeSource?.name}-${graphVersion}`}
      >
        <Background color="#333" gap={20} size={1} />
        <Controls />
        
        <Panel position="top-right" className="flex flex-col gap-3">
           <div className="flex items-center gap-2 bg-card/80 backdrop-blur-md p-1.5 rounded-lg border border-border shadow-lg">
              <button 
                onClick={handleDownloadRaw}
                className="px-3 py-1 bg-primary text-primary-foreground rounded-md text-xs font-medium border border-border hover:bg-primary/90 shadow-sm flex items-center gap-1.5 transition-colors"
                title="Baixar JSON"
              >
                <Download size={14} />
                Exportar
              </button>
              
              <div className="h-6 w-[1px] bg-border mx-1" />
              
              <div className="flex bg-secondary/50 rounded-md p-0.5 border border-border/50">
                <button 
                  onClick={() => setGraphDirection('TB')}
                  className={`px-3 py-1 rounded-sm text-xs font-medium transition-all ${graphDirection === 'TB' ? 'bg-primary text-primary-foreground shadow-sm' : 'text-muted-foreground hover:text-foreground'}`}
                >
                  Vertical
                </button>
                <button 
                  onClick={() => setGraphDirection('LR')}
                  className={`px-3 py-1 rounded-sm text-xs font-medium transition-all ${graphDirection === 'LR' ? 'bg-primary text-primary-foreground shadow-sm' : 'text-muted-foreground hover:text-foreground'}`}
                >
                  Horizontal
                </button>
              </div>

              <div className="h-6 w-[1px] bg-border mx-1" />

              <Popover>
                <PopoverTrigger asChild>
                  <Button 
                    variant="outline" 
                    size="sm" 
                    className={`h-8 gap-2 px-3 border-border shadow-sm transition-all ${activeFiltersCount > 0 ? 'bg-blue-500/10 border-blue-500/50 text-blue-400 hover:bg-blue-500/20' : 'bg-transparent text-muted-foreground hover:text-foreground'}`}
                  >
                    <Filter size={14} className={activeFiltersCount > 0 ? "text-blue-400" : "text-muted-foreground"} />
                    <span className="text-xs">
                      {activeFiltersCount > 0 ? `Filtros (${activeFiltersCount})` : 'Filtros'}
                    </span>
                    <ChevronDown size={12} className="opacity-50" />
                  </Button>
                </PopoverTrigger>
                <PopoverContent className="w-64 p-0 bg-card/95 backdrop-blur-xl border-border shadow-2xl" align="end">
                  <CardHeader className="p-4 pb-2 flex flex-row items-center justify-between space-y-0">
                    <CardTitle className="text-[10px] uppercase font-bold tracking-wider text-muted-foreground flex items-center gap-2">
                      <Filter size={12} />
                      Configurações de Visão
                    </CardTitle>
                  </CardHeader>
                  <CardContent className="p-4 pt-1 space-y-5">
                    <div className="space-y-3">
                      <div className="flex items-center justify-between group">
                        <label htmlFor="pop-entities" className="text-sm flex items-center gap-2 cursor-pointer">
                          {showEntities ? <Eye size={14} className="text-blue-400" /> : <EyeOff size={14} className="text-muted-foreground" />}
                          <span className={showEntities ? "text-foreground" : "text-muted-foreground"}>Mostrar Entidades</span>
                        </label>
                        <Checkbox 
                          id="pop-entities" 
                          checked={showEntities} 
                          onCheckedChange={(checked) => setShowEntities(!!checked)}
                        />
                      </div>
                      <div className="flex items-center justify-between group">
                        <label htmlFor="pop-relations" className="text-sm flex items-center gap-2 cursor-pointer">
                          {showRelations ? <Layers size={14} className="text-green-400" /> : <Layers size={14} className="text-muted-foreground" />}
                          <span className={showRelations ? "text-foreground" : "text-muted-foreground"}>Mostrar Relações</span>
                        </label>
                        <Checkbox 
                          id="pop-relations" 
                          checked={showRelations} 
                          onCheckedChange={(checked) => setShowRelations(!!checked)}
                        />
                      </div>
                    </div>

                    {availableRelationTypes.length > 0 && (
                      <div className="space-y-3 pt-3 border-t border-border/50">
                        <div className="flex items-center justify-between">
                          <div className="flex items-center gap-2 text-[10px] font-bold text-muted-foreground uppercase tracking-tight">
                            <Hash size={12} />
                            Tipos de Relação
                          </div>
                          {selectedRelationTypes.length > 0 && (
                            <button 
                              onClick={() => setSelectedRelationTypes([])}
                              className="text-[10px] text-blue-400 hover:text-blue-300 transition-colors"
                            >
                              Limpar
                            </button>
                          )}
                        </div>
                        <ScrollArea className="h-[200px] -mr-4 pr-4">
                          <div className="space-y-1 pr-2">
                            {availableRelationTypes.map((type) => (
                              <div key={type} className="flex items-center justify-between group px-2 py-1.5 rounded hover:bg-muted/50 transition-colors">
                                <label 
                                  htmlFor={`pop-filter-${type}`} 
                                  className="text-xs truncate max-w-[160px] cursor-pointer text-muted-foreground group-hover:text-foreground transition-colors"
                                >
                                  {type}
                                </label>
                                <Checkbox 
                                  id={`pop-filter-${type}`} 
                                  checked={selectedRelationTypes.includes(type)}
                                  onCheckedChange={() => toggleRelationType(type)}
                                />
                              </div>
                            ))}
                          </div>
                        </ScrollArea>
                      </div>
                    )}
                  </CardContent>
                </PopoverContent>
              </Popover>
           </div>
        </Panel>
      </ReactFlow>
    </div>
  );
}

export function Workspace({ knowledgeBase: _ }: WorkspaceProps) {
  const { groups, activeGroupId, activeSourceId } = useDialecticStore();
  const activeGroup = groups.find(g => g.id === activeGroupId);
  const activeSource = activeGroup?.sources.find(s => s.id === activeSourceId);

  return (
    <div className="flex flex-col h-full bg-background relative overflow-hidden">
      <ReactFlowProvider>
        <GraphView activeSource={activeSource} />
      </ReactFlowProvider>
    </div>
  );
}

