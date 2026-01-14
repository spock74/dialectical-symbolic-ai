import { useEffect, useState, useRef, useMemo } from 'react';
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
import { fetchGraph } from '../api';
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
import { RELATION_COLORS, DEFAULT_EDGE_COLOR } from '../config/color_constants';

const nodeWidth = 172;
const nodeHeight = 40;

// Optimization: Define types outside to avoid re-creation
const nodeTypes = {};
const edgeTypes = {};
const EMPTY_ARRAY: string[] = [];

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
    
    // Return a new object to avoid mutating the original node
    return {
      ...node,
      targetPosition: isHorizontal ? 'left' : 'top',
      sourcePosition: isHorizontal ? 'right' : 'bottom',
      position: {
        x: nodeWithPosition.x - nodeWidth / 2,
        y: nodeWithPosition.y - nodeHeight / 2,
      }
    };
  });

  return { nodes: layoutedNodes, edges };
};

interface WorkspaceProps {
  knowledgeBase: KnowledgeBase | null;
}

// Optimization: Define types outside to avoid re-creation
// const nodeTypes = {}; // Already defined above
// const edgeTypes = {}; // Already defined above

function GraphView({ activeSource }: { activeSource: any }) {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [rawGraphData, setRawGraphData] = useState<{ nodes: any[], edges: any[] } | null>(null);
  
  const { fitView } = useReactFlow();
  const lastSourceIdRef = useRef<string | null>(null);
  
  // Use selectors to avoid unnecessary re-renders
  const graphVersion = useDialecticStore(state => state.graphVersion);
  const graphDirection = useDialecticStore(state => state.graphDirection);
  const activeSourceId = useDialecticStore(state => state.activeSourceId);
  const sourceFilters = useDialecticStore(state => state.sourceFilters);
  const setShowEntities = useDialecticStore(state => state.setShowEntities);
  const setShowRelations = useDialecticStore(state => state.setShowRelations);
  const setGraphDirection = useDialecticStore(state => state.setGraphDirection);
  const toggleRelationType = useDialecticStore(state => state.toggleRelationType);
  const setSelectedRelationTypes = useDialecticStore(state => state.setSelectedRelationTypes);
  const initializeSourceFilters = useDialecticStore(state => state.initializeSourceFilters);

  const activeFilters = (activeSourceId && sourceFilters[activeSourceId]) || null;
  
  const showEntities = activeFilters?.showEntities !== false; // Default to true
  const showRelations = activeFilters?.showRelations !== false; // Default to true
  // Fix: Use stable empty array reference
  const selectedRelationTypes = activeFilters?.selectedRelationTypes || EMPTY_ARRAY;

  const handleDownloadRaw = () => {
    try {
      const data = { nodes, edges };
      const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = `knowledge-graph-view-${new Date().toISOString().split('T')[0]}.json`;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
    } catch (err) {
      console.error("Failed to download graph JSON:", err);
    }
  };

  // 1. Fetch data ONLY on source/version change
  useEffect(() => {
    if (!activeSource) {
      setRawGraphData(null);
      return;
    }

    fetchGraph(activeSource.name)
      .then(data => {
        setRawGraphData(data);
        
        // Count frequencies in raw data for filter UI
        const counts: Record<string, number> = {};
        data.edges.forEach((e: any) => {
            const type = e.label || e.relation;
            if (type) {
                counts[type] = (counts[type] || 0) + 1;
            }
        });

        const sortedTypes = Object.keys(counts).sort((a, b) => counts[b] - counts[a]);

        // Initialize filters if needed
        if (activeSource?.id && !sourceFilters[activeSource.id]) {
            initializeSourceFilters(activeSource.id, sortedTypes);
        }
      })
      .catch(err => console.error("Failed to fetch graph data:", err));
  }, [activeSource?.name, graphVersion, activeSourceId]); // Minimal dependencies

  // 2. Local Filtering and Mapping
  const { filteredNodes, filteredEdges, availableRelationTypes, relationCounts } = useMemo(() => {
    if (!rawGraphData) return { filteredNodes: EMPTY_ARRAY, filteredEdges: EMPTY_ARRAY, availableRelationTypes: EMPTY_ARRAY, relationCounts: {} };

    // A. Recalculate frequencies for UI
    const counts: Record<string, number> = {};
    rawGraphData.edges.forEach((e: any) => {
        const type = e.label || e.relation;
        if (type) counts[type] = (counts[type] || 0) + 1;
    });
    const sortedTypes = Object.keys(counts).sort((a, b) => counts[b] - counts[a]);

    // B. Map and Color Edges
    const CATEGORY_COLORS: Record<string, string> = {
        "CAUSAL": "#ef4444",
        "METHODOLOGY": "#3b82f6",
        "BIBLIOGRAPHIC": "#94a3b8",
        "ONTOLOGY": "#22c55e",
    };

    const typeColorMap: Record<string, string> = {};
    sortedTypes.forEach((type: string, index: number) => {
        typeColorMap[type] = RELATION_COLORS[index % RELATION_COLORS.length];
    });

    let currentEdges = rawGraphData.edges.map((edge: any) => {
        const label = edge.label || edge.relation;
        let color = DEFAULT_EDGE_COLOR;
        if (edge.category) {
            const catKey = edge.category.replace(/^:/, '').toUpperCase();
            color = CATEGORY_COLORS[catKey] || typeColorMap[label] || DEFAULT_EDGE_COLOR;
        } else {
            color = typeColorMap[label] || DEFAULT_EDGE_COLOR;
        }
        
        return {
            ...edge,
            label, // Ensure label exists for filtered check
            style: { ...edge.style, stroke: color, strokeWidth: 2 },
            markerEnd: { type: 'arrowclosed', color },
            data: { ...edge.data, color } 
        };
    });

    // C. Filter Edges
    if (!showRelations) {
        currentEdges = [];
    } else if (activeFilters) {
        // Only apply subset filter if filter state has been initialized/modified
        currentEdges = currentEdges.filter((e: any) => selectedRelationTypes.includes(e.label));
    }

    // D. Filter Nodes
    const connectedNodeIds = new Set([
        ...currentEdges.map((e: any) => e.source),
        ...currentEdges.map((e: any) => e.target)
    ]);

    let currentNodes = rawGraphData.nodes;
    if (!showEntities) {
        currentNodes = currentNodes.filter((n: any) => connectedNodeIds.has(n.id));
    }

    return { 
        filteredNodes: currentNodes, 
        filteredEdges: currentEdges, 
        availableRelationTypes: sortedTypes, 
        relationCounts: counts 
    };
  }, [rawGraphData, showEntities, showRelations, selectedRelationTypes]);

  // 3. Layout and Sync
  useEffect(() => {
    if (!rawGraphData) {
        setNodes([]);
        setEdges([]);
        return;
    }

    // console.log('[GraphView] Filtering details:', { ... });

    try {
        const { nodes: layoutedNodes, edges: layoutedEdges } = getLayoutedElements(
          filteredNodes,
          filteredEdges,
          graphDirection
        );
        
        // console.log('[GraphView] Layout result:', { ... });
        
        setNodes(layoutedNodes);
        setEdges(layoutedEdges);

        // Auto-fit on source change
        if (activeSource?.id !== lastSourceIdRef.current) {
            lastSourceIdRef.current = activeSource?.id || null;
            setTimeout(() => fitView({ duration: 800 }), 50);
        }
    } catch (e) {
        console.error("Layout calculation failed:", e);
    }
  }, [filteredNodes, filteredEdges, graphDirection, fitView]);


  const activeFiltersCount = selectedRelationTypes.length + (showEntities ? 0 : 1) + (showRelations ? 0 : 1);

  return (
    <div className="flex-1 w-full h-full relative"> 
      <ReactFlow 
        nodes={nodes}
        edges={edges}
        nodeTypes={nodeTypes}
        edgeTypes={edgeTypes}
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
                <PopoverContent className="w-80 p-0 bg-card/95 backdrop-blur-xl border-border shadow-2xl" align="end">
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
                          <div className="flex gap-2">
                            <button 
                              onClick={() => setSelectedRelationTypes(availableRelationTypes)}
                              className="text-[10px] text-blue-400 hover:text-blue-300 transition-colors"
                            >
                              Tudo
                            </button>
                            {selectedRelationTypes.length > 0 && (
                              <button 
                                onClick={() => setSelectedRelationTypes([])}
                                className="text-[10px] text-red-400 hover:text-red-300 transition-colors"
                              >
                                Limpar
                              </button>
                            )}
                          </div>
                        </div>
                        <ScrollArea className="h-[200px] -mr-4 pr-4">
                          <div className="space-y-1 pr-2">
                            {availableRelationTypes.map((type, index) => {
                              const color = RELATION_COLORS[index % RELATION_COLORS.length];
                              return (
                                <div key={type} className="flex items-center justify-between group px-2 py-1.5 rounded hover:bg-muted/50 transition-colors">
                                  <label 
                                    htmlFor={`pop-filter-${type}`} 
                                    className="text-xs truncate max-w-[160px] cursor-pointer text-muted-foreground group-hover:text-foreground transition-colors flex flex-1 items-center mr-2"
                                  >
                                    {/* Badge on left */}
                                    <span 
                                        className="text-[10px] px-1.5 py-0.5 rounded-full mr-2 font-mono font-bold"
                                        style={{ backgroundColor: `${color}20`, color: color }}
                                    >
                                      {relationCounts[type] || 0}
                                    </span>
                                    <span>{type}</span>
                                  </label>
                                  <Checkbox 
                                    id={`pop-filter-${type}`} 
                                    checked={selectedRelationTypes.includes(type)}
                                    onCheckedChange={() => toggleRelationType(type)}
                                    style={{ 
                                        borderColor: color, // Always keep the colored border
                                        backgroundColor: selectedRelationTypes.includes(type) ? color : 'transparent' // Fill when checked
                                    }}
                                    className="data-[state=checked]:text-white" // Ensure checkmark is white
                                  />
                                </div>
                              );
                            })}
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
  const activeGroupId = useDialecticStore(state => state.activeGroupId);
  const activeSourceId = useDialecticStore(state => state.activeSourceId);
  const groups = useDialecticStore(state => state.groups);
  
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
