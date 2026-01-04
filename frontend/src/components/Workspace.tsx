

import { useEffect, useState, useRef } from 'react';
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
  const [relationCounts, setRelationCounts] = useState<Record<string, number>>({});
  const { fitView } = useReactFlow();
  const lastSourceIdRef = useRef<string | null>(null);
  
  // Subscribe to store state needed for filters
  const {
    graphVersion,
    graphDirection,
    activeSourceId,
    sourceFilters,
    setShowEntities,
    setShowRelations,
    setGraphDirection,
    toggleRelationType,
    setSelectedRelationTypes,
    initializeSourceFilters,
  } = useDialecticStore();

  // Derive filter state for valid active source or fallback to defaults
  const activeFilters = (activeSourceId && sourceFilters[activeSourceId]) || {
    showEntities: true,
    showRelations: true,
    selectedRelationTypes: [], // Initially empty, will be populated by effect
  };
  
  const { showEntities, showRelations, selectedRelationTypes } = activeFilters;

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

  // Fetch and Filter Logic
  useEffect(() => {
    if (!activeSource) {
      setNodes([]);
      setEdges([]);
      setRelationCounts({});
      setAvailableRelationTypes([]);
      return;
    }

    fetchGraph(activeSource.name)
      .then(data => {
        // Collect types and count frequencies
        const counts: Record<string, number> = {};
        data.edges.forEach((e: any) => {
            if (e.label) {
                counts[e.label] = (counts[e.label] || 0) + 1;
            }
        });
        setRelationCounts(counts);

        // Sort types by count (descending)
        const sortedTypes = Object.keys(counts).sort((a, b) => counts[b] - counts[a]);
        setAvailableRelationTypes(sortedTypes);

        // Generate Color Map
        const typeColorMap: Record<string, string> = {};
        sortedTypes.forEach((type, index) => {
            typeColorMap[type] = RELATION_COLORS[index % RELATION_COLORS.length];
        });

        // Initialize filters if this is the first load for this source
        if (activeSource?.id && !sourceFilters[activeSource.id]) {
            initializeSourceFilters(activeSource.id, sortedTypes);
        }

        // Category Color Map (Hardcoded for semantic consistency)
        const CATEGORY_COLORS: Record<string, string> = {
            "CAUSAL": "#ef4444",      // Red
            "METHODOLOGY": "#3b82f6", // Blue
            "BIBLIOGRAPHIC": "#94a3b8", // Grey (slate-400)
            "ONTOLOGY": "#22c55e",    // Green
        };

        let filteredEdges = data.edges.map((edge: any) => {
            // Priority: 1. Category Color, 2. Dynamic Label Color, 3. Default
            let color = DEFAULT_EDGE_COLOR;
            if (edge.category) {
                // Remove leading colon if present in backend export (e.g. ":CAUSAL" -> "CAUSAL")
                const catKey = edge.category.replace(/^:/, '').toUpperCase();
                color = CATEGORY_COLORS[catKey] || typeColorMap[edge.label] || DEFAULT_EDGE_COLOR;
            } else {
                color = typeColorMap[edge.label] || DEFAULT_EDGE_COLOR;
            }
            // Only override color if it's NOT an inference (which uses purple dashed)
            // Or maybe we want to color code EVERYTHING by type now?
            // The user asked to "change dynamically all relations of that type to a certain color".
            // Let's apply the type color. Inference edges are distinguished by style (dashed).
            
            return {
                ...edge,
                style: { 
                    ...edge.style, 
                    stroke: color,
                    strokeWidth: 2 
                },
                markerEnd: {
                    type: 'arrowclosed',
                    color: color
                },
                // Store color in data for easy access if needed
                data: { ...edge.data, color } 
            };
        });

        // 1. Filter edges
        if (!showRelations) {
          filteredEdges = [];
        } else {
             // Explicit filtering: Only show edges whose type is in selectedRelationTypes
             // If selectedRelationTypes is [], we show NO edges.
             // But we need to handle the "initial load" case where activeFilters might be empty before the effect runs.
             // Actually, initializeSourceFilters above will trigger re-render with all selected.
             // For this specific render pass (before store update reflects), we might want to be careful.
             // However, since we react to `selectedRelationTypes`, and it starts [], showing nothing is technically correct until initialized.
             // To avoid "flash of no edges", we could default to "All" if uninitialized, but we made it explicit.
             // Let's rely on the store update.
             
             // BUT: The very first time execution reaches here, selectedRelationTypes is [], 
             // initializeSourceFilters is called but state update is async/next-tick in React loop.
             // So for this frame, edges are hidden?
             // Yes. That's acceptable for a split second, or we can use a local 'isInitialized' check?
             // Let's just use the standard filter:
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
        // Only fit view if this is a new source load (id changed)
        // This prevents zoom reset when just toggling filters
        if (activeSource?.id !== lastSourceIdRef.current) {
            lastSourceIdRef.current = activeSource?.id || null;
            setTimeout(() => fitView({ duration: 800 }), 50);
        }
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
    fitView,
    activeSourceId,
    sourceFilters,
    initializeSourceFilters
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

