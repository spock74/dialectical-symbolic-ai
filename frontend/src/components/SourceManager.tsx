import { useRef, useState, type ChangeEvent } from "react"
import { Folder, FileText, Plus, Upload, Check, ChevronDown } from "lucide-react"
import { uploadPdf, uploadPdfMultimodal } from "../api"
import type { KnowledgeBase, Source } from "../types"
import { useDialecticStore } from "../store/useStore"
import { Button } from "./ui/button"
import { cn } from "@/lib/utils"

interface SourceManagerProps {
  onKnowledgeLoaded: (kb: KnowledgeBase) => void;
}

export function SourceManager({ onKnowledgeLoaded }: SourceManagerProps) {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [useMultimodal, setUseMultimodal] = useState(false);
  const [loading, setLoading] = useState(false);
  const [isCreatingGroup, setIsCreatingGroup] = useState(false);
  const [newGroupName, setNewGroupName] = useState("");

  const { 
    groups, 
    activeGroupId, 
    activeSourceId, 
    createGroup, 
    setActiveGroup, 
    addSourceToActiveGroup,
    setActiveSource
  } = useDialecticStore();

  const activeGroup = groups.find(g => g.id === activeGroupId);

  const handleCreateGroup = () => {
    if (!newGroupName.trim()) return;
    createGroup(newGroupName.trim());
    setNewGroupName("");
    setIsCreatingGroup(false);
  };

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file && activeGroupId) {
      try {
        setLoading(true);
        console.log(`Uploading... ${file.name} (Multimodal: ${useMultimodal})`);
        
        // 1. Upload & Process
        const result = useMultimodal 
          ? await uploadPdfMultimodal(file) 
          : await uploadPdf(file);
        
        // 2. Add to Store
        const newSource: Source = {
            id: crypto.randomUUID(),
            name: file.name,
            type: 'pdf',
            uploadDate: new Date().toISOString()
        };
        addSourceToActiveGroup(newSource);

        console.log("Uploaded!", result);
        onKnowledgeLoaded(result);
      } catch (error) {
        console.error("Upload failed", error);
        alert("Failed to upload/extract PDF. Check console.");
      } finally {
        setLoading(false);
        // Reset input
        if (fileInputRef.current) fileInputRef.current.value = "";
      }
    }
  };

  return (
    <div className="flex flex-col h-full border-r border-border bg-card min-w-0">
        {/* Header: Group Selection */}
        <div className="p-4 border-b border-border space-y-3">
            <div className="flex items-center justify-between">
                <h2 className="text-sm font-semibold uppercase tracking-wider text-muted-foreground">Trilhas de Estudo</h2>
                <Button 
                    variant="ghost" 
                    size="icon" 
                    className="h-6 w-6" 
                    onClick={() => setIsCreatingGroup(!isCreatingGroup)}
                    title="Nova Trilha"
                >
                    <Plus size={14} />
                </Button>
            </div>

            {isCreatingGroup ? (
                <div className="flex gap-2 animate-in fade-in slide-in-from-top-1">
                    <input 
                        autoFocus
                        value={newGroupName}
                        onChange={(e) => setNewGroupName(e.target.value)}
                        placeholder="Nome da Trilha..."
                        className="flex h-8 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                        onKeyDown={(e) => e.key === 'Enter' && handleCreateGroup()}
                    />
                    <Button size="sm" onClick={handleCreateGroup} disabled={!newGroupName.trim()}>Add</Button>
                </div>
            ) : (
                <div className="relative">
                    <select 
                        className="flex h-9 w-full items-center justify-between rounded-md border border-input bg-background px-3 py-2 text-sm shadow-sm ring-offset-background placeholder:text-muted-foreground focus:outline-none focus:ring-1 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50 appearance-none"
                        value={activeGroupId || ""}
                        onChange={(e) => setActiveGroup(e.target.value)}
                    >
                        <option value="" disabled>Selecione uma trilha...</option>
                        {groups.map(g => (
                            <option key={g.id} value={g.id}>{g.name}</option>
                        ))}
                    </select>
                    <ChevronDown className="absolute right-3 top-3 h-4 w-4 opacity-50 pointer-events-none" />
                </div>
            )}
        </div>

      <div className="flex flex-col p-4 gap-4 flex-1 overflow-hidden">
        {activeGroup ? (
            <>
                {/* Upload Controls */}
                <div className="flex flex-col gap-2 p-3 bg-muted/30 rounded-lg border border-border/50">
                    <div className="flex items-center justify-between">
                        <span className="text-sm font-medium">Adicionar Fonte</span>
                        <div className="flex gap-1">
                            <input 
                            type="file" 
                            ref={fileInputRef} 
                            className="hidden" 
                            accept="application/pdf"
                            onChange={handleFileChange}
                            />
                            <Button 
                            size="sm"
                            variant="secondary"
                            className="h-7 text-xs gap-1"
                            onClick={() => fileInputRef.current?.click()}
                            disabled={loading}
                            >
                            {loading ? (
                                <div className="animate-spin h-3 w-3 border-2 border-primary border-t-transparent rounded-full" />
                            ) : (
                                <Upload size={12} />
                            )}
                            Upload PDF
                            </Button>
                        </div>
                    </div>
                    
                    {/* Toggle Mode */}
                    <label className="flex items-center gap-2 cursor-pointer select-none mt-1">
                        <input 
                        type="checkbox" 
                        checked={useMultimodal} 
                        onChange={(e) => setUseMultimodal(e.target.checked)}
                        className="rounded border-gray-300 text-primary focus:ring-primary h-3 w-3 accent-primary"
                        />
                        <span className="text-xs text-muted-foreground">Usar Visão (Multimodal)</span>
                    </label>

                    {loading && (
                        <div className="text-xs text-primary animate-pulse pt-1">
                            {useMultimodal ? "👁️ Analisando visualmente..." : "📄 Extraindo conhecimento..."}
                        </div>
                    )}
                </div>

                {/* Sources List */}
                <div className="space-y-1 flex-1 overflow-y-auto min-h-0">
                    <h3 className="text-xs font-semibold text-muted-foreground mb-2 uppercase">Fontes Disponíveis</h3>
                    {activeGroup.sources.length === 0 && (
                        <div className="text-center py-8 text-sm text-muted-foreground italic">
                            Nenhuma fonte nesta trilha.
                        </div>
                    )}
                    {activeGroup.sources.map((source) => (
                        <div 
                            key={source.id}
                            className={cn(
                                "flex items-center gap-2 p-2 rounded-md text-sm transition-colors cursor-pointer border border-transparent",
                                activeSourceId === source.id 
                                    ? "bg-primary/10 border-primary/20 text-primary font-medium" 
                                    : "hover:bg-accent/50 text-foreground"
                            )}
                            onClick={() => setActiveSource(source.id === activeSourceId ? null : source.id)}
                        >
                            <div className={cn(
                                "flex h-4 w-4 items-center justify-center rounded border transition-colors",
                                activeSourceId === source.id ? "bg-primary border-primary" : "border-muted-foreground/30"
                            )}>
                                {activeSourceId === source.id && <Check size={10} className="text-primary-foreground" />}
                            </div>
                            <FileText size={14} className="opacity-70" />
                            <span className="truncate">{source.name}</span>
                        </div>
                    ))}
                </div>
            </>
        ) : (
            <div className="flex flex-col items-center justify-center h-full text-center p-4 text-muted-foreground space-y-2">
                <Folder size={32} className="opacity-20" />
                <p className="text-sm">Selecione ou crie uma<br/>Trilha de Estudo para começar.</p>
            </div>
        )}
      </div>
    </div>
  )
}
