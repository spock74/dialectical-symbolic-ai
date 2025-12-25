import { useRef, useState, type ChangeEvent, type DragEvent } from "react"
import { 
  Folder, 
  FileText, 
  Plus, 
  Upload, 
  Clock, 
  File,
  Loader2
} from "lucide-react"
import { uploadPdf, uploadPdfMultimodal, chat } from "../api"
import type { KnowledgeBase, Source } from "../types"
import { useDialecticStore } from "../store/useStore"
import { Button } from "./ui/button"
import { Card, CardHeader, CardTitle } from "./ui/card"
import { ScrollArea } from "./ui/scroll-area"
import { Checkbox } from "./ui/checkbox"
import { Input } from "./ui/input"
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
  const [isDragging, setIsDragging] = useState(false);

  const { 
    groups, 
    activeGroupId, 
    activeSourceId, 
    createGroup, 
    setActiveGroup, 
    addSourceToActiveGroup,
    setActiveSource,
    incrementGraphVersion,
    addMessage,
    setLastReasoningLogs
  } = useDialecticStore();

  const activeGroup = groups.find(g => g.id === activeGroupId);

  const handleCreateGroup = () => {
    if (!newGroupName.trim()) return;
    createGroup(newGroupName.trim());
    setNewGroupName("");
    setIsCreatingGroup(false);
  };

  const readFileContent = (file: File): Promise<string> => {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = (e) => resolve(e.target?.result as string);
      reader.onerror = (e) => reject(e);
      reader.readAsText(file);
    });
  };

  const processFile = async (file: File) => {
    if (!activeGroupId) return;
    try {
      setLoading(true);
      const fileType = file.name.split('.').pop()?.toLowerCase();
      
      let result: KnowledgeBase | null = null;
      
      if (fileType === 'pdf') {
        result = useMultimodal 
          ? await uploadPdfMultimodal(file) 
          : await uploadPdf(file);
      } else if (fileType === 'md' || fileType === 'txt') {
        // MD/TXT integration via Chat as requested
        const content = await readFileContent(file);
        const prompt = `Analise o seguinte documento e extraia conhecimento estruturado:\n\n---\nArquivo: ${file.name}\nConteúdo:\n${content}\n---`;
        
        addMessage({ role: 'user', content: `[Arquivo carregado: ${file.name}]` });
        
        const { useConversationalMemory, useBypassSDialect } = useDialecticStore.getState();
        const chatResponse = await chat(prompt, [], useConversationalMemory, useBypassSDialect, file.name);
        
        addMessage({ role: 'model', content: chatResponse.text });
        if (chatResponse.reasoningLogs) {
           setLastReasoningLogs(chatResponse.reasoningLogs);
        }
      }
      
      const newSource: Source = {
          id: crypto.randomUUID(),
          name: file.name,
          type: (fileType as any) || 'pdf',
          uploadDate: new Date().toISOString(),
          hash: "simulated-hash",
          active: true
      };
      
      addSourceToActiveGroup(newSource);

      if (result) {
        onKnowledgeLoaded(result);
        incrementGraphVersion();
      }
    } catch (error) {
      console.error("Upload failed", error);
    } finally {
      setLoading(false);
    }
  };

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;
    if (files && activeGroupId) {
      for (const file of Array.from(files)) {
        await processFile(file);
      }
      if (fileInputRef.current) fileInputRef.current.value = "";
    }
  };

  const handleDragOver = (e: DragEvent) => {
    e.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = (e: DragEvent) => {
    e.preventDefault();
    setIsDragging(false);
  };

  const handleDrop = async (e: DragEvent) => {
    e.preventDefault();
    setIsDragging(false);
    if (!activeGroupId) return;

    const files = e.dataTransfer.files;
    if (files.length > 0) {
      for (const file of Array.from(files)) {
        await processFile(file);
      }
    }
  };

  return (
    <div className="flex flex-col h-full border-r border-border bg-[#0a0a0a] min-w-[280px] select-none">
        {/* SECTION 1: GROUPS (Independent Scroll) */}
        <div className="flex flex-col h-[45%] border-b border-border">
          <div className="p-4 flex items-center justify-between shrink-0">
              <h2 className="text-xs font-bold uppercase tracking-widest text-muted-foreground/80">Trilhas de Estudo</h2>
              <Button 
                  variant="ghost" 
                  size="icon" 
                  className="h-6 w-6 hover:bg-primary/20 hover:text-primary transition-colors" 
                  onClick={() => setIsCreatingGroup(!isCreatingGroup)}
              >
                  <Plus size={16} />
              </Button>
          </div>

          {isCreatingGroup && (
            <div className="px-4 pb-3 animate-in fade-in slide-in-from-top-1">
              <div className="flex gap-2">
                <Input 
                    autoFocus
                    value={newGroupName}
                    onChange={(e) => setNewGroupName(e.target.value)}
                    placeholder="Nome da trilha..."
                    className="h-8 text-xs bg-muted/20 border-border/50"
                    onKeyDown={(e) => e.key === 'Enter' && handleCreateGroup()}
                />
                <Button size="sm" className="h-8" onClick={handleCreateGroup} disabled={!newGroupName.trim()}>Criar</Button>
              </div>
            </div>
          )}

          <ScrollArea className="flex-1 px-4 pb-4">
              <div className="space-y-3">
                {groups.length === 0 && !isCreatingGroup && (
                  <div className="text-center py-10 opacity-30">
                    <Folder className="mx-auto mb-2" size={24} />
                    <p className="text-[10px]">Crie sua primeira trilha</p>
                  </div>
                )}
                {groups.map(g => (
                  <Card 
                    key={g.id}
                    className={cn(
                      "cursor-pointer transition-all duration-200 border-border/50 hover:border-primary/40 bg-muted/5",
                      activeGroupId === g.id && "border-blue-500 ring-1 ring-blue-500/50 shadow-[0_0_15px_rgba(59,130,246,0.1)]"
                    )}
                    onClick={() => setActiveGroup(g.id)}
                  >
                    <CardHeader className="p-3 space-y-1">
                      <div className="flex items-center justify-between">
                        <CardTitle className="text-sm font-medium truncate pr-2">{g.name}</CardTitle>
                        {activeGroupId === g.id && (
                          <div className="h-1.5 w-1.5 rounded-full bg-blue-500 animate-pulse" />
                        )}
                      </div>
                      <div className="flex items-center gap-1.5 text-[10px] text-muted-foreground">
                        <Clock size={10} />
                        <span>Criado em {g.createdAt}</span>
                      </div>
                    </CardHeader>
                  </Card>
                ))}
              </div>
          </ScrollArea>
        </div>

        {/* SECTION 2: DROP ZONE & SOURCES (Independent Scroll) */}
        <div className="flex flex-col flex-1 min-h-0 relative">
          {activeGroup ? (
            <>
              {/* Drag & Drop Zone */}
              <div 
                className={cn(
                  "m-4 p-6 border-2 border-dashed rounded-xl transition-all duration-300 flex flex-col items-center justify-center gap-2 text-center group",
                  isDragging 
                    ? "border-blue-500 bg-blue-500/10 scale-[0.98]" 
                    : "border-border/40 bg-muted/5 hover:border-primary/30",
                  loading && "opacity-50 pointer-events-none"
                )}
                onDragOver={handleDragOver}
                onDragLeave={handleDragLeave}
                onDrop={handleDrop}
              >
                {loading ? (
                  <Loader2 className="h-8 w-8 animate-spin text-primary" />
                ) : (
                  <Upload className={cn("h-8 w-8 transition-colors", isDragging ? "text-blue-500" : "text-muted-foreground/40 group-hover:text-primary/60")} />
                )}
                <div className="space-y-1">
                  <p className="text-xs font-medium">
                    {loading ? "Processando..." : (isDragging ? "Solte agora!" : "Arraste seus arquivos")}
                  </p>
                  <p className="text-[10px] text-muted-foreground">PDF, Markdown ou TXT</p>
                </div>
                {!loading && (
                   <Button 
                    variant="link" 
                    size="sm" 
                    className="text-[10px] h-auto p-0 text-primary/60 hover:text-primary"
                    onClick={() => fileInputRef.current?.click()}
                   >
                     ou clique para selecionar
                   </Button>
                )}
                <input 
                  type="file" 
                  ref={fileInputRef} 
                  className="hidden" 
                  multiple
                  accept=".pdf,.md,.txt"
                  onChange={handleFileChange}
                />
              </div>

              {/* Multimodal Toggle */}
              <div className="px-4 mb-4 flex items-center justify-between">
                <label className="flex items-center gap-2 cursor-pointer group">
                  <Checkbox 
                    id="multimodal" 
                    checked={useMultimodal} 
                    onCheckedChange={(checked) => setUseMultimodal(!!checked)}
                    className="h-3.5 w-3.5"
                  />
                  <span className="text-[10px] text-muted-foreground group-hover:text-foreground transition-colors">Usar Visão Multimodal</span>
                </label>
              </div>

              {/* Sources Area */}
              <div className="px-4 pb-2 text-[10px] font-bold uppercase tracking-widest text-muted-foreground/60 flex items-center justify-between">
                <span>Fontes de Conhecimento</span>
                <span className="bg-muted px-1.5 rounded-full text-[9px]">{activeGroup.sources.length}</span>
              </div>

              <ScrollArea className="flex-1 px-4 pb-4">
                <div className="space-y-1.5">
                  {activeGroup.sources.length === 0 && (
                    <div className="py-10 text-center opacity-20">
                      <File className="mx-auto mb-2" size={20} />
                      <p className="text-[10px]">Nativa fonte encontrada</p>
                    </div>
                  )}
                  {activeGroup.sources.map((source) => (
                    <div 
                      key={source.id}
                      className={cn(
                        "group flex items-center gap-3 p-2.5 rounded-lg border border-transparent transition-all hover:bg-muted/30",
                        activeSourceId === source.id && "bg-primary/5 border-primary/20",
                        !source.active && "opacity-50"
                      )}
                    >
                      <Checkbox 
                        checked={activeSourceId === source.id}
                        onCheckedChange={() => setActiveSource(source.id === activeSourceId ? null : source.id)}
                        className="h-3.5 w-3.5 mt-0.5"
                      />
                      <div 
                        className="flex-1 min-w-0 cursor-pointer"
                        onClick={() => setActiveSource(source.id === activeSourceId ? null : source.id)}
                      >
                        <div className="flex items-center gap-2 mb-0.5">
                          <FileText size={12} className={cn(activeSourceId === source.id ? "text-primary" : "text-muted-foreground")} />
                          <span className={cn(
                            "text-xs truncate",
                            activeSourceId === source.id && "font-medium text-primary"
                          )}>
                            {source.name}
                          </span>
                        </div>
                        <div className="flex items-center gap-2 text-[9px] text-muted-foreground/70">
                          <code className="bg-muted px-1 rounded uppercase">{source.type}</code>
                          <span>•</span>
                          <span className="font-mono opacity-60">#{source.hash.substring(0, 8)}</span>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              </ScrollArea>
            </>
          ) : (
            <div className="flex-1 flex flex-col items-center justify-center p-8 text-center animate-in fade-in zoom-in duration-500">
               <div className="h-16 w-16 rounded-full bg-muted/20 flex items-center justify-center mb-4">
                 <Folder size={24} className="text-muted-foreground/40" />
               </div>
               <h3 className="text-sm font-medium mb-1">Nenhuma trilha ativa</h3>
               <p className="text-[10px] text-muted-foreground max-w-[150px]">
                 Selecione uma trilha acima ou crie uma nova para gerenciar suas fontes.
               </p>
            </div>
          )}
        </div>
    </div>
  )
}
