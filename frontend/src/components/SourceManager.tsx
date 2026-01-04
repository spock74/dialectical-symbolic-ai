import { useState } from "react"
import { Plus, Folder } from "lucide-react"
import { uploadPdf, uploadPdfMultimodal, chat } from "../api"
import type { KnowledgeBase, Source } from "../types"
import { useDialecticStore } from "../store/useStore"
import { Button } from "./ui/button"
import { ScrollArea } from "./ui/scroll-area"
import { Checkbox } from "./ui/checkbox"
import { Input } from "./ui/input"

import { GroupItem } from "./GroupItem"

interface SourceManagerProps {
  onKnowledgeLoaded: (kb: KnowledgeBase) => void;
}

export function SourceManager({ onKnowledgeLoaded }: SourceManagerProps) {
  const [useMultimodal, setUseMultimodal] = useState(false);
  const [isProcessing, setIsProcessing] = useState(false);
  const [isCreatingGroup, setIsCreatingGroup] = useState(false);
  const [newGroupName, setNewGroupName] = useState("");

  const { 
    groups, 
    activeGroupId, 
    createGroup, 
    setActiveGroup, 
    addSourceToGroup,
    deleteGroup,
    incrementGraphVersion,
    addMessage,
    setLastReasoningLogs,
    activeSourceId,
    setActiveSource
  } = useDialecticStore();

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

  const processFile = async (file: File, targetGroupId?: string) => {
    // Fallback to activeGroupId if strict target not provided
    const effectiveGroupId = targetGroupId || activeGroupId;
    if (!effectiveGroupId) {
        console.error("Cannot upload: No active or target group selected.");
        return;
    }
    
    try {
      setIsProcessing(true);
      // Ensure the group we are uploading to is active (or becomes active)
      if (effectiveGroupId !== activeGroupId) {
          setActiveGroup(effectiveGroupId);
      }
      const fileType = file.name.split('.').pop()?.toLowerCase();
      
      let result: KnowledgeBase | null = null;
      
      if (fileType === 'pdf') {
        result = useMultimodal 
          ? await uploadPdfMultimodal(file) 
          : await uploadPdf(file);
      } else if (fileType === 'md' || fileType === 'txt') {
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
      
      addSourceToGroup(effectiveGroupId, newSource);

      if (result) {
        onKnowledgeLoaded(result);
        incrementGraphVersion();
      }
    } catch (error) {
      console.error("Upload failed", error);
    } finally {
      setIsProcessing(false);
    }
  };

  return (
    <div className="flex flex-col h-full border-r border-border bg-[#0a0a0a] min-w-[280px] select-none">
        
        {/* SECTION 1: GROUPS LIST (Full Height) */}
        <div className="flex flex-col flex-1 min-h-0">
          <div className="p-4 py-3 flex items-center justify-between shrink-0 border-b border-border bg-muted/5">
              <h2 className="text-xs font-bold uppercase tracking-widest text-muted-foreground/80">
                  Trilhas de Estudo
              </h2>
              <div className="flex items-center gap-2">
                 <label className="flex items-center gap-2 cursor-pointer group mr-2">
                    <Checkbox 
                      id="multimodal" 
                      checked={useMultimodal} 
                      onCheckedChange={(checked) => setUseMultimodal(!!checked)}
                      className="h-3.5 w-3.5"
                    />
                    <span className="text-[10px] text-muted-foreground group-hover:text-foreground transition-colors">
                        Visão
                    </span>
                  </label>
                  <Button 
                      variant="ghost" 
                      size="icon" 
                      className="h-6 w-6 hover:bg-primary/20 hover:text-primary transition-colors" 
                      onClick={() => setIsCreatingGroup(!isCreatingGroup)}
                  >
                      <Plus size={16} />
                  </Button>
              </div>
          </div>

          {isCreatingGroup && (
            <div className="px-4 py-3 animate-in fade-in slide-in-from-top-1 bg-muted/5 border-b border-border/50">
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

          <ScrollArea className="flex-1 px-2 pb-2">
              <div className="space-y-2 mt-2">
                {groups.length === 0 && !isCreatingGroup && (
                  <div className="text-center py-10 opacity-30">
                    <Folder className="mx-auto mb-2" size={24} />
                    <p className="text-[10px]">Crie sua primeira trilha</p>
                  </div>
                )}
                
                {groups.map(g => (
                  <GroupItem 
                    key={g.id}
                    group={g}
                    isActive={activeGroupId === g.id}
                    onSelect={() => setActiveGroup(g.id)}
                    onDelete={() => deleteGroup(g.id)}
                    activeSourceId={activeSourceId}
                    onSelectSource={setActiveSource}
                    onFileUpload={processFile}
                    isProcessing={isProcessing}
                  />
                ))}
              </div>
          </ScrollArea>
        </div>
    </div>
  )
}
