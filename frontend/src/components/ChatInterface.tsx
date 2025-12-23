import { useState, useRef, useEffect } from "react";
import { Paperclip, Loader2, Sparkles, Brain, Clock, ChevronUp, Bot, User, FileText, Trash2, Database } from "lucide-react";
import { chat, extractMarkdown, resetKnowledge } from "../api";
import { cn } from "../lib/utils";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Avatar, AvatarFallback } from "@/components/ui/avatar";
import { Checkbox } from "@/components/ui/checkbox";
import { useDialecticStore } from "@/store/useStore";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from "@/components/ui/dialog";

interface Message {
  role: 'user' | 'model' | 'tool';
  content: string;
}

export function ChatInterface() {
  /* Global Store State */
  const { 
    groups, 
    activeGroupId, 
    activeSourceId,
    messages,
    addMessage,
    clearMessages,
    useConversationalMemory,
    setUseConversationalMemory,
    incrementGraphVersion
  } = useDialecticStore();

  const [input, setInput] = useState("");
  const [loading, setLoading] = useState(false);
  const [isUploading, setIsUploading] = useState(false);
  const [isResetting, setIsResetting] = useState(false);
  const [resetDialogOpen, setResetDialogOpen] = useState(false);
  const fileInputRef = useRef<HTMLInputElement>(null);
  const textareaRef = useRef<HTMLTextAreaElement>(null);
  const bottomRef = useRef<HTMLDivElement>(null);

  const activeGroup = groups.find(g => g.id === activeGroupId);
  const activeSource = activeGroup?.sources.find(s => s.id === activeSourceId);

  const adjustHeight = () => {
    if (textareaRef.current) {
      textareaRef.current.style.height = 'auto';
      textareaRef.current.style.height = `${textareaRef.current.scrollHeight}px`;
    }
  };

  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [messages, loading]);

  useEffect(() => {
    if (input === "") {
        adjustHeight();
    }
  }, [input]);

  const handleSend = async () => {
    if (!input.trim() || loading) return;

    let promptToSend = input;
    // Inject Context if Source is Active
    if (activeSource) {
        promptToSend = `[Context: Focused on Source "${activeSource.name}"]\n${input}`;
    }

    const userMsg: Message = { role: 'user', content: input }; 
    addMessage(userMsg); // Add to store
    setInput("");
    setLoading(true);

    try {
      const { useConversationalMemory } = useDialecticStore.getState();
      const history = messages.map(m => ({ role: m.role, content: m.content }));
      
      const response = await chat(promptToSend, history, useConversationalMemory);
      addMessage({ role: 'model', content: response.text });
    } catch (error) {
      console.error(error);
      addMessage({ role: 'tool', content: `Error: ${String(error)}` });
    } finally {
      setLoading(false);
    }
  };

  const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
    if (!e.target.files?.length) return;
    const file = e.target.files[0];
    setIsUploading(true);
    try {
      const result = await extractMarkdown(file);
      const knowledgeSummary = `Analyzed ${file.name}. Extracted ${result.knowledgeBase.length} concepts.`;
      addMessage({ role: 'user', content: `[Uploaded ${file.name}]` });
      addMessage({ role: 'tool', content: knowledgeSummary + `\n\n(Context added to backend memory)` });
    } catch (error) {
      console.error(error);
      addMessage({ role: 'tool', content: `Upload Error: ${String(error)}` });
    } finally {
      setIsUploading(false);
      if (fileInputRef.current) fileInputRef.current.value = '';
    }
  };

  const handleResetKB = async () => {
    setIsResetting(true);
    try {
      await resetKnowledge();
      clearMessages(); // Full wipe as per user requirement "zerar o Knowledge Base" and context of cleaning up
      incrementGraphVersion();
      addMessage({ 
        role: 'tool', 
        content: "Knowledge Base e histórico de chat resetados com sucesso. O motor semântico está limpo." 
      });
      setResetDialogOpen(false);
    } catch (error) {
      console.error(error);
      addMessage({ role: 'tool', content: `Erro ao resetar: ${String(error)}` });
    } finally {
      setIsResetting(false);
    }
  };

  return (
    <div className="flex flex-col h-full bg-background/50 backdrop-blur-sm shadow-sm overflow-hidden">
      {/* Header removed - moved to App.tsx */}

      {/* Messages */}
      <ScrollArea className="flex-1 p-4">
        <div className="space-y-6 pr-2">
          {messages.length === 0 && (
            <div className="h-full flex flex-col items-center justify-center text-muted-foreground opacity-50 space-y-4 py-10">
              <div className="p-4 bg-muted/50 rounded-full">
                  <Sparkles size={32} className="text-primary" />
              </div>
              <p className="text-sm text-center max-w-[200px]">
                I can help you study Lisp, extract knowledge from Markdown, and reason about code.
              </p>
            </div>
          )}
          
          {messages.map((msg, idx) => (
            <div 
              key={idx} 
              className={cn(
                "flex w-full gap-3", 
                msg.role === 'user' ? "flex-row-reverse" : "flex-row"
              )}
            >
               <Avatar className="h-8 w-8 mt-1 border border-border/50">
                  <AvatarFallback className={cn("text-[10px]", msg.role === 'user' ? "bg-primary text-primary-foreground" : "bg-muted text-foreground")}>
                    {msg.role === 'user' ? <User size={14} /> : <Bot size={14}/>}
                  </AvatarFallback>
               </Avatar>
               
               <div className={cn(
                 "max-w-[85%] rounded-2xl px-4 py-3 text-sm shadow-sm",
                 msg.role === 'user' 
                  ? "bg-primary text-primary-foreground rounded-tr-sm" 
                  : msg.role === 'tool' 
                    ? "bg-secondary text-secondary-foreground font-mono text-xs border border-border rounded-tl-sm ring-1 ring-ring/5"
                    : "bg-background border border-border rounded-tl-sm text-foreground"
               )}>
                  <div className="whitespace-pre-wrap leading-relaxed">{msg.content}</div>
               </div>
            </div>
          ))}
          
          {loading && (
            <div className="flex w-full gap-3">
               <Avatar className="h-8 w-8 mt-1 border border-border/50">
                  <AvatarFallback className="bg-muted text-foreground"><Bot size={14}/></AvatarFallback>
               </Avatar>
               <div className="bg-background border border-border rounded-2xl rounded-tl-sm px-4 py-3 shadow-sm flex items-center gap-2">
                 <Loader2 className="animate-spin h-3 w-3 text-primary" />
                 <span className="text-xs text-muted-foreground animate-pulse">Processing...</span>
               </div>
            </div>
          )}
          <div ref={bottomRef} className="h-2" />
        </div>
      </ScrollArea>

      {/* Input Area - Inspiration Style */}
      <div className="p-4 bg-card/50 backdrop-blur-sm">
         {/* Context Pills */}
         <div className="flex items-center gap-2 mb-3 overflow-x-auto pb-1 scrollbar-hide">
            {activeSource && (
                <Button variant="secondary" size="sm" className="h-7 text-xs gap-1 rounded-full bg-primary/10 text-primary border border-primary/20">
                    <FileText size={12} />
                    Context: {activeSource.name}
                </Button>
            )}
            
            {/* Knowledge Base Reset Dialog */}
            <Dialog open={resetDialogOpen} onOpenChange={setResetDialogOpen}>
              <DialogTrigger asChild>
                <Button variant="outline" size="sm" className="h-7 text-xs gap-1 rounded-full border-dashed bg-background/50 hover:bg-destructive/10 hover:text-destructive hover:border-destructive/40 transition-colors">
                   <Database size={12} />
                   Knowledge Base
                </Button>
              </DialogTrigger>
              <DialogContent className="sm:max-w-[425px] border-destructive/20 shadow-2xl bg-background/95 backdrop-blur-xl">
                <DialogHeader>
                  <DialogTitle className="flex items-center gap-2 text-destructive">
                    <Trash2 size={18} />
                    Resetar Base de Conhecimento
                  </DialogTitle>
                  <DialogDescription className="pt-2">
                    Esta ação irá <strong>apagar permanentemente</strong> todos os dados do motor semântico (Lisp) e limpar o histórico de chat. Esta ação não pode ser desfeita.
                  </DialogDescription>
                </DialogHeader>
                <DialogFooter className="mt-4 gap-2 sm:gap-0">
                  <Button variant="outline" onClick={() => setResetDialogOpen(false)} disabled={isResetting}>
                    Cancelar
                  </Button>
                  <Button variant="destructive" onClick={handleResetKB} disabled={isResetting} className="gap-2">
                    {isResetting && <Loader2 className="h-4 w-4 animate-spin" />}
                    Confirmar Reset
                  </Button>
                </DialogFooter>
              </DialogContent>
            </Dialog>

            <Button variant="outline" size="sm" className="h-7 text-xs gap-1 rounded-full border-dashed bg-background/50">
               <Brain size={12} />
               Raciocínio
            </Button>
            
            <div className="flex items-center gap-2 bg-background/50 px-3 py-1 h-7 rounded-full border border-dashed border-border shadow-sm group hover:border-primary/40 transition-colors">
               <Checkbox 
                 id="input-memory-toggle" 
                 checked={useConversationalMemory} 
                 onCheckedChange={(checked) => setUseConversationalMemory(!!checked)}
                 className="h-3.5 w-3.5 data-[state=checked]:bg-primary data-[state=checked]:border-primary"
               />
               <label 
                 htmlFor="input-memory-toggle" 
                 className="text-[11px] font-medium cursor-pointer select-none text-muted-foreground group-hover:text-primary transition-colors flex items-center gap-1"
               >
                 <Clock size={11} className="opacity-70" />
                 Memória
               </label>
            </div>
         </div>

         {/* Main Input Container */}
         <div className="relative flex items-center bg-input/10 border border-input focus-within:border-primary/50 rounded-xl transition-all shadow-sm">
             <Button
               variant="ghost"
               size="icon"
               onClick={() => fileInputRef.current?.click()}
               disabled={loading || isUploading}
               title="Upload Context"
               className="h-10 w-10 text-muted-foreground hover:text-foreground shrink-0 rounded-l-xl"
             >
                {isUploading ? <Loader2 className="h-4 w-4 animate-spin" /> : <Paperclip className="h-4 w-4" />}
             </Button>
             
             <input 
                type="file" 
                ref={fileInputRef} 
                className="hidden" 
                accept=".md,.txt" 
                onChange={handleFileUpload} 
             />
             <textarea
               ref={textareaRef}
               rows={1}
               className="flex-1 bg-transparent border-none px-2 py-3 text-sm focus:outline-none placeholder:text-muted-foreground/70 resize-none max-h-40 overflow-y-auto"
               placeholder={isUploading ? "Uploading context..." : "Escolha uma sugestão ou digite sua pergunta..."}
               value={input}
               onChange={(e) => {
                 setInput(e.target.value);
                 adjustHeight();
               }}
               onKeyDown={(e) => {
                 if (e.key === 'Enter' && !e.shiftKey) {
                   e.preventDefault();
                   handleSend();
                 }
               }}
               disabled={loading || isUploading}
             />
             
             <Button
               size="icon"
               onClick={handleSend}
               disabled={!input.trim() || loading || isUploading}
               className={cn(
                 "h-8 w-8 mr-1 mb-1 self-end rounded-lg transition-all", 
                 input.trim() ? "bg-primary text-primary-foreground shadow-md" : "bg-muted text-muted-foreground"
               )}
             >
                <div className={cn("transition-transform", loading && "scale-0 absolute")}>
                   <ChevronUp size={18} strokeWidth={3} />
                </div>
                {loading && <Loader2 className="h-4 w-4 animate-spin absolute" />}
             </Button>
         </div>
         <div className="mt-2 text-[10px] text-center text-muted-foreground opacity-50">
            AI can make mistakes. Verify important information. Presione <kbd className="font-sans px-1 rounded bg-muted border border-border shadow-sm">Shift+Enter</kbd> para nova linha.
         </div>
      </div>
    </div>
  );
}
