import { useState } from "react"
import { ChatInterface } from "./components/ChatInterface"
import { SourceManager } from "./components/SourceManager"
import { ThemeToggle } from "./components/ThemeToggle"
import { Workspace } from "./components/Workspace"
import { ReplInterface } from "./components/ReplInterface"
import { ResizableHandle, ResizablePanel, ResizablePanelGroup } from "@/components/ui/resizable"
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs"
import * as TabsPrimitive from "@radix-ui/react-tabs"
import type { KnowledgeBase } from "./types"

function App() {
  const [knowledgeBase, setKnowledgeBase] = useState<KnowledgeBase | null>(null);

  const handleKnowledgeLoaded = (kb: KnowledgeBase) => {
    setKnowledgeBase(kb);
  };

  return (
    <div className="h-screen w-screen overflow-hidden bg-background text-foreground">
       <ResizablePanelGroup orientation="horizontal" className="h-full w-full">
         {/* Left Panel: Sources */}
         <ResizablePanel defaultSize={20} minSize={10} className="border-r border-border bg-card">
            <SourceManager onKnowledgeLoaded={handleKnowledgeLoaded} />
         </ResizablePanel>
         
         <ResizableHandle withHandle />
         
         {/* Center Panel: Chat & Graph Tabs */}
         <ResizablePanel defaultSize={60} minSize={30}>
            <Tabs defaultValue="chat" className="h-full flex flex-col">
              <div className="border-b px-4 py-2 bg-muted/40 flex items-center justify-between shrink-0 h-14">
                <div className="flex items-center gap-2">
            <h1 className="text-xl font-bold">SDialectic</h1>
            <span className="text-xs bg-zinc-800 px-2 py-0.5 rounded text-zinc-400">Genkit + Lisp Core</span>
          </div>
                
                <TabsList className="grid w-full max-w-[400px] grid-cols-2">
                  <TabsTrigger value="chat">Chat</TabsTrigger>
                  <TabsTrigger value="graph">Graph Analysis</TabsTrigger>
                </TabsList>
                
                <div className="flex items-center gap-2">
                   <ThemeToggle />
                </div>
              </div>
              
              <div className="flex-1 overflow-hidden relative">
                {/* 
                  Instead of TabsContent which unmounts, we use a hidden class approach 
                  to preserve the internal state (scroll, input) of ChatInterface.
                */}
                <TabsPrimitive.Content value="chat" className="h-full m-0 p-0 border-0 data-[state=active]:flex flex-col data-[state=inactive]:hidden">
                   <ChatInterface />
                </TabsPrimitive.Content>
                <TabsPrimitive.Content value="graph" className="h-full m-0 p-0 border-0 data-[state=active]:block data-[state=inactive]:hidden">
                   <Workspace knowledgeBase={knowledgeBase} />
                </TabsPrimitive.Content>
              </div>
            </Tabs>
         </ResizablePanel>
         
         <ResizableHandle withHandle />
         
         {/* Right Panel: Tools (Live REPL) */}
         <ResizablePanel defaultSize={20} minSize={10} className="border-l border-border bg-muted/5 flex flex-col">
             <ReplInterface />
         </ResizablePanel>
       </ResizablePanelGroup>
    </div>
  )
}

export default App
