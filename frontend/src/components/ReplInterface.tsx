import { useEffect, useRef, useState } from "react";
import { Terminal, Circle } from "lucide-react";

import { ScrollArea } from "@/components/ui/scroll-area";
import { Badge } from "@/components/ui/badge";
import { useDialecticStore } from "../store/useStore";

import { API_BASE_URL } from "../api";

interface LogEntry {
  timestamp: string;
  content: string;
}

export function ReplInterface() {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [isConnected, setIsConnected] = useState(false);
  const bottomRef = useRef<HTMLDivElement>(null);

  /* 
   * Optimization: Use Ref for buffer to prevent frequent re-renders on high-speed log streams.
   * Only update state every X ms. 
   */
  const bufferRef = useRef<LogEntry[]>([]);
  const isMounted = useRef(true);

  useEffect(() => {
    isMounted.current = true;
    
    // [DEBUG] Stream Re-enabled for Monitoring
    const eventSource = new EventSource(`${API_BASE_URL}/lisp-stream`);

    const setConnectionStatus = (status: string) => {
      setIsConnected(status === "Connected");
    };

    const addLog = (content: string) => {
      const newLogEntry = { timestamp: new Date().toISOString(), content };
      bufferRef.current.push(newLogEntry);
    };

    eventSource.onopen = () => {
      setConnectionStatus("Connected");
      addLog(";; Connected to System Stream");
    };

    eventSource.onmessage = (event: MessageEvent) => {
      try {
        const parsed = JSON.parse(event.data);
        addLog(parsed.content);
        
        // Detect state changes to refresh graph
        const content = parsed.content || "";
        if (
          content.includes("Memorizado:") || 
          content.includes("Relacao estruturada:") ||
          content.includes("Ferramenta aprendida:") ||
          content.includes("Estado carregado") ||
          content.includes("Auto-Saved Knowledge Graph") ||
          content.includes("Inferencia concluida")
        ) {
           useDialecticStore.getState().incrementGraphVersion();
        }
      } catch (e) {
        console.error("Failed to parse log", e);
      }
    };


    eventSource.onerror = () => {
      if (isMounted.current) setIsConnected(false);
      eventSource.close();
    };

    // Flux Buffer Interval
    const interval = setInterval(() => {
      if (bufferRef.current.length > 0) {
        setLogs(prev => {
          // Keep only last 1000 logs to prevent memory leak / DOM lag
          const newLogs = [...prev, ...bufferRef.current];
          bufferRef.current = []; // Clear buffer
          return newLogs.slice(-1000); 
        });
      }
    }, 200); // 5 updates per second max

    return () => {
      isMounted.current = false;
      clearInterval(interval);
      eventSource.close();
    };
  }, []);

  useEffect(() => {
    // Only scroll if we are near bottom? Or always?
    // For monitoring, typically always scroll.
    bottomRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [logs]);

  return (
    <div className="flex flex-col h-full bg-[#1e1e1e] text-[#f8f8f2] font-mono text-xs overflow-hidden rounded-md border border-border/20 shadow-inner">
      {/* Header */}
      <div className="flex items-center justify-between px-3 py-2 bg-[#2d2d2d] border-b border-white/10 shrink-0">
        <div className="flex items-center gap-2">
          <Terminal size={14} className="text-green-400" />
          <span className="font-semibold text-gray-300">SBCL REPL</span>
        </div>
        <Badge variant="outline" className={`h-5 gap-1 ${isConnected ? "text-green-400 border-green-400/30" : "text-red-400 border-red-400/30"} bg-transparent`}>
           <Circle size={6} fill="currentColor" />
           {isConnected ? "Connected" : "Disconnected"}
        </Badge>
      </div>

      {/* Terminal View */}
      <ScrollArea className="flex-1 p-3">
         <div className="space-y-1">
            {logs.map((log, i) => (
              <div key={i} className="break-all whitespace-pre-wrap leading-tight">
                 <span className="text-gray-500 mr-2 select-none">[{new Date(log.timestamp).toLocaleTimeString([], {hour12: false})}]</span>
                 <span className={
                    log.content.includes("ERR") ? "text-red-400" :
                    log.content.startsWith(";;") ? "text-gray-400 italic" :
                    log.content.startsWith("*") ? "text-yellow-400 font-bold" :
                    "text-[#f8f8f2]"
                 }>
                    {log.content}
                 </span>
              </div>
            ))}
            <div ref={bottomRef} />
         </div>
      </ScrollArea>
    </div>
  );
}
