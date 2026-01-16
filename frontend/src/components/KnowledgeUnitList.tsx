import { useEffect, useState } from "react";
import { Trash2, Power as PowerIcon, RefreshCw, FileText } from "lucide-react";
import { Button } from "./ui/button";
import { ScrollArea } from "./ui/scroll-area";
import { cn } from "@/lib/utils";
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
    AlertDialogTrigger,
  } from "@/components/ui/alert-dialog"

interface KnowledgeUnit {
    id: string;
    nodeCount: number;
    relationCount: number;
}

export function KnowledgeUnitList() {
    const [units, setUnits] = useState<KnowledgeUnit[]>([]);
    const [loadedUnits, setLoadedUnits] = useState<Set<string>>(new Set());
    const [loading, setLoading] = useState(false);

    const fetchUnits = async () => {
        try {
            const res = await fetch("http://localhost:3000/api/knowledge-units");
            if (res.ok) {
                const data = await res.json();
                setUnits(data);
            }
        } catch (e) {
            console.error("Failed to fetch units", e);
        }
    };

    useEffect(() => {
        fetchUnits();
    }, []);

    const toggleUnitLoad = async (id: string) => {
        setLoading(true);
        const isLoaded = loadedUnits.has(id);
        const endpoint = isLoaded ? "unload" : "load";
        
        try {
            const res = await fetch(`http://localhost:3000/api/knowledge-units/${id}/${endpoint}`, {
                method: "POST"
            });
            if (res.ok) {
                const newLoaded = new Set(loadedUnits);
                if (isLoaded) newLoaded.delete(id);
                else newLoaded.add(id);
                setLoadedUnits(newLoaded);
            }
        } catch (e) {
            console.error(`Failed to ${endpoint} unit ${id}`, e);
        } finally {
            setLoading(false);
        }
    };

    const deleteUnit = async (id: string, e: React.MouseEvent) => {
        e.stopPropagation();
        // Dialog already confirmed
        
        try {
            await fetch(`http://localhost:3000/api/knowledge-units/${id}`, {
                method: "DELETE"
            });
            fetchUnits();
            const newLoaded = new Set(loadedUnits);
            newLoaded.delete(id);
            setLoadedUnits(newLoaded);
        } catch (e) {
            console.error("Failed to delete unit", e);
        }
    };

    return (
        <div className="flex flex-col h-full">
            <div className="p-4 border-b border-border flex justify-between items-center">
                <h3 className="text-xs font-bold uppercase tracking-widest text-muted-foreground/80">Unidades de Conhecimento (Backend)</h3>
                <Button variant="ghost" size="icon" className="h-6 w-6" onClick={fetchUnits}>
                    <RefreshCw size={14} />
                </Button>
            </div>
            <ScrollArea className="flex-1 px-4 py-2">
                <div className="space-y-2">
                    {units.length === 0 && (
                         <div className="text-center py-6 opacity-30">
                            <FileText className="mx-auto mb-2" size={24} />
                            <p className="text-[10px]">Nenhuma unidade encontrada</p>
                        </div>
                    )}
                    {units.map(unit => {
                        const isLoaded = loadedUnits.has(unit.id);
                        return (
                            <div key={unit.id} className={cn(
                                "flex items-center justify-between p-3 rounded-lg border transition-all",
                                isLoaded ? "bg-primary/10 border-primary/30" : "bg-muted/5 border-border/50 hover:bg-muted/10"
                            )}>
                                <div className="flex items-center gap-3 min-w-0">
                                    <div 
                                        className={cn("w-2 h-2 rounded-full", isLoaded ? "bg-green-500 shadow-[0_0_8px_rgba(34,197,94,0.4)]" : "bg-muted-foreground/30")}
                                    />
                                    <div className="min-w-0">
                                        <p className="text-xs font-medium truncate" title={unit.id}>{unit.id}</p>
                                        <p className="text-[9px] text-muted-foreground">
                                            {unit.nodeCount} nós • {unit.relationCount} rel .
                                        </p>
                                    </div>
                                </div>
                                
                                <div className="flex items-center gap-1">
                                    <Button 
                                        variant="ghost" 
                                        size="icon" 
                                        className={cn("h-7 w-7", isLoaded && "text-primary hover:text-primary/80")}
                                        onClick={() => toggleUnitLoad(unit.id)}
                                        disabled={loading}
                                        title={isLoaded ? "Descarregar" : "Carregar"}
                                    >
                                        <PowerIcon size={14} />
                                    </Button>

                                    <AlertDialog>
                                        <AlertDialogTrigger asChild>
                                            <Button 
                                                variant="ghost" 
                                                size="icon" 
                                                className="h-7 w-7 text-muted-foreground hover:text-red-500"
                                                disabled={loading}
                                                title="Apagar permanentemente"
                                            >
                                                <Trash2 size={14} />
                                            </Button>
                                        </AlertDialogTrigger>
                                        <AlertDialogContent>
                                            <AlertDialogHeader>
                                                <AlertDialogTitle>Apagar Unidade de Conhecimento?</AlertDialogTitle>
                                                <AlertDialogDescription>
                                                    Tem certeza que deseja apagar a unidade <strong>{unit.id}</strong>?
                                                    <br/>
                                                    Essa ação removerá os dados do disco permanentemente.
                                                </AlertDialogDescription>
                                            </AlertDialogHeader>
                                            <AlertDialogFooter>
                                                <AlertDialogCancel>Cancelar</AlertDialogCancel>
                                                <AlertDialogAction 
                                                    onClick={(e) => deleteUnit(unit.id, e)}
                                                    className="bg-red-900/50 hover:bg-red-900 border border-red-800 text-red-100"
                                                >
                                                    Apagar
                                                </AlertDialogAction>
                                            </AlertDialogFooter>
                                        </AlertDialogContent>
                                    </AlertDialog>

                                </div>
                            </div>
                        )
                    })}
                </div>
            </ScrollArea>
        </div>
    );
}
