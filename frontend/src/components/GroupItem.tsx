import { Trash2, FileText, File, Calendar, ChevronDown, ChevronRight, Hash } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";
import { ScrollArea } from "@/components/ui/scroll-area";
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
} from "@/components/ui/alert-dialog";
import type { Group } from "../types";
import { UploadZone } from "./UploadZone";

interface GroupItemProps {
  group: Group;
  isActive: boolean;
  onSelect: () => void;
  onDelete: () => void;
  activeSourceId?: string | null;
  onSelectSource?: (sourceId: string) => void;
  onFileUpload?: (file: File) => Promise<void>;
  isProcessing?: boolean;
}

export function GroupItem({
  group,
  isActive,
  onSelect,
  onDelete,
  activeSourceId,
  onSelectSource,
  onFileUpload,
  isProcessing
}: GroupItemProps) {
  
  return (
    <Card 
      className={cn(
        "transition-all duration-200 overflow-hidden",
        isActive 
          ? "border-primary shadow-sm bg-accent/5" 
          : "hover:bg-muted/50 cursor-pointer"
      )}
    >
      {/* Group Header - Always Visible */}
      <div 
        className="p-3 flex items-center justify-between group"
        onClick={onSelect}
      >
        <div className="flex-1 min-w-0 pr-3">
            <div className="flex items-center gap-2 mb-1">
                {isActive ? (
                    <ChevronDown className="h-4 w-4 text-primary shrink-0 transition-transform" />
                ) : (
                    <ChevronRight className="h-4 w-4 text-muted-foreground shrink-0" />
                )}
                <h3 className={cn(
                    "font-medium truncate text-sm",
                    isActive ? "text-primary" : "text-foreground"
                )}>
                    {group.name}
                </h3>
            </div>
            <div className="flex items-center gap-2 text-xs text-muted-foreground pl-6">
                <Calendar className="h-3 w-3" />
                <span>{group.createdAt}</span>
                <span className="text-[10px] bg-muted px-1.5 py-0.5 rounded-full flex items-center gap-1">
                  <File className="h-3 w-3" />
                  {group.sources.length}
                </span>
            </div>
        </div>

        <AlertDialog>
          <AlertDialogTrigger asChild>
            <Button
              variant="ghost"
              size="icon"
              className="h-8 w-8 text-muted-foreground hover:text-destructive hover:bg-destructive/10 opacity-0 group-hover:opacity-100 transition-opacity"
              onClick={(e) => e.stopPropagation()}
            >
              <Trash2 className="h-4 w-4" />
            </Button>
          </AlertDialogTrigger>
          <AlertDialogContent>
            <AlertDialogHeader>
              <AlertDialogTitle>Excluir Grupo</AlertDialogTitle>
              <AlertDialogDescription>
                Tem certeza que deseja excluir o grupo "{group.name}"?
                Isso removerá a organização visual, mas os arquivos permanecerão na base de conhecimento.
              </AlertDialogDescription>
            </AlertDialogHeader>
            <AlertDialogFooter>
              <AlertDialogCancel onClick={(e) => e.stopPropagation()}>Cancelar</AlertDialogCancel>
              <AlertDialogAction 
                onClick={(e) => {
                  e.stopPropagation();
                  onDelete();
                }}
                className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
              >
                Excluir
              </AlertDialogAction>
            </AlertDialogFooter>
          </AlertDialogContent>
        </AlertDialog>
      </div>

      {/* Group Sources Body - Collapsible */}
      {isActive && (
        <div className="border-t border-border/50 bg-background/50 animate-in slide-in-from-top-2 duration-200">
             
             {/* Integrated Upload Zone */}
             {onFileUpload && (
                <div className="p-2 pb-0">
                    <UploadZone 
                        onFileUpload={onFileUpload} 
                        isProcessing={isProcessing} 
                        variant="compact"
                    />
                </div>
             )}

            {group.sources.length === 0 ? (
                <div className="p-4 text-center text-xs text-muted-foreground py-6">
                    Nenhum arquivo neste grupo
                </div>
            ) : (
                <ScrollArea className="max-h-[200px]">
                    <div className="p-2 space-y-1">
                        {group.sources.map((source) => (
                            <div
                                key={source.id}
                                onClick={(e) => {
                                    e.stopPropagation();
                                    onSelectSource?.(source.id);
                                }}
                                className={cn(
                                    "flex items-center gap-2 p-2 rounded-md text-sm cursor-pointer transition-colors",
                                    source.id === activeSourceId 
                                        ? "bg-primary/10 text-primary font-medium" 
                                        : "hover:bg-muted text-muted-foreground hover:text-foreground"
                                )}
                            >
                                {source.type === 'pdf' ? (
                                    <FileText className="h-4 w-4 shrink-0 opacity-70" />
                                ) : (
                                    <Hash className="h-4 w-4 shrink-0 opacity-70" />
                                )}
                                <span className="truncate flex-1">{source.name}</span>
                            </div>
                        ))}
                    </div>
                </ScrollArea>
            )}
        </div>
      )}
    </Card>
  );
}
