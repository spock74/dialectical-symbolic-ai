import { useRef, useState } from "react";
import { Upload, Loader2 } from "lucide-react";

import { Card } from "@/components/ui/card";
import { cn } from "@/lib/utils";

interface UploadZoneProps {
  onFileUpload: (file: File) => Promise<void>;
  disabled?: boolean;
  isProcessing?: boolean;
  variant?: 'default' | 'compact';
}

export function UploadZone({ onFileUpload, disabled, isProcessing, variant = 'default' }: UploadZoneProps) {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [isDragging, setIsDragging] = useState(false);

  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault();
    if (!disabled) setIsDragging(true);
  };

  const handleDragLeave = (e: React.DragEvent) => {
    e.preventDefault();
    setIsDragging(false);
  };

  const handleDrop = async (e: React.DragEvent) => {
    e.preventDefault();
    setIsDragging(false);
    if (disabled || isProcessing) return;

    if (e.dataTransfer.files && e.dataTransfer.files[0]) {
      await onFileUpload(e.dataTransfer.files[0]);
    }
  };

  const handleClick = () => {
    if (!disabled && !isProcessing) {
      fileInputRef.current?.click();
    }
  };

  const handleFileChange = async (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0]) {
      await onFileUpload(e.target.files[0]);
    }
    // Reset inputs
    if (fileInputRef.current) fileInputRef.current.value = "";
  };

  const isCompact = variant === 'compact';

  return (
    <Card
      className={cn(
        "border-dashed transition-colors cursor-pointer flex flex-col items-center justify-center gap-2 text-muted-foreground hover:bg-muted/50",
        isDragging && "border-primary bg-primary/5",
        (disabled || isProcessing) && "opacity-50 cursor-not-allowed hover:bg-background",
        isCompact ? "border p-3 min-h-[60px]" : "border-2 p-6"
      )}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
      onClick={handleClick}
    >
      <input
        type="file"
        ref={fileInputRef}
        className="hidden"
        onChange={handleFileChange}
        accept=".md,.pdf,.txt"
        disabled={disabled || isProcessing}
      />
      
      {isProcessing ? (
        <div className="flex items-center gap-2">
          <Loader2 className={cn("animate-spin text-primary", isCompact ? "h-4 w-4" : "h-8 w-8")} />
          <p className={cn("font-medium", isCompact ? "text-xs" : "text-sm")}>
            {isCompact ? "Processando..." : "Processando arquivo..."}
          </p>
        </div>
      ) : (
        isCompact ? (
          <div className="flex items-center gap-2 text-xs">
            <Upload className="h-3 w-3" />
            <span>Arraste ou clique para adicionar arquivos</span>
          </div>
        ) : (
          <>
            <div className="p-3 bg-muted rounded-full ring-4 ring-background">
              <Upload className="h-6 w-6" />
            </div>
            <div className="text-center space-y-1">
              <p className="font-medium text-foreground">
                Clique para upload ou arraste
              </p>
              <p className="text-xs">
                Suporta Markdown, PDF e Text
              </p>
            </div>
          </>
        )
      )}
    </Card>
  );
}
