import { useRef, useState, type ChangeEvent } from "react"
import { Folder, FileText, Plus, Upload } from "lucide-react"
import { uploadPdf, uploadPdfMultimodal } from "../api"
import type { KnowledgeBase } from "../types"

interface SourceManagerProps {
  onKnowledgeLoaded: (kb: KnowledgeBase) => void;
}

export function SourceManager({ onKnowledgeLoaded }: SourceManagerProps) {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [useMultimodal, setUseMultimodal] = useState(false);
  const [loading, setLoading] = useState(false);

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file) {
      try {
        setLoading(true);
        console.log(`Uploading... ${file.name} (Multimodal: ${useMultimodal})`);
        const result = useMultimodal 
          ? await uploadPdfMultimodal(file) 
          : await uploadPdf(file);
        
        console.log("Uploaded!", result);
        onKnowledgeLoaded(result);
      } catch (error) {
        console.error("Upload failed", error);
        alert("Failed to upload/extract PDF. Check console.");
      } finally {
        setLoading(false);
      }
    }
  };

  return (
    <div className="flex flex-col h-full border-r border-border bg-card p-4 min-w-0">
      <div className="flex flex-col gap-2 mb-4">
        <div className="flex items-center justify-between">
          <h2 className="text-lg font-semibold">Sources</h2>
          <div className="flex gap-1">
            <input 
              type="file" 
              ref={fileInputRef} 
              className="hidden" 
              accept="application/pdf"
              onChange={handleFileChange}
            />
            <button 
              className="p-1 hover:bg-accent rounded disabled:opacity-50" 
              title="Upload PDF"
              onClick={() => fileInputRef.current?.click()}
              disabled={loading}
            >
              {loading ? (
                <div className="animate-spin h-4 w-4 border-2 border-primary border-t-transparent rounded-full" />
              ) : (
                <Upload size={18} />
              )}
            </button>
            <button className="p-1 hover:bg-accent rounded">
              <Plus size={18} />
            </button>
          </div>
        </div>
        
        {/* Toggle Mode */}
        <div className="flex items-center gap-2 text-xs">
          <label className="flex items-center gap-2 cursor-pointer select-none">
            <input 
              type="checkbox" 
              checked={useMultimodal} 
              onChange={(e) => setUseMultimodal(e.target.checked)}
              className="rounded border-gray-300 text-primary focus:ring-primary h-3 w-3"
            />
            <span>Use Multimodal (Gemma Vision)</span>
          </label>
        </div>
       
        {loading && (
           <div className="text-xs text-muted-foreground animate-pulse">
             {useMultimodal ? "Analyzing visual structures..." : "Extracting text knowledge..."}
           </div>
        )}
      </div>
      
      {/* Groups List */}
      <div className="space-y-2 flex-1 overflow-y-auto">
        <div className="group-item">
          <div className="flex items-center gap-2 text-sm font-medium mb-1 text-muted-foreground hover:text-foreground cursor-pointer">
            <Folder size={16} />
            Study Group 1
          </div>
          <div className="pl-6 space-y-1">
            <div className="flex items-center gap-2 text-sm p-1 hover:bg-accent rounded cursor-pointer">
              <FileText size={14} />
              paper.pdf
            </div>
            <div className="flex items-center gap-2 text-sm p-1 hover:bg-accent rounded cursor-pointer">
              <FileText size={14} />
              notes.md
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
