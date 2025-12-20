import { Zap } from "lucide-react"

export function ActionPanel() {
  return (
    <div className="flex flex-col h-full border-l border-border bg-card p-4">
      <div className="flex items-center gap-2 mb-6">
        <Zap className="text-primary" size={20} />
        <h2 className="text-lg font-semibold">Actions</h2>
      </div>

      <div className="space-y-4">
        <button className="w-full py-2 px-4 bg-primary text-primary-foreground rounded-md shadow hover:bg-primary/90 transition">
          Generate Mind Map
        </button>
        <button className="w-full py-2 px-4 bg-secondary text-secondary-foreground rounded-md hover:bg-secondary/80 transition">
          Analyze PDF
        </button>
        <button className="w-full py-2 px-4 bg-secondary text-secondary-foreground rounded-md hover:bg-secondary/80 transition">
          Create Flashcards
        </button>
      </div>
    </div>
  )
}
