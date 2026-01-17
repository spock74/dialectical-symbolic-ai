import type { KnowledgeConcept } from '../types';
import { cn } from '../lib/utils';
import { BadgeCheck, AlertTriangle, Lightbulb } from 'lucide-react';

interface KnowledgeViewProps {
  concepts: KnowledgeConcept[];
}

export function KnowledgeView({ concepts }: KnowledgeViewProps) {
  if (!concepts || concepts.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center h-full text-muted-foreground">
        <Lightbulb size={48} className="mb-4 opacity-50" />
        <p>No knowledge extracted yet. Upload a PDF to begin.</p>
      </div>
    );
  }

  return (
    <div className="p-6 space-y-8 overflow-y-auto h-full bg-background">
      <h1 className="text-3xl font-bold mb-6">Extracted Knowledge Base</h1>
      
      {concepts.map((concept, idx) => (
        <div key={concept.conceptId || idx} className="bg-card border border-border rounded-xl p-6 shadow-sm">
          <div className="flex items-start justify-between mb-4">
            <div>
              <h2 className="text-xl font-semibold">{concept.core_concept}</h2>
              <div className="flex gap-2 mt-2">
                <span className={cn("px-2 py-0.5 rounded text-xs font-medium bg-primary/10 text-primary")}>
                  {concept.conceptualComplexity} Complexity
                </span>
                <span className={cn("px-2 py-0.5 rounded text-xs font-medium bg-accent text-accent-foreground")}>
                  {concept.clinicalRelevance} Relevance
                </span>
              </div>
            </div>
            {/* Future: Add Confidence Score if available */}
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* Nuggets Section */}
            <div className="space-y-4">
              <h3 className="text-sm font-semibold uppercase tracking-wider text-muted-foreground flex items-center gap-2">
                <BadgeCheck size={16} /> Verified Nuggets
              </h3>
              <ul className="space-y-3">
                {concept.knowledgeNuggets.map((nugget, i) => (
                  <li key={i} className="bg-secondary/50 p-3 rounded-lg text-sm">
                    <p className="font-medium text-foreground mb-1">{nugget.nugget}</p>
                    <p className="text-muted-foreground text-xs italic border-l-2 border-primary/30 pl-2">
                      “{nugget.source_quote}”
                    </p>
                  </li>
                ))}
              </ul>
            </div>

            {/* Application Section */}
            <div className="space-y-6">
              {concept.potentialMisconceptions.length > 0 && (
                <div>
                   <h3 className="text-sm font-semibold uppercase tracking-wider text-muted-foreground flex items-center gap-2 mb-2">
                    <AlertTriangle size={16} /> Misconceptions
                  </h3>
                  <ul className="list-disc list-inside text-sm text-muted-foreground space-y-1">
                    {concept.potentialMisconceptions.map((m, i) => (
                      <li key={i}>{m}</li>
                    ))}
                  </ul>
                </div>
              )}
              
              <div>
                <h3 className="text-sm font-semibold uppercase tracking-wider text-muted-foreground mb-2">Bloom Levels</h3>
                <div className="flex flex-wrap gap-2">
                  {concept.bloomLevels.map((level, i) => (
                    <span key={i} className="px-2 py-1 bg-secondary rounded text-xs text-secondary-foreground">
                      {level}
                    </span>
                  ))}
                </div>
              </div>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}
