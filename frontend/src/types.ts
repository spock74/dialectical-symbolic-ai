export interface KnowledgeNugget {
  nugget: string;
  source_quote: string;
}

export interface RelatedConcept {
  type: 'prerequisite' | 'co-requisite' | 'application' | 'contrast';
  conceptId: string;
}

export interface KnowledgeConcept {
  conceptId?: string; // Optional as it might be generated late
  core_concept: string;
  knowledgeNuggets: KnowledgeNugget[];
  potentialMisconceptions: string[];
  bloomLevels: string[];
  conceptualComplexity: 'Baixa' | 'Média' | 'Alta';
  clinicalRelevance: 'Baixa' | 'Média' | 'Alta' | 'Crítica';
  relatedConcepts?: RelatedConcept[];
  metacognitivePrompts: string[];
}

export interface KnowledgeBase {
  knowledgeBase: KnowledgeConcept[];
}

export interface Source {
  id: string;
  name: string;
  type: "pdf" | "markdown" | "txt";
  uploadDate: string;
  hash: string;
  active: boolean;
}

export interface Group {
  id: string;
  name: string;
  createdAt: string;
  sources: Source[];
}
