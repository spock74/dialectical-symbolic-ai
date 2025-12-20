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
