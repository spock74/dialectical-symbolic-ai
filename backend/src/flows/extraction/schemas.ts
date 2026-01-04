import { z } from 'genkit';
import { ai } from '../../genkit';

export const KnowledgeNuggetSchema = z.object({
  nugget: z.string().describe("The atomic fact extracted."),
  source_quote: z.string().describe("Verbatim quote from the text proving the nugget.").optional(),
});

export const RelatedConceptSchema = z.object({
  type: z.enum(['prerequisite', 'co-requisite', 'application', 'contrast']),
  conceptId: z.string(),
});

export const KnowledgeConceptSchema = z.object({
  // conceptId will be generated via Hash in post-processing
  core_concept: z.string().describe("Main summary of the finding."),
  knowledgeNuggets: z.array(KnowledgeNuggetSchema),
  potentialMisconceptions: z.array(z.string()).optional(),
  bloomLevels: z.array(z.enum(['Lembrar', 'Compreender', 'Aplicar', 'Analisar', 'Avaliar', 'Criar'])).optional(),
  conceptualComplexity: z.enum(['Baixa', 'Média', 'Alta']).optional(),
  clinicalRelevance: z.enum(['Baixa', 'Média', 'Alta', 'Crítica']).optional(),
  relatedConcepts: z.array(RelatedConceptSchema).optional(),
  metacognitivePrompts: z.array(z.string()).optional(),
});

export const RelationSchema = z.object({
  source: z.string(),
  target: z.string(),
  label: z.string(),
  category: z.enum(['CAUSAL', 'METHODOLOGY', 'BIBLIOGRAPHIC', 'ONTOLOGY']),
});

export const KnowledgeBaseOutputSchema = z.object({
  knowledgeBase: z.array(KnowledgeConceptSchema),
  relations: z.array(RelationSchema).optional(),
});

ai.defineSchema('KnowledgeBaseOutputSchema', KnowledgeBaseOutputSchema);
