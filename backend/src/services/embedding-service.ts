import { CONFIG } from '../config/constants';

interface EmbeddingResponse {
  embedding: number[];
}

export class EmbeddingService {
  private static instance: EmbeddingService;
  // Default to a lightweight embedding model if not specified in env
  private modelName: string = CONFIG.OLLAMA_EMBED_MODEL_NAME; 

  private constructor() {
    // If we have a specific env var for embedding model, we could use it.
    // For now, hardcoding or inferring could work. 
    // Ideally, add OLLAMA_EMBED_MODEL to constants.
  }

  public static getInstance(): EmbeddingService {
    if (!EmbeddingService.instance) {
      EmbeddingService.instance = new EmbeddingService();
    }
    return EmbeddingService.instance;
  }

  /**
   * Generates a vector embedding for a given text using Ollama.
   * Returns null if generation fails to avoid breaking lines.
   */
  public async getEmbedding(text: string): Promise<number[] | null> {
    try {
      if (!CONFIG.USE_LOCAL_MODELS) {
        // TODO: Implement Gemini Embeddings if needed. 
        // For now, return null to skip vector logic in Cloud mode.
        return null; 
      }

      // Sanitize input
      const cleanText = text.trim();
      if (!cleanText) return null;

      const response = await fetch('http://localhost:11434/api/embeddings', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          model: this.modelName,
          prompt: cleanText,
        }),
      });

      if (!response.ok) {
        console.warn(`[EmbeddingService] Failed to fetch embedding: ${response.statusText}`);
        return null;
      }

      const data = await response.json() as EmbeddingResponse;
      return data.embedding;
    } catch (error) {
      console.error('[EmbeddingService] Error generating embedding:', error);
      return null;
    }
  }

  /**
   * Formats a vector array into a Lisp-readable string representation.
   * Example: "#(0.1 0.2 ...)"
   */
  public formatVectorForLisp(vector: number[] | null): string {
    if (!vector) return "nil";
    // SBCL expects simple-array single-float. 
    // We format as #(0.1 0.2 ...)
    return `#(${vector.join(' ')})`;
  }
}

export const embeddingService = EmbeddingService.getInstance();
