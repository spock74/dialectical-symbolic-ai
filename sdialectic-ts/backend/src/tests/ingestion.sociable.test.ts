import { describe, it, expect, beforeAll } from 'vitest';
import { extractKnowledgeMultimodal } from '../flows/extraction/multimodal-flow';
import { commitKnowledgeToGraph } from '../services/graph-service';
import * as fs from 'fs';
import * as path from 'path';
import { CONFIG } from '../config/constants';

/**
 * SOCIABLE TEST: PDF Ingestion & Persistence
 * philosophy: Zero Mocks, Real Dependencies.
 */
describe('Sociable Test: Multimodal PDF Ingestion', () => {
  const testPdfPath = path.resolve(__dirname, 'mind-reading.pdf');
  const trackId = 'mind-reading-test';
  
  // Persistence paths
  const graphPath = path.resolve(process.cwd(), 'data', 'graphs', `${trackId}.json`);
  const unitPath = path.resolve(process.cwd(), 'data', 'units', trackId, 'knowledge.json');
  const metadataPath = path.resolve(process.cwd(), 'data', 'units', trackId, 'metadata.json');

  beforeAll(() => {
    // Ensure API Key is present for the sociable test
    if (!CONFIG.GEMINI_API_KEY) {
      throw new Error('Sociable test requires GEMINI_API_KEY environment variable.');
    }
    
    // Cleanup previous test state if any
    if (fs.existsSync(graphPath)) fs.unlinkSync(graphPath);
    if (fs.existsSync(unitPath)) fs.unlinkSync(unitPath);
    if (fs.existsSync(metadataPath)) fs.unlinkSync(metadataPath);
  });

  it('should ingest PDF, extract knowledge, and persist to disk without mocks', async () => {
    console.log('[Test] Starting sociable ingestion for:', testPdfPath);

    // 1. EXECUTE REAL FLOW (Calls Gemini File API)
    const result = await extractKnowledgeMultimodal({
      filePath: testPdfPath,
      trackId: trackId
    });

    expect(result).toBeDefined();
    expect(result.relations.length).toBeGreaterThan(0);
    expect(result.lisp_raw).toContain('(adicionar-relacao');

    console.log(`[Test] Extraction successful. Found ${result.relations.length} relations and ${result.rules.length} rules.`);
    if (result.rules.length === 0) {
      console.log('[Test] Zero rules found. Raw Lisp snippet:', result.lisp_raw.slice(0, 500) + '...');
    }

    // 2. COMMIT TO GRAPH (Updates in-memory state and triggers persistence)
    await commitKnowledgeToGraph(result, trackId);

    // 3. VERIFY PERSISTENCE (Black Box verification of side effects)
    
    // Check Active Graph JSON
    expect(fs.existsSync(graphPath), 'Graph JSON should be created in data/graphs').toBe(true);
    const graphData = JSON.parse(fs.readFileSync(graphPath, 'utf-8'));
    expect(graphData.relations.length).toBeGreaterThan(0);

    // Check Knowledge Unit directory and files
    expect(fs.existsSync(unitPath), 'knowledge.json should be created in data/units').toBe(true);
    expect(fs.existsSync(metadataPath), 'metadata.json should be created in data/units').toBe(true);
    
    const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf-8'));
    expect(metadata.id).toBe(trackId);
    expect(metadata.stats.edges).toBe(graphData.relations.length);

    console.log('[Test] Sociable verification complete. All persistence files verified.');
  }, 540000); // 9 minute timeout for Gemini processing
});
