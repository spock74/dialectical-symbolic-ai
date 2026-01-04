import * as fs from 'fs';
import * as path from 'path';

export interface Concept {
  id: string;
  type: string;
  value?: any;
}

export interface Relation {
  subject: string;
  predicate: string;
  object: string;
  provenance?: "user" | "inference";
  label?: string; // Original rich verb
  category: 'CAUSAL' | 'METHODOLOGY' | 'BIBLIOGRAPHIC' | 'ONTOLOGY';
}

// Snapshot type for internal use
interface GraphSnapshot {
  nodes: Concept[];
  relations: Relation[];
}

export class KnowledgeGraph {
  private nodes: Map<string, Concept> = new Map();
  private relations: Map<string, Relation[]> = new Map(); // Indexed by subject
  private snapshot: GraphSnapshot | null = null;
  public sourceName: string;

  constructor(sourceName: string = "default") {
    this.sourceName = sourceName;
  }

  /**
   * Adds a node to the graph if it doesn't already exist.
   * Passo 1: addNode(id: string, type: string)
   */
  addNode(id: string, type: string = 'concept'): void {
    if (!this.nodes.has(id)) {
      this.nodes.set(id, { id, type });
    }
  }

  /**
   * Adds a relation between two nodes. 
   * Verifies semantic duplicates (O(1) lookup in subject map).
   * Passo 1: addRelation(s: string, p: string, o: string)
   */
  addRelation(s: string, p: string, o: string, provenance: "user" | "inference" = "user", category: 'CAUSAL' | 'METHODOLOGY' | 'BIBLIOGRAPHIC' | 'ONTOLOGY' = 'ONTOLOGY'): void {
    this.addNode(s);
    this.addNode(o);

    const subjectRelations = this.relations.get(s) || [];
    const exists = subjectRelations.some(
      r => r.predicate === p && r.object === o
    );

    if (!exists) {
      subjectRelations.push({ subject: s, predicate: p, object: o, provenance, category, label: p });
      this.relations.set(s, subjectRelations);
    }
  }

  /**
   * Simple transitivity inference: if (A, "e_um", B) and (B, "e_um", C) -> (A, "e_um", C)
   * Passo 3 mapping: inferir -> graph.infer()
   */
  infer(): string[] {
    const newRelations: Relation[] = [];
    const allRelations = Array.from(this.relations.values()).flat();
    const eUmRelations = allRelations.filter(r => r.predicate === 'e_um');

    for (const r1 of eUmRelations) {
      const r2s = (this.relations.get(r1.object) || []).filter(r => r.predicate === 'e_um');
      for (const r2 of r2s) {
        const exists = (this.relations.get(r1.subject) || []).some(
          r => r.predicate === 'e_um' && r.object === r2.object
        );
        if (!exists && r1.subject !== r2.object) {
          newRelations.push({ 
            subject: r1.subject, 
            predicate: 'e_um', 
            object: r2.object, 
            provenance: 'inference',
            category: 'ONTOLOGY', // Inferred is-a is Ontology
            label: 'e_um'
          });
        }
      }
    }

    const added: string[] = [];
    for (const rel of newRelations) {
      this.addRelation(rel.subject, rel.predicate, rel.object, rel.provenance, rel.category);
      added.push(`Inferencia: (${rel.subject} e_um ${rel.object})`);
    }
    return added;
  }

  /**
   * RETURNS the current state of the graph for visualization.
   * Passo 1: getGraphSnapshot()
   */
  getGraphSnapshot() {
    return {
      nodes: Array.from(this.nodes.values()).map(n => ({ key: n.id, value: n.type, val: n.value })),
      edges: Array.from(this.relations.values()).flat().map(r => ({
        source: r.subject,
        target: r.object,
        relation: r.predicate,
        provenance: r.provenance
      }))
    };
  }

  /**
   * Causal Intervention (Judea Pearl): do(targetNode = forcedValue)
   * Cuts off incoming causes and sets the value.
   * Passo 2: doIntervention(targetNode: string, forcedValue: any)
   */
  doIntervention(targetNode: string, forcedValue: any): void {
    // 1. Snapshot for rollback (deep copy nodes and relations)
    this.snapshot = {
      nodes: Array.from(this.nodes.values()).map(n => ({ ...n })),
      relations: Array.from(this.relations.values()).flat().map(r => ({ ...r }))
    };

    // 2. Mutilation: Remove all edges where "object" is the target (incoming causes)
    for (const [subject, rels] of this.relations.entries()) {
      const filtered = rels.filter(r => r.object !== targetNode);
      if (filtered.length !== rels.length) {
        this.relations.set(subject, filtered);
      }
    }

    // 3. Set forced value
    const node = this.nodes.get(targetNode);
    if (node) {
      node.value = forcedValue;
    } else {
      this.nodes.set(targetNode, { id: targetNode, type: 'intervention', value: forcedValue });
    }
  }

  /**
   * Restores the graph to the state before the last intervention.
   * Passo 2: resetIntervention()
   */
  resetIntervention(): void {
    if (this.snapshot) {
      this.nodes.clear();
      this.relations.clear();
      this.snapshot.nodes.forEach(n => this.nodes.set(n.id, { ...n }));
      this.snapshot.relations.forEach(r => this.addRelation(r.subject, r.predicate, r.object, r.provenance));
      this.snapshot = null;
    }
  }

  /**
   * Persists state to a JSON file.
   * Passo 1: saveState(filepath: string)
   */
  saveState(filepath: string): void {
    const dir = path.dirname(filepath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    const data = this.exportState();
    fs.writeFileSync(filepath, JSON.stringify(data, null, 2));
  }

  /**
   * Exports the current state of the graph.
   * Used for Knowledge Unit persistence.
   */
  exportState(): { nodes: Concept[], relations: Relation[] } {
      return {
          nodes: Array.from(this.nodes.values()),
          relations: Array.from(this.relations.values()).flat()
      };
  }

  /**
   * Hydrates state from a JSON file.
   * Passo 1: loadState(filepath: string)
   */
  loadState(filepath: string): void {
    if (fs.existsSync(filepath)) {
      try {
        const data = JSON.parse(fs.readFileSync(filepath, 'utf-8'));
        this.nodes.clear();
        this.relations.clear();
        if (data.nodes) {
          data.nodes.forEach((n: Concept) => {
            this.nodes.set(n.id, n);
            // Ensure relations map is initialized for each node
            if (!this.relations.has(n.id)) this.relations.set(n.id, []);
          });
        }
        if (data.relations) {
          data.relations.forEach((r: Relation) => {
            this.addRelation(r.subject, r.predicate, r.object, r.provenance);
          });
        }
      } catch (e) {
        console.error(`Failed to load state from ${filepath}:`, e);
      }
    }
  }

  /**
   * Searches for all relations involving a specific node (as subject or object).
   */
  findRelations(nodeId: string): Relation[] {
    const results: Relation[] = [];
    // As subject
    const asSubject = this.relations.get(nodeId) || [];
    results.push(...asSubject);
    
    // As object
    for (const allRels of this.relations.values()) {
      for (const rel of allRels) {
        if (rel.object === nodeId && rel.subject !== nodeId) {
          results.push(rel);
        }
      }
    }
    return results;
  }

  /**
   * Retrieves a node's details.
   */
  getNode(nodeId: string): Concept | undefined {
    return this.nodes.get(nodeId);
  }

  listNodes(): string {
    const nodes = Array.from(this.nodes.values()).map(n => `(${n.id} :type ${n.type}${n.value !== undefined ? ` :value ${n.value}` : ''})`);
    return `(${nodes.join(' ')})`;
  }

  listRelations(): string {
    const rels = Array.from(this.relations.values()).flat().map(r => `(${r.subject} ${r.predicate} ${r.object})`);
    return `(${rels.join(' ')})`;
  }

  clear(): void {
    this.nodes.clear();
    this.relations.clear();
    this.snapshot = null;
  }
}


export class GraphManager {
  private instances: Map<string, KnowledgeGraph> = new Map();
  private baseDir: string = path.join(process.cwd(), "data", "graphs");

  constructor() {
    if (!fs.existsSync(this.baseDir)) {
      fs.mkdirSync(this.baseDir, { recursive: true });
    }
    // Initialize default instance
    this.getGraph("default");
  }

  getGraph(sourceName: string): KnowledgeGraph {
    if (!this.instances.has(sourceName)) {
      const graph = new KnowledgeGraph(sourceName);
      // Try to load state if it exists
      const filepath = path.join(this.baseDir, `${sourceName}.json`);
      if (fs.existsSync(filepath)) {
        // We temporarily use saveState path logic or just load it
        graph.loadState(filepath);
      }
      this.instances.set(sourceName, graph);
    }
    return this.instances.get(sourceName)!;
  }

  saveAll(): void {
    for (const [name, graph] of this.instances.entries()) {
      const filepath = path.join(this.baseDir, `${name}.json`);
      graph.saveState(filepath);
    }
  }

  listGraphs(): string[] {
    return Array.from(this.instances.keys());
  }
}

// Singleton manager instance
export const graphManager = new GraphManager();

/**
 * Helper to get the active graph for a given source.
 * Falls back to "default" if no source is provided.
 */
export const getActiveGraph = (source?: string): KnowledgeGraph =>
  graphManager.getGraph(source || "default");
