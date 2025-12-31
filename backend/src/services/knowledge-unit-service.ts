import * as fs from 'fs';
import * as path from 'path';

const UNITS_DIR = path.join(process.cwd(), 'data', 'units');

export interface KnowledgeUnitMetadata {
    id: string;
    description?: string;
    created_at: string;
    updated_at: string;
    stats?: {
        nodes: number;
        edges: number;
    };
}

export class KnowledgeUnitService {
    constructor() {
        if (!fs.existsSync(UNITS_DIR)) {
            fs.mkdirSync(UNITS_DIR, { recursive: true });
        }
    }

    private getUnitPath(id: string): string {
        return path.join(UNITS_DIR, id);
    }

    async listUnits(): Promise<KnowledgeUnitMetadata[]> {
        if (!fs.existsSync(UNITS_DIR)) return [];
        const dirs = await fs.promises.readdir(UNITS_DIR, { withFileTypes: true });
        
        const units = await Promise.all(dirs
            .filter(d => d.isDirectory())
            .map(async (d) => {
                const id = d.name;
                const metaPath = path.join(UNITS_DIR, id, 'metadata.json');
                if (fs.existsSync(metaPath)) {
                    try {
                        const content = await fs.promises.readFile(metaPath, 'utf-8');
                        return JSON.parse(content) as KnowledgeUnitMetadata;
                    } catch (e) {
                         console.error(`Failed to read metadata for unit ${id}`, e);
                         return null;
                    }
                }
                // Fallback if no metadata file
                return {
                    id,
                    created_at: new Date().toISOString(),
                    updated_at: new Date().toISOString()
                } as KnowledgeUnitMetadata;
            }));
            
        return units.filter((u): u is KnowledgeUnitMetadata => u !== null);
    }

    async createOrUpdateUnit(id: string, graphData: any): Promise<void> {
        const unitPath = this.getUnitPath(id);
        if (!fs.existsSync(unitPath)) {
            fs.mkdirSync(unitPath, { recursive: true });
        }

        // Save Graph
        const graphPath = path.join(unitPath, 'knowledge.json');
        await fs.promises.writeFile(graphPath, JSON.stringify(graphData, null, 2));

        // Save/Update Metadata
        const metaPath = path.join(unitPath, 'metadata.json');
        const stats = {
            nodes: graphData.nodes?.length || 0,
            edges: graphData.relations?.length || 0 // Assuming graphData structure from KnowledgeGraph.saveState
        };

        const meta: KnowledgeUnitMetadata = {
            id,
            created_at: new Date().toISOString(), // In reality we should preserve creation time if exists
            updated_at: new Date().toISOString(),
            stats
        };
        
        // Preserve creation time if exists
        try {
            if (fs.existsSync(metaPath)) {
                const old = JSON.parse(await fs.promises.readFile(metaPath, 'utf-8'));
                meta.created_at = old.created_at;
            }
        } catch {}

        await fs.promises.writeFile(metaPath, JSON.stringify(meta, null, 2));
    }

    async getUnitGraph(id: string): Promise<any | null> {
        const graphPath = path.join(this.getUnitPath(id), 'knowledge.json');
        if (!fs.existsSync(graphPath)) return null;
        try {
             return JSON.parse(await fs.promises.readFile(graphPath, 'utf-8'));
        } catch (e) {
            console.error(`Failed to load graph for unit ${id}`, e);
            return null;
        }
    }

    async deleteUnit(id: string): Promise<void> {
        const unitPath = this.getUnitPath(id);
        if (fs.existsSync(unitPath)) {
            await fs.promises.rm(unitPath, { recursive: true, force: true });
        }
    }
}

export const knowledgeUnitService = new KnowledgeUnitService();
