import { describe, it, expect } from 'vitest';
import { transformMemoriesToGraph, Memory } from '../services/graph-service';

describe('Graph Service', () => {
    it('should transform empty memories into empty graph', () => {
        const memories: Memory[] = [];
        const result = transformMemoriesToGraph(memories);
        expect(result.nodes).toEqual([]);
        expect(result.edges).toEqual([]);
    });

    it('should transform memories into nodes', () => {
        const memories: Memory[] = [
            { key: 'concept-1', value: 'Definition 1' },
            { key: 'concept-2', value: 'Definition 2' },
        ];
        const result = transformMemoriesToGraph(memories);
        
        expect(result.nodes).toHaveLength(2);
        
        const node1 = result.nodes.find(n => n.id === 'concept-1');
        expect(node1).toBeDefined();
        expect(node1?.data.label).toBe('concept-1');
        expect(node1?.data.details).toBe('Definition 1');
        expect(node1?.type).toBe('default');
        
        // Check structural integrity
        expect(node1?.position).toHaveProperty('x');
        expect(node1?.position).toHaveProperty('y');
    });
});
