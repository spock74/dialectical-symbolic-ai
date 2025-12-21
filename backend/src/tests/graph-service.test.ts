import { describe, it, expect } from 'vitest';
import { transformMemoriesToGraph, Memory } from '../services/graph-service';

describe('Graph Service', () => {

    it("should transform empty memories into empty graph", () => {
      const input = { nodes: [], edges: [] };
      const result = transformMemoriesToGraph(input);
      expect(result.nodes).toEqual([]);
      expect(result.edges).toEqual([]);
    });

    it('should transform memories into nodes', () => {
        const input = {
          nodes: [
            { key: "concept-1", value: "Definition 1" },
            { key: "concept-2", value: "Definition 2" },
          ],
          edges: [],
        };
        const result = transformMemoriesToGraph(input);
        
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

    it("should transform relations into edges", () => {
      const input = {
        nodes: [
          { key: "A", value: "Alpha" },
          { key: "B", value: "Beta" },
        ],
        edges: [{ source: "A", target: "B", relation: "leads-to" }],
      };
      const result = transformMemoriesToGraph(input);
      expect(result.edges).toHaveLength(1);
      expect(result.edges[0].source).toBe("A");
      expect(result.edges[0].target).toBe("B");
      expect(result.edges[0].label).toBe("leads-to");
      expect(result.edges[0].id).toContain("e0-A-B");
    });

});
