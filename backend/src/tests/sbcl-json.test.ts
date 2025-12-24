
import { describe, it, expect } from 'vitest';
import { SbclProcess } from '../lisp/sbcl-process';

// Mock subclass to avoid actual child process spawning
class MockSbclProcess extends SbclProcess {
  mockOutput: string = "";

  constructor() {
    super();
  }
  // Override start to do nothing
  // @ts-ignore
  protected start() {
    this.isReady = true;
  }

  // Override eval to return mock output
  override async eval(
    code: string
  ): Promise<{ result: string; output: string }> {
    return { result: this.mockOutput, output: this.mockOutput };
  }
}

describe('SBCL JSON Parsing', () => {
    it('should parse simple valid JSON graph', async () => {
        const proc = new MockSbclProcess();
        proc.mockOutput = '{"nodes": [{"key": "A", "value": "ValA"}], "edges": []}';
        
        const data = await proc.getGraphData();
        expect(data.nodes).toHaveLength(1);
        expect(data.nodes[0].key).toBe("A");
        expect(data.edges).toHaveLength(0);
    });

    it('should unwrap Lisp string quotes if present', async () => {
        const proc = new MockSbclProcess();
        // Lisp might return: "{...}" (with surrounding quotes if it was a format return)
        proc.mockOutput = '"{\\"nodes\\": [], \\"edges\\": []}"';
        
        const data = await proc.getGraphData();
        expect(data.nodes).toEqual([]);
        expect(data.edges).toEqual([]);
    });

    it('should handle escaped newlines from Lisp', async () => {
        const proc = new MockSbclProcess();
        // Lisp: "{\"key\": \"Line1\\nLine2\", ...}"
        proc.mockOutput = '"{\\"nodes\\": [{\\"key\\": \\"K\\", \\"value\\": \\"Line1\\\\nLine2\\"}], \\"edges\\": []}"';
        
        const data = await proc.getGraphData();
        expect(data.nodes[0].value).toBe("Line1\nLine2");
    });

    it('should return empty graph on malformed JSON', async () => {
        const proc = new MockSbclProcess();
        proc.mockOutput = '{ not valid json }';
        
        const data = await proc.getGraphData();
        expect(data.nodes).toEqual([]);
        expect(data.edges).toEqual([]);
    });
});
