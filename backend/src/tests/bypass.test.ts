
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { reflectiveLoop } from '../flows/reflective-loop';

// Mock the services
vi.mock('../services/reflective-orchestrator', () => {
    return {
        ReflectiveOrchestrator: vi.fn().mockImplementation(() => {
            return {
                think: vi.fn().mockResolvedValue("mocked fact package"),
                getReasoningLogs: vi.fn().mockReturnValue("mocked reasoning logs"),
            };
        })
    };
});

// Mock Lisp and Genkit components
vi.mock('../lisp/sbcl-process', () => {
    return {
        SbclProcess: {
            getInstance: vi.fn().mockReturnValue({
                emit: vi.fn(),
                saveState: vi.fn().mockResolvedValue("ok")
            })
        }
    };
});

vi.mock('../genkit', () => {
    return {
        ai: {
            registry: {},
            defineFlow: (meta: any, fn: any) => fn
        }
    };
});

vi.mock('@genkit-ai/dotprompt', () => {
    return {
        prompt: vi.fn().mockResolvedValue({
            generate: vi.fn().mockResolvedValue({ text: "Synthesis Response" })
        })
    };
});

// Mock model cleanup
vi.mock('../services/model-cleanup', () => ({
    scheduleModelUnload: vi.fn()
}));

// Import the mocked orchestrator to check calls
import { ReflectiveOrchestrator } from '../services/reflective-orchestrator';

describe('S-Dialectic Bypass', () => {
    beforeEach(() => {
        vi.clearAllMocks();
    });

    it('should NOT call ReflectiveOrchestrator when bypassSDialect is true', async () => {
        const input = {
            prompt: "Test prompt",
            bypassSDialect: true
        };

        // @ts-ignore
        const result = await reflectiveLoop(input);

        expect(result).toBe("Synthesis Response");
        expect(ReflectiveOrchestrator).not.toHaveBeenCalled();
    });

    it('should call ReflectiveOrchestrator when bypassSDialect is false or missing', async () => {
        const input = {
            prompt: "Test prompt",
            bypassSDialect: false
        };

        // @ts-ignore
        const result = await reflectiveLoop(input);

        expect(result).toBe("Synthesis Response");
        expect(ReflectiveOrchestrator).toHaveBeenCalled();
    });
});
