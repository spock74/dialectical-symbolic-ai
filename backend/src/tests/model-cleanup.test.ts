import { describe, it, expect, vi, afterEach } from 'vitest';
import { scheduleModelUnload } from '../services/model-cleanup';

describe('Model Cleanup Service', () => {
    // Mock global fetch
    const fetchMock = vi.fn();

    afterEach(() => {
        vi.restoreAllMocks();
        fetchMock.mockReset();
        vi.useRealTimers();
    });

    it('should schedule unload after delay', () => {
        vi.useFakeTimers();
        const modelName = 'gemma3:4b';
        const delaySeconds = 30;

        // Call the scheduler
        scheduleModelUnload(modelName, delaySeconds, fetchMock as unknown as typeof fetch);

        // Should NOT be called immediately
        expect(fetchMock).not.toHaveBeenCalled();

        // Fast-forward not enough time
        vi.advanceTimersByTime(29000);
        expect(fetchMock).not.toHaveBeenCalled();

        // Fast-forward to completion
        vi.advanceTimersByTime(2000); // Total 31s
        expect(fetchMock).toHaveBeenCalledTimes(1);
        
        // Check arguments
        const [url, options] = fetchMock.mock.calls[0];
        expect(url).toBe('http://127.0.0.1:11434/api/generate');
        expect(options.method).toBe('POST');
        const body = JSON.parse(options.body);
        expect(body).toEqual({ model: 'gemma3:4b', keep_alive: 0 });
    });

    it('should strip ollama/ prefix from model name', () => {
        vi.useFakeTimers();
        scheduleModelUnload('ollama/my-model', 1, fetchMock as unknown as typeof fetch);

        vi.advanceTimersByTime(1100);
        expect(fetchMock).toHaveBeenCalled();
        
        const body = JSON.parse(fetchMock.mock.calls[0][1].body);
        expect(body.model).toBe('my-model');
    });

    it('should unload immediately if delay is 0', () => {
        const modelName = 'fast-model';
        scheduleModelUnload(modelName, 0, fetchMock as unknown as typeof fetch);
        expect(fetchMock).toHaveBeenCalledTimes(1);
    });
});
