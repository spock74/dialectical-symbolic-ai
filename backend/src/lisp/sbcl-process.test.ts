import { describe, it, expect, afterAll } from 'vitest';
import { SbclProcess } from './sbcl-process';

describe('SbclProcess', () => {
  const sbcl = SbclProcess.getInstance();

  afterAll(() => {
    sbcl.kill();
  });

  it('should evaluate simple math', async () => {
    const result = await sbcl.eval('(+ 1 2)');
    expect(result).toContain('3');
  });

  it('should maintain state', async () => {
    await sbcl.eval('(defvar *x* 10)');
    const result = await sbcl.eval('(+ *x* 5)');
    expect(result).toContain('15');
  });

  it('should handle formatted output', async () => {
    const result = await sbcl.eval('(format t "Hello Lisp")');
    // The output might contain the return value (NIL) and the printed stdout.
    expect(result).toContain('Hello Lisp');
  });
});
