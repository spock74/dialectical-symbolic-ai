import sys
import os

# Ensure we can import from core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from core.kernel.sbcl_bridge import SBCLKernel

def test_bridge():
    print("[INFO] Initializing SBCL Kernel...")
    try:
        kernel = SBCLKernel()
    except FileNotFoundError:
        print("[CRITICAL] 'sbcl' executable not found in PATH.")
        print("   Please install it via 'brew install sbcl' or 'sudo apt install sbcl'")
        return

    # Test 1: Safe Fact using check-consistency (default main loop behavior)
    # The default main-loop calls (check-consistency input)
    safe_fact = "(valid-fact)"
    print(f"\n[TEST 1] Sending SAFE logic -> {safe_fact}")
    result = kernel.validate_expression(safe_fact)
    if result:
        print("   [PASSED] Kernel accepted valid fact.")
    else:
        print("   [FAILED] Kernel rejected valid fact.")

    # Test 2: Forbidden Fact
    forbidden_fact = "(eat poison)"
    print(f"\n[TEST 2] Sending TOXIC logic -> {forbidden_fact}")
    result = kernel.validate_expression(forbidden_fact)
    if not result:
        print("   [PASSED] Kernel correctly blocked toxic fact.")
    else:
        print("   [FAILED] Kernel allowed toxic fact!")

    print("\n[INFO] Shutting down Kernel...")
    kernel.shutdown()

if __name__ == "__main__":
    test_bridge()
