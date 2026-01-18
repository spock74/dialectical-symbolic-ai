import json
import os
from core.kernel.sbcl_bridge import SBCLKernel

def validate_dataset(filepath="datasets/golden_logic.json"):
    print(f"Validating dataset: {filepath}")
    
    # 1. Start Kernel
    try:
        kernel = SBCLKernel()
        # Give it a second to boot
        import time; time.sleep(1)
    except FileNotFoundError:
        print("[ERROR] SBCL not found.")
        return

    # 2. Load Dataset
    try:
        with open(filepath, "r") as f:
            data = json.load(f)
    except Exception as e:
        print(f"[ERROR] Could not load JSON: {e}")
        kernel.shutdown()
        return

    # 3. Validate Each Entry
    all_valid = True
    for i, entry in enumerate(data):
        lisp_code = entry.get("gold_logic", "")
        print(f"\nEntry {i+1}: {lisp_code[:50]}...")
        
        # Verify Quoting (Kernel needs quote to treat as data)
        # However, if the dataset has it quoted inside the string, checking logic:
        # If string is "'((...))", we need to send it exactly like that?
        # The Kernel expects a read-able S-expression.
        # If main.py appends a quote, the optimizer uses this gold logic as a target.
        # The target output of the LLM should be exactly what we expect.
        # If we expect the LLM to output quoted text, then gold_logic should have the quote.
        
        is_valid = kernel.validate_expression(lisp_code)
        if is_valid:
            print("  [OK] Valid S-Expression")
        else:
            print("  [FAIL] Invalid S-Expression or System 2 Violation")
            all_valid = False

    # 4. Cleanup
    kernel.shutdown()
    
    if all_valid:
        print("\n[SUCCESS] All examples are valid.")
    else:
        print("\n[FAILURE] Some examples failed validation.")

if __name__ == "__main__":
    validate_dataset()
