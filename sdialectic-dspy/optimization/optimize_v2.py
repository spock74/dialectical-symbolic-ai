import os
import sys
import json
import dspy
import random
from dspy.teleprompt import MIPROv2

# Add project root to sys.path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from dspy_modules.signatures import NarrativeToLogic
from optimization.metrics import sbcl_metric
from core.config import settings

def load_dataset(filepath="datasets/golden_logic.json"):
    """Loads JSON dataset and converts to DSPy Examples."""
    full_path = os.path.join(os.path.dirname(__file__), '..', filepath)
    with open(full_path, "r") as f:
        data = json.load(f)
        
    examples = []
    for entry in data:
        # Create Example
        # Inputs: document_chunk
        # Labels: lisp_code, logic_reasoning
        ex = dspy.Example(
            document_chunk=entry['input'],
            lisp_code=entry['gold_logic'],
            logic_reasoning=entry['reasoning']
        ).with_inputs('document_chunk')
        examples.append(ex)
    return examples

def main():
    print("==========================================")
    print("   S-DIALECTIC: DSPy OPTIMIZER (V2)       ")
    print("==========================================")
    
    # Check API Key
    if not settings.GEMINI_API_KEY:
        print("[ERROR] GEMINI_API_KEY missing.")
        return

    # 1. Configure Models
    try:
        # Student: Ollama (phi4-mini)
        student_lm = dspy.LM(model=settings.STUDENT_MODEL_NAME, api_base=settings.OLLAMA_HOST, stop=["\n\n"])
        # Teacher: Gemini (Flash)
        teacher_lm = dspy.LM(model=settings.TEACHER_MODEL_NAME, api_key=settings.GEMINI_API_KEY)
        
        dspy.settings.configure(lm=student_lm, teacher=teacher_lm)
        print("[INFO] Models Configured.")
    except Exception as e:
        print(f"[ERROR] Model configuration failed: {e}")
        return

    # 2. Load Data
    full_set = load_dataset()
    print(f"[INFO] Loaded {len(full_set)} examples.")
    
    # Split (For Proof of Concept with tiny data, use same for train/val)
    # In prod, strict split needed.
    train_data = full_set
    val_data = full_set # Overfitting risk, but acceptable for syntax alignment test
    
    # 3. Define Optimizer
    # MIPROv2: Multi-prompt Instruction Proposal Optimizer
    teleprompter = MIPROv2(
        metric=sbcl_metric,
        auto="light",
        num_threads=1 # Avoid thread issues with SBCL pipe
    )
    
    # 4. Compile
    print("[INFO] Starting MIPROv2 Compilation...")
    try:
        optimized_program = teleprompter.compile(
            dspy.ChainOfThought(NarrativeToLogic),
            trainset=train_data,
            valset=val_data,
            max_bootstrapped_demos=2,
            max_labeled_demos=2,
            requires_permission_to_run=False
        )
        
        print("[SUCCESS] Optimization Completed!")
        
        # 5. Save
        output_path = os.path.join(os.path.dirname(__file__), '../dspy_modules/optimized_narrative_v2.json')
        optimized_program.save(output_path)
        print(f"[INFO] Saved to: {output_path}")

    except Exception as e:
        print(f"[CRITICAL] Optimization Failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
