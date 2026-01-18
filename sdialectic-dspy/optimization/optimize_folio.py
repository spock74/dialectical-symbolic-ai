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

def load_dataset(filepath):
    """Loads JSON dataset and converts to DSPy Examples."""
    full_path = os.path.join(os.path.dirname(__file__), '..', filepath)
    if not os.path.exists(full_path):
        print(f"[WARN] Dataset not found: {filepath}")
        return []
        
    with open(full_path, "r") as f:
        data = json.load(f)
        
    examples = []
    for entry in data:
        # Create Example
        # Inputs: document_chunk
        # Labels: lisp_code (can be empty str), logic_reasoning
        ex = dspy.Example(
            document_chunk=entry['input'],
            lisp_code=entry.get('gold_logic', ""), 
            logic_reasoning=entry.get('reasoning', "")
        ).with_inputs('document_chunk')
        examples.append(ex)
    return examples

def main():
    print("==========================================")
    print("   S-DIALECTIC: FOLIO OPTIMIZER (V2.5)    ")
    print("   Hybrid Strategy: Golden Set + FOLIO    ")
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

    # 2. Load Data (Hybrid Strategy)
    # A. Golden Set (High precision seeds)
    golden_set = load_dataset("datasets/golden_logic.json")
    print(f"[INFO] Loaded {len(golden_set)} Golden Examples.")
    
    # B. FOLIO (High volume/complexity seeds)
    folio_set = load_dataset("datasets/folio_processed.json")
    print(f"[INFO] Loaded {len(folio_set)} FOLIO Examples.")
    
    # Sample FOLIO (Don't use all 1000 for speed, start with 30)
    FOLIO_SAMPLE_SIZE = 30
    folio_sample = random.sample(folio_set, min(len(folio_set), FOLIO_SAMPLE_SIZE))
    print(f"[INFO] Sampled {len(folio_sample)} FOLIO examples for training.")
    
    # Combined Training Set
    train_data = golden_set + folio_sample
    # Shuffle
    random.shuffle(train_data)
    
    print(f"[INFO] Total Training Set: {len(train_data)} examples.")
    
    # Validation Set (Use Golden Set to ensure we don't regress on known goods)
    val_data = golden_set 
    
    # 3. Define Optimizer
    # MIPROv2: Multi-prompt Instruction Proposal Optimizer
    teleprompter = MIPROv2(
        metric=sbcl_metric,
        auto="light",
        num_threads=1 # Avoid thread issues with SBCL pipe
    )
    
    # 4. Compile
    print("[INFO] Starting MIPROv2 Compilation (Hybrid)...")
    try:
        optimized_program = teleprompter.compile(
            dspy.ChainOfThought(NarrativeToLogic),
            trainset=train_data,
            valset=val_data, # Validate against strict Golden rules
            max_bootstrapped_demos=2,
            max_labeled_demos=2,
            requires_permission_to_run=False
        )
        
        print("[SUCCESS] Folio Optimization Completed!")
        
        # 5. Save
        output_path = os.path.join(os.path.dirname(__file__), '../dspy_modules/optimized_narrative_folio.json')
        optimized_program.save(output_path)
        print(f"[INFO] Saved to: {output_path}")

    except Exception as e:
        print(f"[CRITICAL] Optimization Failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
