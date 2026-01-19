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
from dspy_modules.signatures import NarrativeToLogic
from optimization.metrics import sbcl_metric
from core.config import settings
from optimization.instrumentation import GeminiLogger
from core.gemini_client import NativeGeminiClient

# ==============================================================================
# CONFIGURATION CONSTANTS
# ==============================================================================

# File Paths
BIORED_DATASET_PATH = "datasets/biored_processed.json"
GOLDEN_DATASET_PATH = "datasets/golden_logic.json"
OUTPUT_MODULE_PATH = "../dspy_modules/optimized_narrative_biored.json"
USAGE_LOG_FILE = "gemini_usage.csv"

# Data Selection
# Set NUM_TRAIN_SAMPLES to an integer for FAST MODE, or None for full dataset
NUM_TRAIN_SAMPLES = 5  # DEBUG MODE
# Set NUM_VAL_SAMPLES to integer for subset, or None to use full Golden Set
NUM_VAL_SAMPLES = None 

# Model Configuration
STUDENT_MAX_TOKENS = 1000
STUDENT_STOP_SEQUENCES = ["\n\n"]

# MIPROv2 Optimizer Settings
MIPRO_AUTO_SETTINGS = "light"
MIPRO_NUM_TRIALS = 10
MIPRO_MINIBATCH_SIZE = 4
MIPRO_NUM_THREADS = 1  # Keep at 1 to avoid thread safety issues with SBCL Kernel
MIPRO_MAX_BOOTSTRAPPED_DEMOS = 2
MIPRO_MAX_LABELED_DEMOS = 2
MIPRO_REQUIRES_PERMISSION = False

# ==============================================================================

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
    print("   S-DIALECTIC: BIORED OPTIMIZER (V1.0)   ")
    print("   Domain Adaptation: Biomedical Science   ")
    print("==========================================")
    
    # Check API Key
    if not settings.GEMINI_API_KEY:
        print("[ERROR] GEMINI_API_KEY missing.")
        return

    # 1. Configure Models
    try:
        # Student: Ollama (phi4-mini)
        student_lm = dspy.LM(
            model=settings.STUDENT_MODEL_NAME, 
            api_base=settings.OLLAMA_HOST, 
            stop=STUDENT_STOP_SEQUENCES, 
            max_tokens=STUDENT_MAX_TOKENS
        )
        
        # Teacher: Gemini (Flash) - WRAPPED WITH LOGGER
        # Using Native Client to bypass LiteLLM as requested
        # Model name from .env is now "gemini-2.5-flash-lite" (clean)
        teacher_model_id = settings.TEACHER_MODEL_NAME
            
        raw_teacher = NativeGeminiClient(model=teacher_model_id, api_key=settings.GEMINI_API_KEY)
        teacher_lm = GeminiLogger(raw_teacher, filename=USAGE_LOG_FILE)
        
        dspy.settings.configure(lm=student_lm, teacher=teacher_lm)
        print("[INFO] Models Configured (Teacher Log Active).")
    except Exception as e:
        print(f"[ERROR] Model configuration failed: {e}")
        return

    # 2. Load Data
    biored_set = load_dataset(BIORED_DATASET_PATH)
    print(f"[INFO] Loaded {len(biored_set)} BioRED Examples.")
    
    golden_set = load_dataset(GOLDEN_DATASET_PATH)
    print(f"[INFO] Loaded {len(golden_set)} Golden Examples.")
    
    # Combined Training Set
    train_data = biored_set + golden_set
    random.shuffle(train_data)
    
    # Apply Sampling (Fast Mode vs Full Mode)
    if NUM_TRAIN_SAMPLES is not None:
        train_data = train_data[:NUM_TRAIN_SAMPLES]
        print(f"[WARN] FAST MODE: Reduced training set to {len(train_data)} examples.")
    else:
        print(f"[INFO] Using full training set: {len(train_data)} examples.")
    
    # Validation Set
    if NUM_VAL_SAMPLES is not None:
        val_data = random.sample(biored_set, min(len(biored_set), NUM_VAL_SAMPLES))
    else:
        val_data = golden_set # Default: Validate against strict Lisp validity on Golden
    
    # 3. Define Optimizer
    teleprompter = MIPROv2(
        metric=sbcl_metric,
        auto=MIPRO_AUTO_SETTINGS,
        num_threads=MIPRO_NUM_THREADS
    )
    
    # 4. Compile
    print(f"[INFO] Starting MIPROv2 Compilation with {MIPRO_NUM_TRIALS} trials...")
    try:
        optimized_program = teleprompter.compile(
            dspy.ChainOfThought(NarrativeToLogic),
            trainset=train_data,
            valset=val_data, 
            max_bootstrapped_demos=MIPRO_MAX_BOOTSTRAPPED_DEMOS,
            max_labeled_demos=MIPRO_MAX_LABELED_DEMOS,
            requires_permission_to_run=MIPRO_REQUIRES_PERMISSION,
            minibatch_size=MIPRO_MINIBATCH_SIZE
        )
        
        print("[SUCCESS] BioRED Optimization Completed!")
        
        # 5. Save
        output_abs_path = os.path.join(os.path.dirname(__file__), OUTPUT_MODULE_PATH)
        optimized_program.save(output_abs_path)
        print(f"[INFO] Saved to: {output_abs_path}")

    except Exception as e:
        print(f"[CRITICAL] Optimization Failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
