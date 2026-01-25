import os
import sys
import dspy
import json
from dspy.evaluate import Evaluate

# Add project root to sys.path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from core.config import settings
from optimization.metrics import sbcl_metric

# Define the signature specifically for this Evaluation (Direct Extraction)
# We assume the fine-tuned model outputs Lisp directly or close to it.
class QwenSignature(dspy.Signature):
    """
    Direct extraction of Lisp S-Expressions from biomedical text.
    Output MUST be a valid Common Lisp S-Expression logic.
    """
    document_chunk = dspy.InputField(desc="biomedical text")
    lisp_code = dspy.OutputField(desc="Lisp S-Expressions")

def load_dataset(filepath):
    print(f"[INFO] Loading dataset from {filepath}...")
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        examples = []
        for item in data:
            # Normalize output to string
            output_val = item['output']
            if isinstance(output_val, list):
                output_val = "\n".join(output_val)
            
            # DSPy Example
            ex = dspy.Example(document_chunk=item['input'], lisp_code=output_val).with_inputs('document_chunk')
            examples.append(ex)
            
        print(f"[INFO] Loaded {len(examples)} examples.")
        return examples
    except FileNotFoundError:
        print(f"[ERROR] File not found {filepath}")
        return []

def main():
    print("==========================================")
    print(f"   EVALUATION: {settings.STUDENT_MODEL_NAME}")
    print("==========================================")
    
    # 1. Setup Student Model (Ollama)
    # The 'ollama/' prefix is handled by dspy.LM if configured correctly, 
    # but dspy.Ollama is safer for older versions, or dspy.LM with api_base.
    # settings.STUDENT_MODEL_NAME should be "ollama/qwen-medico-spv:latest"
    
    try:
        lm = dspy.LM(model=settings.STUDENT_MODEL_NAME, api_base=settings.OLLAMA_HOST, stop=['\n\n'])
        dspy.settings.configure(lm=lm)
        print(f"[INFO] Connected to {settings.STUDENT_MODEL_NAME} at {settings.OLLAMA_HOST}")
    except Exception as e:
        print(f"[ERROR] Failed to connect to model: {e}")
        return

    # 2. Load Dataset
    # Path relative to project root
    dataset_path = os.path.join(os.path.dirname(__file__), '../datasets/test_dataset_qwen_spv_01.json')
    devset = load_dataset(dataset_path)
    if not devset:
        return

    # 3. Define Predictor
    # We use dspy.Predict which sends the signature instructions.
    # If the model is heavily fine-tuned, it might ignore instructions, but that's what we want to test.
    predictor = dspy.Predict(QwenSignature)

    # 4. Run Evaluation
    print("[INFO] Starting Evaluation Loop...")
    
    # Use sbcl_metric which checks: Hallucinations, Parentheses, Logic Validity (via Lisp Kernel)
    evaluator = Evaluate(devset=devset, metric=sbcl_metric, num_threads=1, display_progress=True, display_table=True)
    
    try:
        score = evaluator(predictor)
        print(f"\n==========================================")
        print(f"FINAL SCORE: {score} (0.0 - 1.0)")
        print(f"==========================================")
        
        # Save results if needed (Evaluate/DSPy usually prints table)
        
    except Exception as e:
        print(f"[CRITICAL] Evaluation Failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    # Ensure Lisp Kernel is reachable for the metric
    main()
