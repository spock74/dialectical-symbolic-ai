import os
import dspy
from dspy.teleprompt import MIPROv2
from dspy.evaluate import Evaluate

# Add project root to sys.path
import sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from dspy_modules.signatures import NarrativeToLogic
from optimization.metrics import sbcl_metric
from optimization.datasets.training_data import training_data, validation_data
from core.config import settings

def main():
    print("==========================================")
    print("       S-DIALECTIC: DSPy OPTIMIZER        ")
    print("==========================================")
    
    # 1. Configure Models
    print(f"[INFO] Student Model: {settings.STUDENT_MODEL_NAME} ({settings.OLLAMA_HOST})")
    print(f"[INFO] Teacher Model: {settings.TEACHER_MODEL_NAME}")

    if not settings.GEMINI_API_KEY:
        print("[ERROR] GEMINI_API_KEY not found in environment (or .env). Optimization requires a smart teacher.")
        return

    # Initialize Student (Ollama)
    # Student model name in config can include 'ollama/' prefix or not.
    # dspy.LM might expect "ollama/phi4-mini:latest" or just "phi4-mini:latest" if api_base is given.
    # Our config default is "ollama/phi4-mini:latest".
    try:
        student_lm = dspy.LM(model=settings.STUDENT_MODEL_NAME, api_base=settings.OLLAMA_HOST, stop=["\n\n"])
    except Exception as e:
        print(f"[ERROR] Failed to connect to Student LM: {e}")
        return

    # Initialize Teacher (Gemini)
    # Using dspy.LM which uses litellm under the hood.
    try:
        teacher_lm = dspy.LM(model=settings.TEACHER_MODEL_NAME, api_key=settings.GEMINI_API_KEY)
    except Exception as e:
        print(f"[ERROR] Failed to connect to Teacher LM: {e}")
        return

    # Context: Teacher teaches Student
    dspy.settings.configure(lm=student_lm, teacher=teacher_lm)

    # 2. Define Program
    program = dspy.ChainOfThought(NarrativeToLogic)

    # 3. Define Optimizer (MIPROv2)
    # MIPROv2 generates instructions and few-shot examples.
    print("[INFO] Initializing MIPROv2...")
    teleprompter = MIPROv2(
        metric=sbcl_metric,
        auto="light", # 'light' optimizes instructions + demos.
        num_threads=1 # CRITICAL: Subprocess pipe is not thread-safe. Run serially.
    )

    # 4. Run Optimization
    # This takes time!
    print(f"[INFO] Starting Compilation with {len(training_data)} training examples...")
    print("       This process optimizes the Prompt Instructions and Few-Shot Selection.")
    
    try:
        optimized_program = teleprompter.compile(
            program,
            trainset=training_data,
            valset=validation_data
        )
        
        print("[SUCCESS] Optimization Completed!")
        
        # 5. Save Artifact
        output_path = os.path.join(os.path.dirname(__file__), '../dspy_modules/optimized_narrative.json')
        optimized_program.save(output_path)
        print(f"[INFO] Optimized program saved to: {output_path}")
        
    except Exception as e:
        print(f"[CRITICAL] Optimization Failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
