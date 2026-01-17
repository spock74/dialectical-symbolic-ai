import dspy
from dspy.teleprompt import BootstrapFewShot
from dspy.evaluate import Evaluate
import os

# Define a simple signature for testing
class BasicQA(dspy.Signature):
    """Answer questions with short answers."""
    question = dspy.InputField()
    answer = dspy.OutputField(desc="Short answer")

def test_local_integration():
    print("[INFO] Testing Local Model Integration settings...")

    # 1. Setup Student Model (Local Ollama)
    # We assume 'llama3' or 'mistral' is pulled. We'll try a generic one or read from ENV.
    # If user hasn't pulled 'llama3', this might fail, so we wrap it.
    model_name = os.getenv("OLLAMA_MODEL", "phi4-mini:latest")
    base_url = os.getenv("OLLAMA_HOST", "http://localhost:11434")

    print(f"[INFO] Connecting to Ollama at {base_url} with model '{model_name}'...")
    
    try:
        # In DSPy 2.5+, Ollama is supported via dspy.OllamaLocal or similar.
        # Checking if dspy.OllamaLocal exists or we use dspy.LM with provider.
        # Let's try the modern way:
        student_lm = dspy.LM(model=f"ollama/{model_name}", api_base=base_url, stop=["\n"])
        
        # Simple health check (may fail if model not pulled)
        # We won't run a heavy inference, just check object creation.
        print(f"[SUCCESS] Student LM object created: {student_lm.model}")

    except Exception as e:
        print(f"[WARNING] Could not initialize Ollama LM. Ensure Ollama is running.\nError: {e}")
        student_lm = None

    # 2. Setup Teacher Model (Mocked)
    from unittest.mock import MagicMock
    import json
    
    mock_teacher = MagicMock(spec=dspy.LM)
    mock_teacher.model = "mock-teacher-gemini"
    
    # The teleprompter will ask the teacher to generate reasoning for the examples.
    # We need to provide "good" responses. 
    # BootstrapFewShot calls the teacher to validating/generating traces.
    
    def teacher_side_effect(*args, **kwargs):
        # Return a valid answer that matches the metric (gold.answer == pred.answer)
        # Input inspection would be better, but for a smoke test, successful answer is key.
        return [json.dumps({"answer": "Paris", "reasoning": "France capital is Paris"})]

    mock_teacher.side_effect = teacher_side_effect
    
    mock_teacher.side_effect = teacher_side_effect
    
    if student_lm:
        print("\n[INFO] 1. Testing Direct Inference (Student)...")
        try:
            # Simple Predict (no hidden reasoning fields)
            predictor = dspy.Predict(BasicQA)
            with dspy.settings.context(lm=student_lm):
                print(f"   Asking: 'What is the capital of France?'")
                response = predictor(question="What is the capital of France?")
                print(f"   Response: {response.answer}")
                if "Paris" in response.answer:
                    print("   [SUCCESS] Direct inference worked.")
                else:
                    print(f"   [WARNING] Direct inference got unexpected answer: {response.answer}")
        except Exception as e:
            print(f"   [ERROR] Direct inference failed: {e}")

        print("\n[INFO] 2. Attempting Compilation Workflow (Teacher -> Student)...")
        # Define a module
        prog = dspy.ChainOfThought(BasicQA)
        
        # Define Training Data
        trainset = [
            dspy.Example(question="What is the capital of France?", answer="Paris").with_inputs("question"),
        ]

        # Define Teleprompter
        # This acts as the "Compiler"
        teleprompter = BootstrapFewShot(metric=lambda gold, pred, trace: gold.answer == pred.answer, max_bootstrapped_demos=1)
        
        try:
            # Note: We configure the environment so the teleprompter sees the models
            with dspy.settings.context(lm=student_lm, teacher=mock_teacher):
                print("[INFO] Starting Compilation (BootstrapFewShot)...")
                
                # Mocking the student logic if it fails to connect/serve
                # But here we genuinely try to use the student_lm defined above.
                # If student_lm is 'ollama/llama3', it will try to hit localhost.
                
                compiled_prog = teleprompter.compile(prog, trainset=trainset)
                
                print("[SUCCESS] Compilation finished. Program optimized for Local Model.")
                
                # Verify the program has demos
                # With BootstrapFewShot(max_bootstrapped_demos=4), it should have some.
                demos_count = 0
                if hasattr(compiled_prog, "demos"):
                    demos_count = len(compiled_prog.demos)
                    print(f"[INFO] Compiled program has {demos_count} few-shot demonstrations.")
                else:
                    print("[WARNING] Compiled program has no 'demos' attribute. (Did optimization fail?)")
                
                # If we had 0 traces, that's likely due to model availability.
                if demos_count == 0:
                     print("[WARNING] Zero demos created. Check teacher/student compatibility.")

        except Exception as e:
            print(f"[ERROR] Compilation failed: {e}")
            if "Connection refused" in str(e) or "ConnectError" in str(e):
                print("[TIP] Is Ollama running? Run `ollama serve` and pull the model.")
    else:
        print("[SKIP] Skipping compilation test because Student LM is unavailable.")

if __name__ == "__main__":
    test_local_integration()
