from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import dspy
from dspy_modules.signatures import NarrativeToLogic
from core.kernel.sbcl_bridge import SBCLKernel
from core.config import settings
import os

app = FastAPI(title=settings.SERVICE_NAME, version=settings.VERSION)

# Setup DSPy (Student Model - Execution Mode)
# Uses the central configuration and points to the local optimized student
try:
    student_lm = dspy.LM(model=settings.STUDENT_MODEL_NAME, api_base=settings.OLLAMA_HOST, stop=["\n\n"])
    dspy.settings.configure(lm=student_lm)
except Exception as e:
    print(f"[WARNING] Failed to connect to Student LM at startup: {e}")
    # Fallback to teacher if student fails? For now, we want stricly student as per optimized plan.
    pass

# Kernel Global
kernel = None

@app.on_event("startup")
def startup_event():
    global kernel
    try:
        kernel = SBCLKernel()
    except FileNotFoundError:
        print("[WARNING] 'sbcl' executable not found. System 2 inactive.")

@app.on_event("shutdown")
def shutdown_event():
    if kernel:
        kernel.shutdown()

class IngestRequest(BaseModel):
    text: str
    persist: bool = False

@app.post("/ingest/atomize")
async def atomize_text(req: IngestRequest):
    """
    Pipeline: Narrativa -> DSPy (CoT) -> Lisp -> SBCL Check
    """
    # 1. System 1: Extraction (Optimized)
    # Load the optimized program state
    optimized_path = os.path.join(os.path.dirname(__file__), 'dspy_modules/optimized_narrative.json')
    
    extractor = dspy.ChainOfThought(NarrativeToLogic)
    
    if os.path.exists(optimized_path):
        try:
            extractor.load(optimized_path)
            # print(f"[INFO] Loaded optimized weights from {optimized_path}")
        except Exception as e:
            print(f"[WARNING] Could not load optimized weights: {e}. Using zero-shot.")
    else:
        print(f"[WARNING] Optimized artifact not found at {optimized_path}. Using zero-shot.")

    # Post-processing: Cleanup Lisp
    prediction = extractor(document_chunk=req.text)
    
    # 1. Sanitize (Remove '?' and uppercase)
    clean_logic = prediction.lisp_code.replace("?", "").upper()
    
    # 2. Balance Parentheses
    open_count = clean_logic.count("(")
    close_count = clean_logic.count(")")
    if close_count > open_count:
        # Remove trailing excess
        diff = close_count - open_count
        clean_logic = clean_logic[:-diff] # Simply remove excess from right
    elif open_count > close_count:
        # Add missing
        diff = open_count - close_count
        clean_logic += ")" * diff
        
    # 3. Ensure it's treated as data by the kernel (Quote it)
    if not clean_logic.startswith("'"):
        clean_logic = f"'{clean_logic}"
        
    prediction.lisp_code = clean_logic
    
    # 2. System 2: Axiomatic Validation
    if kernel:
        is_valid = kernel.validate_expression(prediction.lisp_code)
        status = "ACCEPTED" if is_valid else "BLOCKED_BY_AXIOMS"
    else:
        status = "KERNEL_OFFLINE"
        is_valid = False

    # 3. Persistence (The "Dual-Write" Protocol - Phase 1)
    # Only persist if explicitly requested AND if valid.
    if req.persist and is_valid and status == "ACCEPTED":
        try:
            lisp_file_path = os.path.join(os.path.dirname(__file__), 'core/kernel/imported_knowledge.lisp')
            with open(lisp_file_path, "a") as f:
                # Add a timestamped comment? Maybe later.
                # Ensure newline
                f.write(f"\n{prediction.lisp_code}")
                
            print(f"[INFO] Persisted fact to {lisp_file_path}")
            
            # Optional: Hot-load into current kernel? 
            # For now, it will be loaded on next restart.
            # To hot-load, we could send it to stdin again, but validate_expression is pure-check.
            # We would need a kernel.exec() method.
            
        except Exception as e:
            print(f"[ERROR] Failed to persist: {e}")

    return {
        "logic": prediction.lisp_code,
        "reasoning": prediction.logic_reasoning,
        "system2_status": status,
        "persisted": (req.persist and is_valid)
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
