from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import dspy
from dspy_modules.signatures import NarrativeToLogic
from core.kernel.sbcl_bridge import SBCLKernel
import os

app = FastAPI(title="SDialectic Core v2", version="2.0.0")

# Setup DSPy (Teacher Model)
# No futuro, isso usará o Ollama Local se a variável estiver setada
gemini = dspy.Google("models/gemini-2.5-flash-lite", api_key=os.getenv("GEMINI_API_KEY"))
dspy.settings.configure(lm=gemini)

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

@app.post("/ingest/atomize")
async def atomize_text(req: IngestRequest):
    """
    Pipeline: Narrativa -> DSPy (CoT) -> Lisp -> SBCL Check
    """
    # 1. System 1: Extraction
    extractor = dspy.ChainOfThought(NarrativeToLogic)
    prediction = extractor(document_chunk=req.text)
    
    # 2. System 2: Axiomatic Validation
    if kernel:
        is_valid = kernel.validate_expression(prediction.lisp_code)
        status = "ACCEPTED" if is_valid else "BLOCKED_BY_AXIOMS"
    else:
        status = "KERNEL_OFFLINE"
        
    return {
        "logic": prediction.lisp_code,
        "reasoning": prediction.logic_reasoning,
        "system2_status": status
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
