import dspy
from core.kernel.sbcl_bridge import SBCLKernel

# Initialize kernel globally (Note: In prod, inject this dependency)
kernel = SBCLKernel()

def sbcl_metric(gold, pred, trace=None):
    """
    Custom DSPy Metric.
    Does NOT compare with gold text. Compares with LOGICAL VALIDITY via SBCL.
    """
    lisp_code = pred.lisp_code
    
    # 1. Basic Syntax Validation (Parentheses)
    if lisp_code.count('(') != lisp_code.count(')'):
        return 0 # Immediate Fail
        
    # 2. Axiomatic Validation via System 2
    is_valid = kernel.validate_expression(lisp_code)
    
    if is_valid:
        return 1.0 # Success
    else:
        return 0.0 # Rejected by Kernel
