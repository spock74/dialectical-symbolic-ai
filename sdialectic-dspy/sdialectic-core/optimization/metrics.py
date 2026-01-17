import dspy
from core.kernel.sbcl_bridge import SBCLKernel

# Inicializa o kernel temporariamente para validação
# Nota: Em prod, isso seria injetado ou gerenciado como serviço
kernel = SBCLKernel()

def sbcl_metric(gold, pred, trace=None):
    """
    Métrica personalizada do DSPy.
    Não compara com gabarito de texto. Compara com a validade lógica.
    """
    lisp_code = pred.lisp_code
    
    # 1. Validação Básica de String (Parenteses)
    if lisp_code.count('(') != lisp_code.count(')'):
        return 0 # Fail imediato
        
    # 2. Validação Axiomática via System 2
    is_valid = kernel.validate_expression(lisp_code)
    
    if is_valid:
        return 1.0 # Sucesso total
    else:
        return 0.0 # Rejeitado pelo Kernel
