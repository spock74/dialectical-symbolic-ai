import subprocess
import time
from typing import Optional

class SBCLKernel:
    """
    Mantém um processo filho SBCL persistente.
    Atua como o 'Juiz' para as métricas do DSPy.
    """
    def __init__(self, sbcl_path="sbcl"):
        self.process = subprocess.Popen(
            [sbcl_path, "--noinform", "--noprint", "--disable-debugger", "--load", "core/kernel/bootstrap.lisp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0
        )
        print(f"[INFO] SBCL Kernel started (PID: {self.process.pid})")

    def validate_expression(self, sexpr: str) -> bool:
        """
        Envia uma S-Expression para o Kernel.
        Retorna True se for lógico/seguro, False se violar axiomas.
        """
        try:
            # Envia o comando para o STDIN do Lisp
            self.process.stdin.write(f"{sexpr}\n")
            self.process.stdin.flush()
            
            # Lê linhas até encontrar uma resposta válida ou timeout (simulado)
            max_lines = 5
            for _ in range(max_lines):
                output = self.process.stdout.readline().strip()
                # print(f"DEBUG LISP: {output}") # Descomente para debug

                if "VIOLATION" in output or "ERROR" in output:
                    return False
                if "SAFE" in output or "VALID" in output:
                    return True
                
            return False
        except Exception as e:
            print(f"[ERROR] Kernel Panic: {e}")
            return False

    def shutdown(self):
        self.process.terminate()

# Instância Global (Singleton)
# kernel = SBCLKernel()
