# SDialectic Core v2.0 (Sovereign AI)

Arquitetura Neuro-Simbólica para validação axiomática de LLMs.

## Requisitos (Jan 2026)
- Conda (Miniconda ou Anaconda)
- SBCL 2.6.0 (Steel Bank Common Lisp) instalado e no PATH.
- Ollama v0.6.1 (opcional, para inferência local).

## Instalação (Via Conda)

1. Crie o ambiente:
   ```bash
   conda env create -f environment.yml
   ```
2. Ative o ambiente:
   ```bash
   conda activate sdialectic-env
   ```
3. Instale SBCL (se não tiver no sistema):
   ```bash
   sudo apt install sbcl  # Debian/Ubuntu
   # ou
   brew install sbcl      # macOS
   ```
4. Configure `.env` com suas chaves.

## Execução
`python main.py`

## Filosofia
- **System 1 (DSPy):** Extrai lógica de narrativas.
- **System 2 (SBCL):** Valida a lógica e bloqueia alucinações.
