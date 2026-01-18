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
- **System 2 (SBCL):** Valida a lógica e bloqueia alucinações.


### TODO

## Deployment & Scaling (Kaggle/Colab) - In very near future


Para escalar a otimização (MIPROv2) além do hardware local, utilizamos uma estratégia de **Injeção de Binário Estático**:

1.  **Docker Build (Local):** Compilamos o SBCL e dependências (`quicklisp`, libs) em um único executável monolítico usando `sb-ext:save-lisp-and-die`.
2.  **Dataset Injection:** Subimos este binário como um "Dataset" no Kaggle.
3.  **Zero-Setup Runtime:** O notebook apenas copia e executa o binário.
    *   **Vantagem 1:** Zero latência de instalação (pula `apt-get` e compilação).
    *   **Vantagem 2:** Reprodutibilidade exata do ambiente local.
    *   **Vantagem 3:** Bypassa restrições de root em ambientes cloud gratuitos de alta performance (TPU/GPU).
