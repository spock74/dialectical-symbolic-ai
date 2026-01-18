# SDialectic Core v2.0 (Sovereign AI)

Neuro-Symbolic Architecture for axiomatic validation of LLMs.

## Requirements (Jan 2026)
- Conda (Miniconda or Anaconda)
- SBCL 2.6.0 (Steel Bank Common Lisp) installed and in PATH.
- Ollama v0.6.1 (optional, for local inference).

## Installation (Via Conda)

1. Create the environment:
   ```bash
   conda env create -f environment.yml
   ```
2. Activate the environment:
   ```bash
   conda activate sdialectic-env
   ```
3. Install SBCL (if not already on the system):
   ```bash
   sudo apt install sbcl  # Debian/Ubuntu
   # or
   brew install sbcl      # macOS
   ```
4. Configure `.env` with your API keys.

## Execution
`python main.py`

## Philosophy
- **System 2 (SBCL):** Validates logic and blocks hallucinations.

## Experiments

### 1. Logical Narrative Optimization (FOLIO Dataset)
*Status: Completed on 2026-01-18*

The system used `MIPROv2` with the FOLIO dataset and our hybrid metric (Syntax+Connectivity). The result (`optimized_narrative_folio.json`) demonstrated the emergence of a highly specialized "Lisp Prompt Engineer".

#### What Did the System Learn by Itself?
The optimizer converged to a precise technical instruction that maximizes compatibility with SBCL:
> *"Convert complex narrative text into symbolic logic represented as valid Common Lisp S-Expressions... Ensure UPPERCASE notation... the output should be a list of lists parseable via SBCL."*

#### Caveats
Several S-expressions were discarded by SBCL, which may cause a severe optimization bias (survivorship bias). To address this, changes are planned (e.g., Error-Driven Self-Correction).

#### Performance
- **Final Score:** 100.0 (Validation Set).
- **Resilience:** The system learned to use compound predicates (`IS-A`, `FAMILIAR_WITH`) and snake_case (`LAWYER_JAMES_COCKS`) to avoid syntax errors in Lisp.

## Deployment & Scaling (Kaggle/Colab) - Roadmap

To scale optimization (`MIPROv2`) beyond local hardware limits, we employ a **Static Binary Injection** strategy:

1.  **Docker Build (Local):** Compile SBCL and dependencies (`quicklisp`, libs) into a single monolithic executable using `sb-ext:save-lisp-and-die`.
2.  **Dataset Injection:** Upload this binary as a "Dataset" on Kaggle.
3.  **Zero-Setup Runtime:** The notebook simply copies and executes the binary.
    *   **Advantage 1:** Zero installation latency (skips `apt-get` and compilation).
    *   **Advantage 2:** Exact reproducibility of the local environment.
    *   **Advantage 3:** Bypasses root restrictions in high-performance free cloud environments (TPU/GPU).
