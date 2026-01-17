# Dialectical Symbolic AI

> A Neuro-Symbolic architecture combining **DSPy optimized LLMs** with a Common Lisp Kernel (SBCL) for mathematically rigorous Knowledge Extraction and **Geometric Consistency** validation.

## 🦅 Overview

This repository houses the evolution of the **S-Dialectic** system, a framework dedicated to grounding the creative hallucinations of Large Language Models (LLMs) with the logical strictness of Symbolic AI (Common Lisp).

The project is structured as a **Monorepo** containing two distinct generations of the system:

### 📂 Directory Structure

#### 1. [`/sdialectic-ts`](./sdialectic-ts) (Unstable / Conceptually Promising)
The original implementation (V1).
- **Stack:** TypeScript (Backend/Frontend) + Common Lisp (SBCL).
- **Focus:** Regex-based extraction, Lisp-based geometric validation logic (Euclidean Distance), and basic Reactive Orchestration.
- **Status:** Unstable, experimental, but conceptually promising.

#### 2. [`/sdialectic-dspy`](./sdialectic-dspy) (Next-Gen / Active)
The future of the system (V2).
- **Stack:** Python (DSPy, MLX) + Common Lisp (SBCL).
- **Focus:** Replacing regex parsing with **DSPy Optimizers** (MIPROv2) and **Test-Time Compute** (Reasoning Models).
- **Goal:** To compile logical constraints directly into model weights via weak supervision from the Lisp Kernel.

## 🧠 Core Philosophy

We view the interaction between Neural Networks and Symbolic Logic as a *Dialectic*:
- **Thesis (Id):** The LLM generates creative, potentially inconsistent knowledge.
- **Antithesis (Super-Ego):** The Lisp Kernel applies geometric constraints ($S+P \approx O$) and logical rules.
- **Synthesis:** The system optimizes the "weights" or "prompts" to minimize the logical dissonance, producing grounded knowledge.

## 🚀 Getting Started

Please refer to the specific `README.md` in each subdirectory for installation and running instructions:

- [Go to TypeScript Documentation](./sdialectic-ts/README.md)
- [Go to DSPy Documentation](./sdialectic-dspy/README.md)

---
*Created by [Moraes](https://github.com/spock74)*
