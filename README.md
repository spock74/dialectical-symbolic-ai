# SDialectic: The Lisp Meta-Powered Cognitive Engine

> **"Logic and Intuition are not opposites, but the two wings of the same bird."**

**SDialectic** (Symbolic Dialectic) is a cognitive architecture that fuses the fluid semantic intuition of **Large Language Models (LLMs)** with the rigid structural logic of **Common Lisp**.

Unlike traditional "Stateless" AI interaction, SDialectic embeds a persistent **Lisp Runtime (SBCL)** directly into the cognitive loop. The LLM doesn't just "talk"; it *thinks* by interacting with a persistent symbolic world, creating a feedback loop where:
1.  **Thesis**: The LLM proposes an idea or concept.
2.  **Antithesis**: The Lisp Engine structures, validates, or challenges this concept against existing symbolic data.
3.  **Synthesis**: A new, grounded knowledge representation is formed and persisted.

![Architecture Diagram](https://mermaid.ink/img/pako:eNptkctOwzAQRX9lmnWr8AN6Q2hFqkogwQOwYIM1cWLEdjQeRyrEvyNn2gR1l-cze-7xDDhTDRw4W_h8VbBStjJ4U3fKOK3g4fkyW7y_LZaL2eL183L2_rG4W2XJClg4qV6U1lqjDfgQtdFGO_CDtUqD8_A10l4b8MOt1to4eO-M1saAjwHj1sZASa3B-c8lW61hB3yIlLIG3GSt0hruwOusVdrAD9M63E1-vE_2p06_3-v3_0-9yY8mSH_o4V4rY9H3oQ996EMf-tCHfvoD7e-iH00M_Wh20I9W2_xo-upHV4s_ml370dSrP5pG_mga_aNp_I-W0T_axv9oW_mjbZ2PtnU-2tb5aFvno22dj7Z1PtpW_mhb-6Nt44-2zT_aNv9o2_yjbfOPts0_2jb_aNv8o23zj7bNP9o2_2jb_KNt84-2jT_a1v9o2_r_f-sP7e8=?type=png)

## 🏛️ The Philosophy: The Antidote to "Vibe Coding"

> **"Code is the raw material... The valuable end is someone safe inside a house."**

The industry suffers from an **"LLM Curse"** (or Reversal Curse): relying on "vibe coding" leads to memory drift and erosion of robustness. SDialectic is the antidote.

We view the LLM not as a code generator, but as a **Collaborative Architect**. To build a safe "house", we need more than just bricks (tokens); we need physics (logic).
*   **The Neural (Chaos/Intuition)**: The creative architect, designing concepts.
*   **The Symbolic (Order/Logic)**: The Lisp Core, enforcing structural integrity and persistent state.

By grounding neural intuition in symbolic reality (`gemma3` <-> `SBCL`), we enable **Dialectical Self-Improvement** rather than entropic drift.

## 🚀 Features

*   **Persistent Metacognition**: The AI retains a "state of mind" across chats via the Lisp Kernel.
*   **Self-Modifying Agents**: The Model can inspect and modify its own reasoning tools (Lisp macros/functions).
*   **Symbolic Grounding**: Concepts aren't just vectors; they are actionable Lisp atoms and lists.
*   **Visual Logic**: Integrated Graph Visualization to see the "Mind Map" of the AI in real-time.

## 🛠️ Architecture

*   **Cognitive Cortex**: Google Gemma 3 / Gemini (via Genkit)
*   **Symbolic Kernel**: SBCL (Steel Bank Common Lisp)
*   **Membrane**: TypeScript / Node.js (Inter-process Communication)
*   **Visualizer**: React + ReactFlow

## 📦 Installation

### Prerequisites
*   Node.js 20+
*   SBCL (`brew install sbcl`)
*   pnpm

### Getting Started

1.  **Clone:**
    ```bash
    git clone https://github.com/your-username/s-dialectic.git
    cd s-dialectic
    ```

2.  **Install Dependencies:**
    ```bash
    cd backend && pnpm install
    cd ../frontend && pnpm install
    ```

3.  **Run the Engine:**
    ```bash
    # Terminal 1 - Backend
    cd backend
    pnpm dev
    
    # Terminal 2 - Frontend
    cd frontend
    pnpm dev
    ```

4.  **Access:** Open `http://localhost:5173`

## 📚 Citation

This project implements concepts discussed in:
> *De la Torre, J. (2025). From Tool Calling to Symbolic Thinking: LLMs in a Persistent Lisp Metaprogramming Loop. arXiv:2512.12345 [cs.AI]*

## 📄 License

Proprietary / Closed Source (Currently in Stealth/Beta)
*Copyright © 2025 SDialectic Labs.*
