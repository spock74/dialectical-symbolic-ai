# NeuroLisp: The Cognitive Metaprogramming Environment

![License](https://img.shields.io/badge/license-MIT-blue.svg) ![Status](https://img.shields.io/badge/status-experimental-orange.svg) ![Stack](https://img.shields.io/badge/stack-Genkit%20%7C%20React%20%7C%20SBCL-purple.svg)

> **"From Tool Calling to Symbolic Thinking"**

**NeuroLisp** is a cutting-edge neuro-symbolic architecture that fuses the semantic intuition of Large Language Models (LLMs) with the logical rigor of Common Lisp. By embedding a persistent Lisp Runtime (SBCL) directly into the cognitive loop of the AI, we enable true **Metacognition**: the ability for the Model to write, execute, and refine its own reasoning tools in real-time.

[🇧🇷 Ler em Português](#neurolisp-o-ambiente-de-metaprogramação-cognitiva)

---

## 🧠 The Concept

Traditional LLM interactions are ephemeral: the model "thinks" only while generating tokens. **NeuroLisp** changes this paradigm by giving the LLM a persistent **Symbolic Memory**.

Inspired by the groundbreaking paper **"From Tool Calling to Symbolic Thinking: LLMs in a Persistent Lisp Metaprogramming Loop" (de la Torre et al., 2025)**, this project implements a *Reflective Loop* where:
1.  **Intuition**: The LLM (Gemma 3) analyzes unstructured data (PDFs, Images, Chat).
2.  **Translation**: It converts insights into symbolic S-expressions `(remember 'concept "definition")`.
3.  **Reasoning**: A live SBCL process interprets these symbols, enforcing logic and consistency.
4.  **Persistence**: The state remains alive across the conversation—a true "Stream of Consciousness".

> *"I had this very same Idea 6 months before the paper being uploaded to arxiv."* — Project Author

## 🏗 Architecture

The system follows a tri-cameral mind architecture:

```mermaid
graph TD
    User((User)) -->|Interacts| UI[Visual Cortex (Frontend)]
    UI -->|JSON/REST| API[Cognitive Router (Backend)]
    
    subgraph "NeuroLisp Engine"
        API -->|Prompts| LLM[Intuitive Processor (Genkit/Gemma)]
        API <-->|S-Expressions| Lisp[Logical Kernel (SBCL Process)]
        
        LLM -.->|Generates Code| Lisp
        Lisp -.->|Returns Truth| LLM
    end
    
    Lisp -->|Live Stream| REPL[REPL Monitor]
    REPL --> UI
```

## ✨ Features

*   **Live Neuro-Symbolic REPL**: Watch the LLM think in code. Real-time streaming of Lisp evaluation (stdout/stderr) directly to the UI.
*   **Persistent Memory**: Concepts are stored in a Lisp Hash Table (`*memoria*`), not just in context window tokens.
*   **Multimodal Ingestion**: Drag-and-drop PDFs and Images; the system extracts knowledge and crystalizes it into symbols.
*   **Self-Correcting Bootstrap**: The `bootstrap.lisp` kernel teaches the LLM how to use its own memory tools.

## 🚀 Getting Started

### Prerequisites
*   Node.js 18+ & pnpm
*   SBCL (Steel Bank Common Lisp) installed (`brew install sbcl`)
*   Ollama (running `gemma3:4b` or acceptable equivalent)

### Installation

1.  **Clone the Repository**
    ```bash
    git clone https://github.com/your-username/neurolisp.git
    cd neurolisp
    ```

2.  **Install Dependencies**
    ```bash
    # Backend
    cd backend
    pnpm install
    
    # Frontend
    cd ../frontend
    pnpm install
    ```

3.  **Ignition**
    ```bash
    # Terminal 1: Cognitive Backend
    cd backend
    pnpm dev
    
    # Terminal 2: Visual Cortex
    cd frontend
    pnpm dev
    ```

---

# NeuroLisp: O Ambiente de Metaprogramação Cognitiva

**NeuroLisp** é uma arquitetura neuro-simbólica de ponta que funde a intuição semântica dos Grandes Modelos de Linguagem (LLMs) com o rigor lógico do Common Lisp. Ao embutir um Runtime Lisp persistente (SBCL) diretamente no loop cognitivo da IA, habilitamos a verdadeira **Metacognição**: a capacidade do Modelo de escrever, executar e refinar suas próprias ferramentas de raciocínio em tempo real.

## 🧠 O Conceito

Interações tradicionais com LLMs são efêmeras: o modelo "pensa" apenas enquanto gera tokens. O **NeuroLisp** muda este paradigma dando ao LLM uma **Memória Simbólica** persistente.

Inspirado pelo artigo revolucionário **"From Tool Calling to Symbolic Thinking: LLMs in a Persistent Lisp Metaprogramming Loop" (de la Torre et al., 2025)**, este projeto implementa um *Loop Reflexivo* onde:
1.  **Intuição**: O LLM (Gemma 3) analisa dados não estruturados (PDFs, Imagens, Chat).
2.  **Tradução**: Ele converte insights em S-expressions simbólicas `(lembrar 'conceito "definição")`.
3.  **Raciocínio**: Um processo SBCL vivo interpreta esses símbolos, forçando lógica e consistência.
4.  **Persistência**: O estado permanece vivo durante toda a conversa — um verdadeiro "Fluxo de Consciência".

> *"Tive essa exata ideia 6 meses antes do paper ser enviado ao arXiv."* — Autor do Projeto

## 🏗 Arquitetura

O sistema segue uma arquitetura tricameral:

```mermaid
graph TD
    User((Usuario)) -->|Interage| UI[Cortex Visual (Frontend)]
    UI -->|JSON/REST| API[Roteador Cognitivo (Backend)]
    
    subgraph "Motor NeuroLisp"
        API -->|Prompts| LLM[Processador Intuitivo (Genkit/Gemma)]
        API <-->|S-Expressions| Lisp[Nucleo Logico (Processo SBCL)]
        
        LLM -.->|Gera Codigo| Lisp
        Lisp -.->|Retorna Verdade| LLM
    end
    
    Lisp -->|Stream Vivo| REPL[Monitor REPL]
    REPL --> UI
```

## ✨ Funcionalidades

*   **REPL Neuro-Simbólico Vivo**: Assista o LLM pensar em código. Streaming em tempo real da avaliação Lisp (stdout/stderr) diretamente para a UI.
*   **Memória Persistente**: Conceitos são armazenados em uma Hash Table Lisp (`*memoria*`), não apenas em tokens da janela de contexto.
*   **Ingestão Multimodal**: Arraste e solte PDFs e Imagens; o sistema extrai conhecimento e o cristaliza em símbolos.
*   **Bootstrap Auto-Corretivo**: O kernel `bootstrap.lisp` ensina o LLM como usar suas próprias ferramentas de memória.
