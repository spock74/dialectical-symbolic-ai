# SDialectic: Neuro-Symbolic Cognitive Architecture

SDialectic is an advanced neuro-symbolic framework designed for high-precision knowledge extraction and logical reasoning. It implements a Dual-Process architecture that bridges the creative flexibility of Large Language Models (LLMs) with the formal rigor of a Symbolic Logic Kernel.

## 1. Executive Summary: The Dual-Process Theory

The system is built upon the Dual-Process theory of cognition, distinguishing between two primary modes of processing:

1.  **System 1 (Neural Intuition)**: Handled by Generation 3 LLMs (e.g., Gemma 3, Qwen 2.5). This layer manages natural language understanding, context sensing, and creative synthesis.
2.  **System 2 (Symbolic Logic)**: Driven by the TS-Kernel (Local JSCL). This layer provides an immutable environment for formal logic, atomic fact storage, and deterministic inference.

The interaction between these layers is managed by the **Reflective Orchestrator**, ensuring that every neural intuition is grounded in symbolic truth.

---

## 2. System Architecture

The architecture is composed of three primary layers: the User Interface, the Orchestration Layer, and the Symbolic Kernel.

### 2.1 Concept: The Digital Brain (Didactic View)

```mermaid
graph TD
    subgraph "The Digital Brain"
        subgraph "Left Hemisphere: Logic"
            SK["Symbolic Kernel (Lisp)<br/>Atomic Facts<br/>Formal Inference"]
        end
        subgraph "Right Hemisphere: Intuition"
            LLM["LLM Layer<br/>Language Synthesis<br/>Contextual Fluidity"]
        end
        ORCH["Reflective Orchestrator<br/>(The Bridge)"]
    end
    
    UserRequest([User Query]) --> ORCH
    ORCH <--> LLM
    ORCH <--> SK
    ORCH --> FinalResponse([Rational Response])
```

### 2.2 Technical Topology

```mermaid
graph TB
    subgraph "Frontend: React Application"
        UI["UI Layer"]
        ZST["Zustand State (Modular Slices)"]
        RF["ReactFlow Analysis Engine"]
    end

    subgraph "Backend: Node.js / Genkit"
        API["Express API Server"]
        RO["Reflective Orchestrator Class"]
        FLOW["Genkit Knowledge Flow"]
    end

    subgraph "Symbolic Layer: TS-Kernel"
        AD["JSCL Adapter (Bridge)"]
        ENG["JSCL Lisp Engine"]
        KG["Knowledge Graph (Typedef)"]
    end

    subgraph "Neural Layer: Ollama"
        LOGIC["Logic Model (Qwen 2.5 Coder)"]
        CHAT["Synthesis Model (Gemma 3)"]
    end

    %% Interactions
    UI <--> ZST
    UI <--> API
    API <--> FLOW
    FLOW <--> RO
    RO <--> AD
    AD <--> ENG
    ENG <--> KG
    RO <--> LOGIC
    RO <--> CHAT
```

---

## 3. Core Component: The Reflective Loop

The Reflective Loop is a multi-turn cognitive process where the AI "reasons" by writing and executing symbolic code before presenting a result.

### 3.1 Interaction Sequence

```mermaid
sequenceDiagram
    autonumber
    participant U as User
    participant ORCH as Orchestrator
    participant LM as Logic Model (Qwen)
    participant SK as Symbolic Kernel (JSCL)
    participant SM as Synthesis Model (Gemma)

    U->>ORCH: Input Query
    
    Note over ORCH, SK: Stage 1: Contextual Grounding
    ORCH->>SK: Query current Knowledge Graph
    SK-->>ORCH: Return relevant Entities/Relations

    Note over ORCH, SK: Stage 2: Logical Reasoning (Loop)
    loop Thinking Turn
        ORCH->>LM: Request Logic Plan (based on Context)
        LM-->>ORCH: Symbolic Code (S-Expressions)
        ORCH->>SK: Execute S-Expressions
        SK->>SK: Perform Inference / Graph Mutation
        SK-->>ORCH: Execution Result / Observations
    end

    Note over ORCH, SM: Stage 3: Human Synthesis
    ORCH->>SM: Prompt with Fact Package + Reasoning Trace
    SM-->>ORCH: Natural Language Response
    ORCH-->>U: Final Verified Response
```

---

## 4. Cognitive Specialization and Context Engineering

The system optimizes for accuracy by treating the Logic Model and the Synthesis Model as two specialized agents with distinct context windows.

### 4.1 Model Roles and Context Inputs

| Feature | Logic Model (Qwen 2.5 Coder) | Synthesis Model (Gemma 3) |
| :--- | :--- | :--- |
| **Primary Goal** | Translate natural language into formal logic. | Translate formal facts into natural language. |
| **Primary Context** | User Prompt + Current Graph Snapshot. | User Prompt + Fact Package + Reasoning Trace. |
| **Output Type** | Executable Lisp (S-Expressions). | Formatted Markdown / Natural Language. |
| **Tone** | Deterministic / Syntactic. | Explanatory / Grounded. |

### 4.2 The Context Transformation Pipeline

The diagram below illustrates how context is filtered and transformed as it moves from the unstructured user input to the final grounded response.

```mermaid
graph LR
    subgraph "Input Processing"
        UP([User Prompt]) --> CS[Context Sensing]
    end

    subgraph "Phase 1: Logic Context"
        CS --> |"Filter Graph"| GS[Graph Snapshot]
        UP --> LM[Logic Model<br/>Qwen 2.5 Coder]
        GS --> LM
    end

    subgraph "Phase 2: Result Transformation"
        LM --> |"Evaluate"| SK[Symbolic Kernel]
        SK --> |"Validate Facts"| FP[Fact Package]
        SK --> |"Log Operations"| RT[Reasoning Trace]
    end

    subgraph "Phase 3: Synthesis Context"
        UP --> SM[Synthesis Model<br/>Gemma 3]
        FP --> SM
        RT --> SM
        SM --> FR([Final Response])
    end

    style FP fill:#eff6ff,stroke:#2563eb,stroke-width:2px
    style RT fill:#f0fdf4,stroke:#16a34a,stroke-width:2px
```

### 4.3 Context Injection Mechanism

1.  **Logic Injection**: The `ReflectiveOrchestrator` performs a "proximity search" on the Knowledge Graph before invoking the Logic Model. This ensures the prompt only contains entities relevant to the current conversation, preventing context overflow.
2.  **Fact Injection**: The Synthesis Model is strictly instructed to prioritize the `Fact Package`. If the Symbolic Kernel returns a "Relationship added" or "Conflict found" result, the Synthesis Model must use this as its "Ground Truth," overriding any internal neural priors.

### 4.4 Knowledge Ingestion Lifecycle

When a user introduces new raw data (specifically Markdown or Text files) via the Source Manager, the system initiates an automated extraction lifecycle.

```mermaid
sequenceDiagram
    autonumber
    participant UI as Source Manager (UI)
    participant ST as Zustand Store
    participant BE as Backend (API)
    participant RO as Reflective Orchestrator
    participant SK as Symbolic Kernel

    UI->>UI: Select .md File
    UI->>UI: Read content locally (FileReader)
    UI->>ST: addMessage("[Archive loaded: filename]")
    
    Note over UI, BE: Automated Extraction Trigger
    UI->>BE: POST /api/chat { prompt: "Analyze and extract...", source: "filename" }
    
    BE->>RO: think()
    RO->>SK: Execute extracted Lisp triples
    SK->>SK: Mutate Persistent Graph
    
    BE->>ST: addSourceToActiveGroup(newMetadata)
    BE-->>UI: Display Extraction Success
    
    Note right of SK: Data is now part of the<br/>permanent Knowledge Base
```

**Key Ingestion Phases:**
1.  **Detection & Pre-processing**: The frontend identifies the file type and wraps the content in a "Structured Extraction Prompt."
2.  **Symbolic Injection**: The Logic Model analyzes the raw content and translates it into atomic `(add-node)` and `(add-relation)` commands.
3.  **Source-Graph Association**: Every ingested file is treated as a distinct "Source" but contributes to the unified Knowledge Graph of the active Group (Study Trail).
4.  **Persistent Mapping**: Metadata (Hash, Name, Date) is saved in the Zustand store, while the factual content is persisted as Lisp code in the backend data layer.

---

## 5. Key Features

### 4.1 Advanced Graph Analysis
The system includes a sophisticated visualization suite for interacting with the Knowledge Graph in real-time.
- **Dynamic Filtering**: Toggle visibility of Entities and Relations independently.
- **Predicate Filtering**: Select specific relation types (e.g., "is valid in", "causes") to isolate sub-graphs.
- **Flexible Layout**: Switch between Vertical (Top-Bottom) and Horizontal (Left-Right) views with automatic centering (Fit View).
- **High-Resolution Zoom**: Optimized for massive graphs with zoom-out capabilities up to 20x.

### 4.2 Symbolic Live Stream (Live REPL)
Every logical operation performed by the AI is streamed to the UI in real-time. This provides a "Glass Box" experience where users can monitor:
- Lisp commands being generated.
- Internal graph mutations.
- Reasoning traces and formal inference logs.

### 4.3 Resilience and Data Integrity
- **Stateless Snapshots**: The Knowledge Graph is persisted as executable Lisp code, ensuring state can be reconstructed perfectly on boot.
- **Rollback Mechanism**: If an interaction is aborted, the system automatically restores the last consistent state.
- **Sandboxed Execution**: The JSCL engine prevents neural errors from compromising system stability.

---

## 5. Technical Implementation Details

### 5.1 Symbolic Primitives
The kernel provides a set of high-level S-Expressions for the AI:
- `(add-node id type)`: Injects a unique concept into the graph.
- `(add-relation subject predicate object)`: Creates a formal triple.
- `(infer)`: Triggers the Forward Chaining engine to derive implicit facts.
- `(get-graph-snapshot)`: Telemetry for the ReactFlow engine.

### 5.2 State Management
The frontend employs a modular Zustand architecture divided into specialized slices:
- **ConfigSlice**: Governance of system flags (Memory, Bypass).
- **ChatSlice**: Thread-safe message history.
- **SourceSlice**: Document and knowledge unit management.
- **GraphSlice**: Persistent UI settings (Layout direction, filter states).

---

## 6. Technical Stack

- **Orchestration**: Node.js, Genkit, TypeScript.
- **Cognitive Layer**: Ollama (Gemma 3, Qwen 2.5 Coder).
- **Symbolic Layer**: JSCL (adapted for TS/Node side-car).
- **Frontend**: React 19, ReactFlow, Zustand, TailwindCSS, Radix UI.

---

*Copyright © 2025 SDialectic Labs. Technical Documentation Internal Release.*
