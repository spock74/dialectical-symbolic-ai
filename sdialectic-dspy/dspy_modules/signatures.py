import dspy

class NarrativeToLogic(dspy.Signature):
    """
    CONVERTS narrative text into SYMBOLIC LOGIC (Lisp S-Expressions).
    
    GOAL: Atomize knowledge into clear Subject-Predicate-Object triplets.
    FORMAT: A list of lists. E.g., ((CAUSES SMOKING CANCER) (IS-A CAT MAMMAL)).
    CONSTRAINTS:
    - Use UPPERCASE symbols for atoms.
    - Avoid natural language fluff inside the atoms.
    - Output MUST be parseable by Common Lisp (SBCL).
    """
    
    document_chunk = dspy.InputField(desc="Raw text segment containing knowledge assertions.")
    
    logic_reasoning = dspy.OutputField(desc="Chain of Thought: Identify entities, relationships, and causal links.")
    lisp_code = dspy.OutputField(desc="Valid Lisp S-Expressions. Example: ((RELATION ENTITY-A ENTITY-B) ...)")

class DialecticSynthesis(dspy.Signature):
    """
    Resolve conflitos entre dois fatos contraditórios gerando uma regra condicional.
    """
    fact_a = dspy.InputField()
    fact_b = dspy.InputField()
    synthesis = dspy.OutputField(desc="Regra Lisp usando IF/THEN para contextualizar a contradição.")
