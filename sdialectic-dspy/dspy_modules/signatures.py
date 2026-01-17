import dspy

class NarrativeToLogic(dspy.Signature):
    """
    Transforma texto narrativo em S-Expressions Lisp estritas.
    Deve extrair fatos atômicos e remover 'fluff'.
    """
    
    document_chunk = dspy.InputField(desc="Trecho de texto bruto de um PDF ou Paper.")
    
    logic_reasoning = dspy.OutputField(desc="Chain of Thought explicando a extração.")
    lisp_code = dspy.OutputField(desc="Lista de S-Expressions. Ex: ((CAUSES COFFEE CANCER) (IMPLIES RAIN WET))")

class DialecticSynthesis(dspy.Signature):
    """
    Resolve conflitos entre dois fatos contraditórios gerando uma regra condicional.
    """
    fact_a = dspy.InputField()
    fact_b = dspy.InputField()
    synthesis = dspy.OutputField(desc="Regra Lisp usando IF/THEN para contextualizar a contradição.")
