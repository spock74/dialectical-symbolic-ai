import dspy

# Gold Standard Examples for NarrativeToLogic
# These examples guide the Teacher model (Gemini) to teach the Student model (Phi4).
# They act as the "Ground Truth" for the logic style we want.

training_data = [
    # 1. Simple Classification
    dspy.Example(
        document_chunk="Socrates is a man.",
        lisp_code="((IS-A SOCRATES MAN))"
    ).with_inputs("document_chunk"),

    # 2. Implication / Rule
    dspy.Example(
        document_chunk="All men are mortal.",
        lisp_code="((IMPLIES (IS-A ?X MAN) (MORTAL ?X)))"
    ).with_inputs("document_chunk"),

    # 3. Two Facts
    dspy.Example(
        document_chunk="Aristotle is a philosopher. Plato is a philosopher.",
        lisp_code="((IS-A ARISTOTLE PHILOSOPHER) (IS-A PLATO PHILOSOPHER))"
    ).with_inputs("document_chunk"),

    # 4. Set Definition (Implicit)
    dspy.Example(
        document_chunk="Let X1 be the set of all things that are mortal.",
        lisp_code="((DEFINE-SET X1 (LAMBDA (?X) (MORTAL ?X))))"
    ).with_inputs("document_chunk"),

    # 5. Relationship
    dspy.Example(
        document_chunk="Paris is the capital of France.",
        lisp_code="((CAPITAL-OF PARIS FRANCE))"
    ).with_inputs("document_chunk"),

    # 6. Negation
    dspy.Example(
        document_chunk="Zeus is not mortal.",
        lisp_code="((NOT (MORTAL ZEUS)))"
    ).with_inputs("document_chunk"),

    # 7. Causality
    dspy.Example(
        document_chunk="Rain causes the ground to be wet.",
        lisp_code="((CAUSES RAIN (WET GROUND)))"
    ).with_inputs("document_chunk"),
    
    # 8. Complex Set Intersection
    dspy.Example(
        document_chunk="Find the intersection of X1 and X2.",
        lisp_code="((INTERSECTION X1 X2))"
    ).with_inputs("document_chunk"),

    # 9. Temporal
    dspy.Example(
        document_chunk="Before 1900, flight was impossible.",
        lisp_code="((TIME-CONSTRAINT (BEFORE 1900) (NOT (POSSIBLE FLIGHT))))"
    ).with_inputs("document_chunk"),
    
    # 10. Distinct Entities
    dspy.Example(
        document_chunk="Bitcoin is different from Ethereum.",
        lisp_code="((DISTINCT BITCOIN ETHEREUM))"
    ).with_inputs("document_chunk")
]

# Validation Set (Unseen during training, used for evaluation)
validation_data = [
    dspy.Example(
        document_chunk="Socrates is mortal.",
        lisp_code="((MORTAL SOCRATES))"
    ).with_inputs("document_chunk"),
    
    dspy.Example(
        document_chunk="If it rains, it pours.",
        lisp_code="((IMPLIES (RAIN) (POUR)))"
    ).with_inputs("document_chunk")
]
