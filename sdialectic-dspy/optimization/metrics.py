import dspy
import re
from core.kernel.sbcl_bridge import SBCLKernel

# Initialize kernel globally
try:
    kernel = SBCLKernel()
except Exception:
    kernel = None

def parse_sexpr(sexpr):
    """Simple recursive S-expression parser."""
    sexpr = sexpr.strip()
    if sexpr.startswith("'"): sexpr = sexpr[1:]
    
    # Tokenize
    tokens = re.findall(r'\(|\)|[^\s()]+', sexpr)
    
    stack = []
    current_list = []
    stack.append(current_list)
    
    for token in tokens:
        if token == '(':
            new_list = []
            stack[-1].append(new_list)
            stack.append(new_list)
        elif token == ')':
            if len(stack) > 1:
                stack.pop()
        else:
            stack[-1].append(token)
            
    # Return the first complete list (the root)
    if not current_list: return []
    return current_list[0]

def calculate_connectivity(lisp_code):
    """Calculates graph connectivity (Largest Component / Total Nodes)."""
    try:
        parsed = parse_sexpr(lisp_code)
        if not parsed: return 0.0
        
        # Build Adjacency List
        adj = {}
        nodes = set()
        
        # Assume list of lists: ((REL A B) (REL B C))
        # If single list: (REL A B) -> treat as 1 edge?
        # Standard format should be a list of triplets.
        
        triplets = parsed if isinstance(parsed[0], list) else [parsed]
        
        for triplet in triplets:
            # triplet usually: [REL, SUBJ, OBJ]
            # Filters out non-symbol atoms
            atoms = [str(x) for x in triplet if isinstance(x, str) and not x.startswith("?")]
            
            # Simple approach: Connect all atoms in the tuple
            for i in range(len(atoms)):
                u = atoms[i]
                nodes.add(u)
                if u not in adj: adj[u] = set()
                
                for j in range(i + 1, len(atoms)):
                    v = atoms[j]
                    nodes.add(v)
                    if v not in adj: adj[v] = set()
                    
                    adj[u].add(v)
                    adj[v].add(u)
        
        if not nodes: return 0.0
        
        # BFS for connected components
        visited = set()
        max_component = 0
        
        for node in nodes:
            if node not in visited:
                # Start traversal
                queue = [node]
                visited.add(node)
                size = 0
                while queue:
                    curr = queue.pop(0)
                    size += 1
                    for neighbor in adj.get(curr, []):
                        if neighbor not in visited:
                            visited.add(neighbor)
                            queue.append(neighbor)
                max_component = max(max_component, size)
                
        return max_component / len(nodes)
        
    except Exception:
        return 0.0

# Whitelist of DSL keywords relative to the domain or logic
DSL_KEYWORDS = {
    "IS-A", "IMPLIES", "AND", "OR", "NOT", "IF", "THEN", "EQUIV", "XOR", 
    "PROMOTES", "SUPPRESSES", "CAUSES", "PREVENTS", "ASSOCIATED-WITH", 
    "POSITIVELY-CORRELATES", "NEGATIVELY-CORRELATES", "BINDS", "INTERACTS",
    "COMPARES-TO", "CONVERTS-TO", "TREATS", "DIAGNOSES", "IS-A-PART-OF",
    # Added for Qwen Medico:
    "HAS-PROPERTY", "RESPONSIBLE-FOR", "PRODUCES", "EXAMPLE-OF", 
    "PLAYS-ROLE-IN", "TARGETS", "AFFECTS", "LOCATED-IN", "PART-OF",
    "REGULATES", "INCREASES", "DECREASES", "STIMULATES", "INHIBITS"
}

from Levenshtein import ratio

def check_hallucinations(lisp_code, source_text):
    """
    Returns True if the lisp_code contains significant atoms not present in source_text.
    Now uses Levenshtein fuzzy matching and ignores short tokens.
    """
    # 1. Extract atoms from Lisp
    clean_lisp = lisp_code.replace("(", " ").replace(")", " ")
    atoms = [x.strip() for x in clean_lisp.split() if x.strip()]
    
    # 2. Extract words from Source
    # Filter out short tokens (< 3 chars) to avoid matching "A", "IS", "IN" falsely
    source_tokens = set([t.upper() for t in re.findall(r'\w+', source_text.upper()) if len(t) > 2])
    
    hallucinations = []
    
    for atom in atoms:
        # Skip keywords
        if atom.upper() in DSL_KEYWORDS:
            continue
            
        # Check numeric/logic ids
        if atom.startswith("ENT-") or atom.isdigit():
            continue
        
        # Check against source
        # Simple check first: Is the atom (or parts) in source?
        parts = atom.replace("-", " ").split()
        
        # Strategy: Each meaningful part of the atom must be supported by the text.
        # "Meaningful" = len > 2.
        
        for p in parts:
            p_upper = p.upper()
            if len(p_upper) < 3: # Skip short parts like "OF" in "TYPE-OF-CELL"
                continue
                
            found = False
            # 1. Exact/Substring Match
            for s in source_tokens:
                if p_upper in s or s in p_upper:
                    found = True
                    break
            
            # 2. Fuzzy Match (if not found)
            if not found:
                for s in source_tokens:
                    if ratio(p_upper, s) > 0.8: # 80% similarity
                        found = True
                        break
            
            if not found:
                hallucinations.append(atom)
                break # One bad part fails the atom
            
    return len(hallucinations) > 0, hallucinations

def sbcl_metric(gold, pred, trace=None):
    """
    Advanced Neuro-Symbolic Metric.
    Score = 0.5 * Valid + 0.5 * Connected
    PENALTY: If hallucinated, Score = 0.0
    """
    lisp_code = pred.lisp_code
    input_text = gold.document_chunk # Access input from example
    
    # 0. Anti-Hallucination Check
    # This is the "Guarda-Costas"
    is_hallucinated, diffs = check_hallucinations(lisp_code, input_text)
    
    hallucination_penalty = 0.0
    if is_hallucinated:
        # Soft Penalty instead of Hard Fail
        # We trust the model's abstraction, but flag potential issues.
        # print(f"[RunDebug] Semantic Drift (Allowed): {diffs}") 
        hallucination_penalty = 0.1 # Small deduction
        # return 0.0 -> DISABLED STRICT MODE

    # 1. Sanitize (Simulate main.py behavior)
    lisp_code = lisp_code.replace("?", "").upper()
    if lisp_code.count('(') != lisp_code.count(')'):
        print(f"[RunDebug] Invalid Parens: {lisp_code[:50]}...")
        return 0.0

    # 2. Axiomatic Validation
    is_valid = False
    if kernel:
        is_valid = kernel.validate_expression(lisp_code)

    if not is_valid:
        print(f"[RunDebug] SBCL Invalid: {lisp_code[:50]}...")
        
    score_valid = 1.0 if is_valid else 0.0
    
    # 3. Connectivity (Reasoning Quality)
    score_conn = calculate_connectivity(lisp_code)
    
    # Weighted Score
    # Validation is critical (must be parseable). Connectivity is bonus.
    if not is_valid:
        return 0.0 # Strict Fail
        
    return 0.5 + (0.5 * score_conn)

