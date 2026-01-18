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

def sbcl_metric(gold, pred, trace=None):
    """
    Advanced Neuro-Symbolic Metric.
    Score = 0.5 * Valid + 0.5 * Connected
    """
    lisp_code = pred.lisp_code
    
    # 0. Sanitize (Simulate main.py behavior)
    lisp_code = lisp_code.replace("?", "").upper()
    if lisp_code.count('(') != lisp_code.count(')'):
        # Naive verify
        return 0.0

    # 1. Axiomatic Validation
    is_valid = False
    if kernel:
        is_valid = kernel.validate_expression(lisp_code)
        
    score_valid = 1.0 if is_valid else 0.0
    
    # 2. Connectivity (Reasoning Quality)
    score_conn = calculate_connectivity(lisp_code)
    
    # Weighted Score
    # Validation is critical (must be parseable). Connectivity is bonus.
    if not is_valid:
        return 0.0 # Strict Fail
        
    return 0.5 + (0.5 * score_conn)
