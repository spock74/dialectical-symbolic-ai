import sys
import os
import re

# Add project root to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from optimization.metrics import check_hallucinations, DSL_KEYWORDS

# Example that failed in the logs:
# Hallucination: ['immune-response', 'protection', 'protecting-body', 'fighting-pathogens']
# We need to see the INPUT text to know why.

# Let's use a reconstructed example based on the log pattern or just a generic one to test the logic.
# If I can't see the exact input from the log, I'll make a strong guess or use the probe output I saw earlier.

# Probe Example 5:
# INPUT: Neurotransmitters are chemical messengers that transmit signals across a synapse.
# ACTUAL: 
# (is-a neurotransmitter chemical-messenger)
# (has-property neurotransmitter signal-transmission)
# (plays-role-in neurotransmitter synaptic-communication)

# Let's test this specific case.

INPUT_TEXT = "Neurotransmitters are chemical messengers that transmit signals across a synapse."
LISP_CODE = """
(is-a neurotransmitter chemical-messenger)
(has-property neurotransmitter signal-transmission)
(plays-role-in neurotransmitter synaptic-communication)
"""

print(f"--- DEBUGGING METRIC ---")
print(f"INPUT: {INPUT_TEXT}")
print(f"LISP: {LISP_CODE}")

print(f"\nDSL_KEYWORDS: {len(DSL_KEYWORDS)} keywords")
if "HAS-PROPERTY" in DSL_KEYWORDS:
    print("HAS-PROPERTY is in whitelist.")
else:
    print("HAS-PROPERTY is MISSING.")

is_hallucinated, diffs = check_hallucinations(LISP_CODE, INPUT_TEXT)

print(f"\nIs Hallucinated: {is_hallucinated}")
print(f"Diffs: {diffs}")

# Detailed trace of the logic inside check_hallucinations
print(f"\n--- TRACE ---")
clean_lisp = LISP_CODE.replace("(", " ").replace(")", " ")
atoms = [x.strip() for x in clean_lisp.split() if x.strip()]
print(f"Atoms found: {atoms}")

source_tokens = set(re.findall(r'\w+', INPUT_TEXT.upper()))
print(f"Source tokens: {source_tokens}")

for atom in atoms:
    if atom.upper() in DSL_KEYWORDS:
        print(f"Skipping keyword: {atom}")
        continue
        
    parts = atom.replace("-", " ").split()
    print(f"Checking atom '{atom}' -> Parts: {parts}")
    
    match_all = True
    for p in parts:
        found = False
        for s in source_tokens:
            if p.upper() in s or s in p.upper():
                found = True
                break
        if not found:
            print(f"  [FAIL] Part '{p}' not found in source.")
            match_all = False
        else:
            print(f"  [OK] Part '{p}' found.")
            
    if not match_all:
        print(f"  -> Atom '{atom}' flagged as hallucination.")
