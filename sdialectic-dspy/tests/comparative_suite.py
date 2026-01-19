import os
import sys
import json
import dspy
import random
import time
import subprocess

# Add root to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from dspy_modules.signatures import NarrativeToLogic
from core.config import settings
from core.kernel.sbcl_bridge import SBCLKernel

RESULTS_FILE = os.path.join(os.path.dirname(__file__), 'comparative_results.json')
REPORT_FILE = os.path.join(os.path.dirname(__file__), 'comparative_report.md')
FOLIO_PATH = os.path.join(os.path.dirname(__file__), '../datasets/folio_processed.json')
STATIC_CASES_PATH = os.path.join(os.path.dirname(__file__), 'test_cases.json')

# Hallucination Artifacts from FOLIO
ARTIFACTS = ["JAMES", "COCKS", "WHIG", "HEPTALOGY", "VIOLINIST", "CONCERT", "IS-A MAN-JAMES"]



from optimization.metrics import check_hallucinations # Reuse the robust logic

# Hallucination Artifacts from FOLIO (Legacy check, kept for comparison)
ARTIFACTS = ["JAMES", "COCKS", "WHIG", "HEPTALOGY", "VIOLINIST", "CONCERT", "IS-A MAN-JAMES"]

def load_unseen_folio(n=10):
    """Loads N examples from FOLIO dataset, skipping the first 30 (used in training)."""
    if not os.path.exists(FOLIO_PATH):
        print("[WARN] FOLIO dataset not found.")
        return []
        
    with open(FOLIO_PATH, 'r') as f:
        data = json.load(f)
        
    # Skip first 100 just to be safe
    unseen = data[100:]
    sample = random.sample(unseen, min(len(unseen), n))
    
    return [{"category": "Unseen FOLIO", "text": item['input']} for item in sample]

def check_hallucination_legacy(lisp_code):
    """Returns True if any training artifact is found in the output."""
    if not lisp_code: return False
    upper_code = lisp_code.upper()
    for artifact in ARTIFACTS:
        if artifact in upper_code:
            return True
    return False

def run_suite():
    print("------------------------------------------------")
    print(" S-DIALECTIC: COMPARATIVE TEST SUITE            ")
    print("------------------------------------------------")
    
    # 1. Setup Models
    try:
        student_lm = dspy.LM(model=settings.STUDENT_MODEL_NAME, api_base=settings.OLLAMA_HOST, stop=["\n\n"], max_tokens=500)
        dspy.settings.configure(lm=student_lm)
    except Exception as e:
        print(f"[FATAL] Connection to Ollama failed: {e}")
        return

    # 2. Setup Kernel
    try:
        kernel = SBCLKernel()
    except FileNotFoundError:
        print("[FATAL] SBCL not found.")
        return

    # 3. Load Test Data
    try:
        with open(STATIC_CASES_PATH, 'r') as f:
            cases = json.load(f)
    except Exception as e:
        print(f"[ERROR] Could not load test cases: {e}")
        cases = []
        
    folio_cases = load_unseen_folio(10)
    cases.extend(folio_cases)
    
    print(f"[INFO] Loaded {len(cases)} test cases.")
    
    # 4. Define Strategies
    strategies = {
        "Zero-Shot": None,
        "Optimized (Folio)": os.path.join(os.path.dirname(__file__), '../dspy_modules/optimized_narrative_folio.json'),
        "Optimized (BioRED)": os.path.join(os.path.dirname(__file__), '../dspy_modules/optimized_narrative_biored.json')
    }
    
    results = {k: [] for k in strategies.keys()}
    
    # 5. Execution Loop
    extractor = dspy.ChainOfThought(NarrativeToLogic)
    
    for strategy_name, weight_path in strategies.items():
        print(f"\n>> Running Strategy: {strategy_name}")
        
        # Load Weights
        if weight_path and os.path.exists(weight_path):
            try:
                extractor.load(weight_path)
                print(f"   [Loaded] {os.path.basename(weight_path)}")
            except Exception as e:
                print(f"   [Error] Loading weights: {e}")
        else:
            print("   [Mode] Zero-Shot (No weights loaded)")
            if strategy_name == "Zero-Shot":
                extractor = dspy.ChainOfThought(NarrativeToLogic)

        for i, case in enumerate(cases):
            print(f"   Processing Case {i+1}/{len(cases)} ({case['category']})... ", end="", flush=True)
            
            try:
                # Inference
                start_time = time.time()
                pred = extractor(document_chunk=case['text'])
                elapsed = time.time() - start_time
                
                lisp = pred.lisp_code
                
                # Validation
                valid_syntax = False
                if lisp:
                    valid_syntax = (lisp.count("(") == lisp.count(")")) and lisp.strip() != ""
                
                valid_kernel = False
                if valid_syntax:
                     valid_kernel = kernel.validate_expression(lisp)
                
                # Hallucination Checks
                # 1. Legacy (Folio specific artifacts)
                is_legacy_hallucinated = check_hallucination_legacy(lisp)
                # 2. General (Atom check against input)
                is_atom_hallucinated, _ = check_hallucinations(lisp, case['text'])
                
                hallucinated = is_legacy_hallucinated or is_atom_hallucinated
                
                # Store
                results[strategy_name].append({
                    "category": case['category'],
                    "input": case['text'],
                    "output": lisp,
                    "valid_syntax": valid_syntax,
                    "valid_kernel": valid_kernel,
                    "hallucination_found": hallucinated,
                    "hallucination_details": {
                        "legacy": is_legacy_hallucinated,
                        "atom": is_atom_hallucinated
                    },
                    "latency": elapsed
                })
                
                print("DONE.")
                
            except Exception as e:
                print(f"FAIL: {e}")
        
        # Save after each strategy to be safe
        with open(RESULTS_FILE, 'w') as f:
            json.dump(results, f, indent=2) 

    print(f"\n[INFO] Results saved to {RESULTS_FILE}")
    
    # 7. Generate Charts
    try:
        subprocess.run(["python", "tests/visualize_results.py", RESULTS_FILE], check=True)
    except Exception as e:
        print(f"[WARN] Failed to generate charts: {e}")

    # 8. Generate Markdown Report
    generate_markdown_report(results)
    
    # Cleanup
    kernel.shutdown()

def generate_markdown_report(results):
    with open(REPORT_FILE, 'w') as f:
        f.write("# Comparative Analysis Report\n\n")
        f.write("## Overview\n")
        f.write("Comparison of Zero-Shot vs Optimized Models on Logic Generation.\n\n")
        
        f.write("## Visualizations\n")
        f.write("![Validity Chart](validity_chart.png)\n\n")
        f.write("![Hallucination Chart](hallucination_chart.png)\n\n")
        
        f.write("## Detailed Trace\n")
        
        for strategy, cases in results.items():
            f.write(f"### Strategy: {strategy}\n")
            f.write("| Category | Input | Output | Valid? | Hallucination? |\n")
            f.write("| :--- | :--- | :--- | :--- | :--- |\n")
            
            for c in cases:
                short_input = (c['input'][:30] + '..') if len(c['input']) > 30 else c['input']
                # Escape pipe in lisp
                out_str = c['output'] if c['output'] else "NULL"
                code = f"`{out_str}`".replace("|", "\\|").replace("\n", "")
                valid_mark = "✅" if c['valid_kernel'] else "❌"
                
                # Granular Hallucination Icons
                details = c.get('hallucination_details', {})
                hal_mark = ""
                if details.get('legacy'):
                    hal_mark += "🤡 " # Clown for "James"
                if details.get('atom'):
                    hal_mark += "👻 " # Ghost for "Invented Atoms"
                if c['hallucination_found'] and not hal_mark:
                    hal_mark = "⚠️" # Fallback
                
                f.write(f"| {c['category']} | {short_input} | {code} | {valid_mark} | {hal_mark} |\n")
            f.write("\n")
            
    print(f"[INFO] Report generated at {REPORT_FILE}")

if __name__ == "__main__":
    run_suite()
