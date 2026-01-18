import requests
import json
import os

FOLIO_URL = "https://raw.githubusercontent.com/Yale-LILY/FOLIO/main/data/v0.0/folio-train.jsonl"
OUTPUT_FILE = "datasets/folio_processed.json"

def ingest_folio():
    print(f"[INFO] Downloading FOLIO dataset from {FOLIO_URL}...")
    try:
        response = requests.get(FOLIO_URL)
        response.raise_for_status()
    except Exception as e:
        print(f"[ERROR] Download failed: {e}")
        return

    raw_data = response.text.strip().split('\n')
    print(f"[INFO] Downloaded {len(raw_data)} lines.")

    processed_data = []
    
    for line in raw_data:
        try:
            entry = json.loads(line)
            premises = entry.get("premises", "")
            conclusion = entry.get("conclusion", "")
            
            # Combine premises into a single narrative block
            # FOLIO premises are usually a list of strings? Let's check format effectively.
            # Assuming list of strings based on research.
            # However, looking at research summary: "premises and their FOL annotations".
            # If it's a string, just use it. If list, join.
            
            # Defensive coding since we haven't inspected the file manually yet
            narrative = ""
            if isinstance(premises, list):
                narrative = " ".join(premises)
            elif isinstance(premises, str):
                narrative = premises
            
            full_text = f"{narrative} Therefore, {conclusion}"
            
            # We don't have the "Gold Lisp" in our dialect.
            # We treat this as "Unlabeled Data" (or semi-labeled) for the Optimizer.
            # The Optimizer (MIPROv2) can take unlabeled data if we use a Teacher.
            # Or we can provide a placeholder and let the Teacher generate the trace.
            
            processed_data.append({
                "input": full_text,
                "gold_logic": "", # Empty string indicates no ground truth Lisp available
                "reasoning": "Derived from FOLIO dataset.",
                "original_fol": entry.get("premises-FOL", "") # Keep for reference
            })
            
        except json.JSONDecodeError:
            print("[WARN] Skipped invalid JSON line")

    # Save
    os.makedirs(os.path.dirname(OUTPUT_FILE), exist_ok=True)
    with open(OUTPUT_FILE, "w") as f:
        json.dump(processed_data, f, indent=2)
        
    print(f"[SUCCESS] Saved {len(processed_data)} examples to {OUTPUT_FILE}")

if __name__ == "__main__":
    ingest_folio()
