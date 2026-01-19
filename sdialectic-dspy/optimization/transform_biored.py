import json
import os
import re
from datasets import load_dataset
import random

# Mapping BioRED relations to S-Dialectic Lisp Predicates
RELATION_MAP = {
    "Positive_Correlation": "POSITIVELY-CORRELATES",
    "Negative_Correlation": "NEGATIVELY-CORRELATES",
    "Association": "ASSOCIATED-WITH",
    "Bind": "BINDS",
    "Cotreatment": "CO-TREATMENT",
    "Drug_Interaction": "INTERACTS",
    "Comparison": "COMPARES-TO",
    "Conversion": "CONVERTS-TO"
}

def clean_entity(text):
    """Sanitizes entity names for Lisp atoms."""
    # Remove special chars, replace spaces with dashes, uppercase
    text = text.upper()
    text = re.sub(r'[^A-Z0-9\s\-]', '', text)
    text = re.sub(r'\s+', '-', text.strip())
    # Ensure it doesn't start with a number or dash
    if text and (text[0].isdigit() or text[0] == '-'):
        text = "ENT-" + text
    return text

import json
import os
import re
import random
import requests
import zipfile
import io

# Mapping BioRED relations to S-Dialectic Lisp Predicates
RELATION_MAP = {
    "Positive_Correlation": "POSITIVELY-CORRELATES",
    "Negative_Correlation": "NEGATIVELY-CORRELATES",
    "Association": "ASSOCIATED-WITH",
    "Bind": "BINDS",
    "Cotreatment": "CO-TREATMENT",
    "Drug_Interaction": "INTERACTS",
    "Comparison": "COMPARES-TO",
    "Conversion": "CONVERTS-TO"
}

def clean_entity(text):
    """Sanitizes entity names for Lisp atoms."""
    text = text.upper()
    text = re.sub(r'[^A-Z0-9\s\-]', '', text)
    text = re.sub(r'\s+', '-', text.strip())
    if text and (text[0].isdigit() or text[0] == '-'):
        text = "ENT-" + text
    return text

import json
import os
import re
import random
import requests
import zipfile
import io

# Mapping BioRED relations to S-Dialectic Lisp Predicates
RELATION_MAP = {
    "Positive_Correlation": "POSITIVELY-CORRELATES",
    "Negative_Correlation": "NEGATIVELY-CORRELATES",
    "Association": "ASSOCIATED-WITH",
    "Bind": "BINDS",
    "Cotreatment": "CO-TREATMENT",
    "Drug_Interaction": "INTERACTS",
    "Comparison": "COMPARES-TO",
    "Conversion": "CONVERTS-TO"
}

def clean_entity(text):
    """Sanitizes entity names for Lisp atoms."""
    text = text.upper()
    text = re.sub(r'[^A-Z0-9\s\-]', '', text)
    text = re.sub(r'\s+', '-', text.strip())
    # Remove leading non-alphanumeric
    text = re.sub(r'^[^A-Z0-9]+', '', text)
    if not text: return "UNKNOWN"
    if text[0].isdigit() or text[0] == '-':
        text = "ENT-" + text
    return text

def download_and_extract_biored():
    # Correct URL is ALL CAPS for the filename
    url = "https://ftp.ncbi.nlm.nih.gov/pub/lu/BioRED/BIORED.zip"
    print(f"[INFO] Downloading BioRED from {url}...")
    r = requests.get(url)
    if r.status_code != 200:
        raise Exception(f"Failed to download BioRED from {url}: {r.status_code}")
        
    print("[INFO] Extracting...")
    z = zipfile.ZipFile(io.BytesIO(r.content))
    
    # Look for the Train file (likely PubTator format)
    train_file = None
    for n in z.namelist():
        if "Train.PubTator" in n:
            train_file = n
            break
            
    if not train_file:
         # Fallback search
        for n in z.namelist():
            if "Train" in n:
                train_file = n
                break
                
    if not train_file:
        print(f"[ERROR] Contents: {z.namelist()}")
        raise Exception("No Train file found in BIORED.zip")
        
    print(f"[INFO] Found training file: {train_file}")
    with z.open(train_file) as f:
        content = f.read().decode('utf-8')
        return content

def parse_pubtator(content):
    """
    Parses PubTator format.
    Blocks separated by blank lines.
    PMID|t|Title
    PMID|a|Abstract
    PMID TAB Annotations...
    """
    print(f"[DEBUG] Raw content length: {len(content)}")
    content = content.replace('\r\n', '\n')
    blocks = content.strip().split('\n\n')
    print(f"[DEBUG] Found {len(blocks)} blocks using \\n\\n split")
    
    if len(blocks) < 2:
         # Try identifying blocks by abstract line?
         print("[DEBUG] First 500 chars:")
         print(content[:500])

    docs = []
    
    for block in blocks:
        lines = block.strip().split('\n')
        if not lines: continue
        
        pmid = lines[0].split('|')[0]
        text = ""
        entities = {} # text -> id?? No, PubTator relation lines use ID.
        # But PubTator entity lines: PMID TAB Start End Text Type ID
        # PubTator relation lines: PMID TAB Type ID1 ID2
        
        # We need to map ID to Text.
        # Wait, BioRED PubTator entity line: 
        # 12345	Gene	10	20	BRCA1	Generic	123
        # ID is the last column?
        
        rels = []
        
        for line in lines:
            parts = line.split('\t')
            if "|t|" in line:
                text += line.split('|t|')[1] + " "
            elif "|a|" in line:
                text += line.split('|a|')[1]
            elif len(parts) >= 6: 
                # Entity line?
                # PMID, Start, End, Text, Type, ID
                # BioRED might correspond to: PMID, Type, ID, Text?
                # Let's inspect standard PubTator:
                # PMID \t Start \t End \t Text \t Type \t ID
                if parts[1].isdigit(): # Start offset
                     e_text = parts[3]
                     e_id = parts[5] if len(parts) > 5 else None
                     if e_id:
                         # Handle list of IDs? "123,456"
                         for sub_id in e_id.split(','):
                             entities[sub_id] = e_text
            elif len(parts) == 4 or len(parts) == 5:
                # Relation line: PMID \t Type \t ID1 \t ID2 (\t Role?)
                r_type = parts[1]
                id1 = parts[2]
                id2 = parts[3]
                rels.append((r_type, id1, id2))
        
        if rels:
            docs.append({
                "text": text.strip(),
                "entities": entities,
                "relations": rels
            })
            
    return docs

def transform_biored(output_path, sample_size=100):
    try:
        raw_content = download_and_extract_biored()
        documents = parse_pubtator(raw_content)
    except Exception as e:
        print(f"[ERROR] Failed to process BioRED: {e}")
        import traceback
        traceback.print_exc()
        return

    print(f"[INFO] Parsed {len(documents)} documents. Selecting samples...")
    
    processed_examples = []
    random.shuffle(documents)
    
    for doc in documents:
        text = doc['text']
        entities = doc['entities']
        relations = doc['relations']
        
        lisp_facts = []
        
        for r_type, id1, id2 in relations:
            if r_type in RELATION_MAP:
                # Resolve IDs
                # BioRED IDs might be complex. exact match first.
                ent1_text = entities.get(id1)
                ent2_text = entities.get(id2)
                
                if ent1_text and ent2_text:
                    ent1 = clean_entity(ent1_text)
                    ent2 = clean_entity(ent2_text)
                    predicate = RELATION_MAP[r_type]
                    
                    fact = f"({predicate} {ent1} {ent2})"
                    # Avoid duplicates
                    if fact not in lisp_facts:
                        lisp_facts.append(fact)
        
        if lisp_facts and len(text) > 20:
            gold_logic = " ".join(lisp_facts)
            reasoning = f"Simulated reasoning based on extracted BioRED relations: {', '.join([r[0] for r in relations])} found in text."
            
            processed_examples.append({
                "input": text,
                "gold_logic": gold_logic,
                "reasoning": reasoning
            })
            
        if len(processed_examples) >= sample_size:
            break
            
    with open(output_path, 'w') as f:
        json.dump(processed_examples, f, indent=2)
        
    print(f"[SUCCESS] Transformed {len(processed_examples)} BioRED examples to {output_path}")

if __name__ == "__main__":
    output = os.path.join(os.path.dirname(__file__), '../datasets/biored_processed.json')
    transform_biored(output, sample_size=150)

