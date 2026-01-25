import os
import json
import random
import time
from google import genai
from google.genai import types
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Configuration
API_KEY = os.environ.get("GEMINI_API_KEY")
if not API_KEY:
    # Try to find it in the system environment if not in .env
    pass 

if not API_KEY:
    print("Warning: GEMINI_API_KEY not found in .env, checking system...")
    # It might be passed via the shell, so os.environ should have it if open_browser passed it
    # But for now let's assume it is there.
    pass

TEACHER_MODEL = "gemini-2.5-flash-lite"
INPUT_DATASET_PATH = "datasets/dataset_500.json"
OUTPUT_DATASET_PATH = "datasets/test_dataset_qwen_spv_01.json"
NUM_EXAMPLES_TO_GENERATE = 50
NUM_FEW_SHOT_EXAMPLES = 10

def load_dataset(filepath):
    """Loads a JSON dataset."""
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: File not found at {filepath}")
        return []

def save_dataset(filepath, data):
    """Saves data to a JSON file."""
    with open(filepath, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=4, ensure_ascii=False)
    print(f"Dataset saved to {filepath}")

def create_prompt(few_shot_examples):
    """Creates the prompt for the model."""
    
    prompt_intro = """You are an expert in biomedical knowledge extraction. 
    Your task is to generate synthetic examples of medical text and their corresponding 
    symbolic knowledge representation in Lisp S-expression format.
    
    The Lisp S-expressions representing the knowledge graph nodes and relations.
    
    Here are some examples of the desired format:
    """
    
    examples_str = ""
    for ex in few_shot_examples:
        examples_str += f"\nInput: {ex['input']}\nOutput:\n{ex['output']}\n"
        
    task_instruction = f"""
    
    Now, generate {NUM_EXAMPLES_TO_GENERATE} NEW and DIVERSE biomedical examples.
    
    Constraints:
    1. The 'input' should be a realistic biomedical sentence or short paragraph describing relations between proteins, diseases, drugs, or biological processes.
    2. The 'output' MUST be a valid Lisp S-expression extraction of the entities and relations in the input.
    3. Use the same style and predicates as the examples (e.g., REL, PROP, TYPE).
    4. Output strictly a JSON array of objects, where each object has "input" and "output" keys.
    5. Do not include markdown code blocks (```json ... ```) in your response, just the raw JSON string if possible, or I will parse it out.
    """
    
    return prompt_intro + examples_str + task_instruction

def main():
    print("--- Starting Dataset Generation ---")
    
    # Initialize Gemini Client
    try:
         client = genai.Client(api_key=API_KEY)
    except Exception as e:
         print(f"Error initializing client: {e}")
         return
    
    # Load existing dataset for few-shot
    existing_data = load_dataset(INPUT_DATASET_PATH)
    if not existing_data:
        print("Could not load input dataset. Exiting.")
        return

    # Select random few-shot examples
    few_shot_examples = random.sample(existing_data, min(len(existing_data), NUM_FEW_SHOT_EXAMPLES))
    
    # Create Prompt
    prompt = create_prompt(few_shot_examples)
    
    print(f"Prompting {TEACHER_MODEL} to generate {NUM_EXAMPLES_TO_GENERATE} examples...")
    
    try:
        response = client.models.generate_content(
            model=TEACHER_MODEL,
            contents=prompt,
            config=types.GenerateContentConfig(
                response_mime_type="application/json",
                max_output_tokens=8192, 
                temperature=0.7 
            )
        )
        
        # Parse output
        try:
            generated_data = json.loads(response.text)
            
            # Validate structure
            if isinstance(generated_data, list) and all("input" in item and "output" in item for item in generated_data):
                print(f"Successfully generated {len(generated_data)} examples.")
                save_dataset(OUTPUT_DATASET_PATH, generated_data)
            else:
                 # Attempt to extract if wrapped in a top-level object key like "examples"
                if isinstance(generated_data, dict):
                    # Check first value that is a list
                    found = False
                    for key, value in generated_data.items():
                        if isinstance(value, list) and all("input" in item and "output" in item for item in value):
                            print(f"Extracted {len(value)} examples from key '{key}'.")
                            save_dataset(OUTPUT_DATASET_PATH, value)
                            found = True
                            break
                    if not found:
                         print("Error: Generated JSON does not contain a list of input/output pairs.")
                         print("Preview:", str(generated_data)[:500])
                else:
                    print("Error: Generated JSON is not a list.")
                    print("Preview:", str(generated_data)[:500])

        except json.JSONDecodeError as e:
            print(f"Error parsing JSON response: {e}")
            print("Raw response:", response.text)

    except Exception as e:
        print(f"An error occurred during generation: {e}")

if __name__ == "__main__":
    main()
