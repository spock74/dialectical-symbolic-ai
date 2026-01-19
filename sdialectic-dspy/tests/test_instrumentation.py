import sys
import os
import csv
import shutil

# Add project root to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from optimization.instrumentation import GeminiLogger

# Add project root to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from optimization.instrumentation import GeminiLogger
from core.config import settings
import dspy

def test_logger():
    print("Testing GeminiLogger with REAL MODEL...")
    csv_file = "test_gemini_log.csv"
    
    # Clean up previous run
    if os.path.exists(csv_file):
        os.remove(csv_file)
        
    if not settings.GEMINI_API_KEY:
        print("[FAIL] No API Key found in settings.")
        return

    # Setup Real Model
    print(f"Initializing Gemini: {settings.TEACHER_MODEL_NAME}")
    try:
        real_lm = dspy.LM(model=settings.TEACHER_MODEL_NAME, api_key=settings.GEMINI_API_KEY)
        logger = GeminiLogger(real_lm, filename=csv_file)
        
        # Trigger call
        print("Invoking Logger (Real API Call)...")
        result = logger("Say exactly 'HELLO' and nothing else.")
        print(f"Result: {result}")
        
    except Exception as e:
         print(f"[FAIL] API Call failed: {e}")
         return
    
    # Verify CSV
    if not os.path.exists(csv_file):
        print("[FAIL] CSV file not created.")
        return

    with open(csv_file, 'r') as f:
        reader = list(csv.reader(f))
        
    print(f"CSV Rows: {len(reader)}")
    if len(reader) < 2:
        print(f"[FAIL] Expected at least 2 rows, got {len(reader)}")
        return
        
    header = reader[0]
    row = reader[1]
    
    print(f"Header: {header}")
    print(f"Row: {row}")
    
    # Check if we captured valid data
    if row[1] == "SUCCESS":
        print("[SUCCESS] Logger captured real API call.")
        print(f"Tokens: Input={row[3]}, Output={row[4]}, Total={row[5]}")
    else:
        print(f"[FAIL] Logged status was not SUCCESS: {row}")

    # Clean up
    if os.path.exists(csv_file):
        os.remove(csv_file)

if __name__ == "__main__":
    test_logger()
