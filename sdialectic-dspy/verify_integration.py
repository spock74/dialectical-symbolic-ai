import requests
import time
import subprocess
import os
import signal
import sys

def run_integration_test():
    print("Starting Integration Test...")
    
    # 1. Start Server
    print("[INFO] Launching API Server...")
    process = subprocess.Popen(
        ["python", "main.py"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        preexec_fn=os.setsid # process group for clean kill
    )
    
    # Wait for startup
    time.sleep(5)
    
    try:
        # 2. Define Payload
        text = "Socrates is a man. All men are mortal."
        payload = {"text": text, "persist": False} # Don't pollute persistence during quick test
        
        # 3. Send Request
        print(f"[INFO] Sending request: {text[:20]}...")
        url = "http://localhost:8000/ingest/atomize"
        response = requests.post(url, json=payload)
        
        # 4. Validate Response
        if response.status_code == 200:
            data = response.json()
            print("\n[RESPONSE]")
            print(json.dumps(data, indent=2))
            
            logic = data.get("logic", "")
            status = data.get("system2_status", "")
            
            # Syntax Check
            if logic.startswith("'") and logic.count("(") == logic.count(")"):
                print("  [PASS] Logic Syntax seems valid")
            else:
                print("  [FAIL] Logic Syntax invalid")
                
            # Kernel Check
            if status == "ACCEPTED":
                print("  [PASS] Kernel Accepted Logic")
            else:
                print(f"  [WARN] Kernel Status: {status}")
                
        else:
            print(f"[FAIL] API Error: {response.status_code}")
            print(response.text)

    except Exception as e:
        print(f"[ERROR] Test failed: {e}")
        
    finally:
        # 5. Cleanup
        print("[INFO] Stopping Server...")
        os.killpg(os.getpgid(process.pid), signal.SIGTERM)
        process.wait()
        print("[INFO] Test Completed.")

if __name__ == "__main__":
    import json
    run_integration_test()
