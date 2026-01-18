import requests
import json
import subprocess
import time

# Start server
server = subprocess.Popen(["python", "main.py"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
print(f"Server started with PID {server.pid}")
time.sleep(15) # Wait for startup (increased)

try:
    url = "http://localhost:8000/ingest/atomize"
    payload = {"text": "Regular exercise improves cardiovascular health.", "persist": True}
    headers = {"Content-Type": "application/json"}

    print(f"Sending request to {url}...")
    response = requests.post(url, json=payload, headers=headers, timeout=10)
    print(f"Status Code: {response.status_code}")
    print("Response JSON:")
    print(json.dumps(response.json(), indent=2))
    
    # Check file
    with open("core/kernel/imported_knowledge.lisp", "r") as f:
        content = f.read()
        print("\n--- File Content ---")
        print(content)
        if "(IMPROVES (EXERCISE) (HEALTH CARDIOVASCULAR))" in content or "IMPROVES" in content:
            print("SUCCESS: Content persisted.")
        else:
            print("FAILURE: Content not found in file.")

except Exception as e:
    print(f"Test failed: {e}")

finally:
    server.terminate()
    print("Server terminated.")
