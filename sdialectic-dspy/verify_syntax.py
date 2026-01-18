import requests
import json
import time

url = "http://localhost:8000/ingest/atomize"
# Using the text that caused the issue
payload = {"text": "Chronic stress increases cortisol levels.", "persist": False} # Don't persist, just check logic
headers = {"Content-Type": "application/json"}

print(f"Sending request to {url}...")
try:
    response = requests.post(url, json=payload, headers=headers, timeout=20)
    print(f"Status Code: {response.status_code}")
    if response.status_code == 200:
        data = response.json()
        print("Response JSON:")
        print(json.dumps(data, indent=2))
        
        logic = data.get("logic", "")
        if "?" in logic:
            print("[FAILURE] Logic still contains '?'.")
        else:
            print("[SUCCESS] Logic is clean of '?'.")
    else:
        print(f"Error: {response.text}")

except Exception as e:
    print(f"Request failed: {e}")
