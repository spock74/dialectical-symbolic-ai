import requests
import json
import sys

url = "http://localhost:8000/ingest/atomize"
payload = {"text": "Smoking significantly increases the risk of lung cancer."}
headers = {"Content-Type": "application/json"}

print(f"Sending request to {url}...")
try:
    response = requests.post(url, json=payload, headers=headers, timeout=60)
    print(f"Status Code: {response.status_code}")
    if response.status_code == 200:
        print("Response JSON:")
        print(json.dumps(response.json(), indent=2))
        sys.exit(0)
    else:
        print(f"Error: {response.text}")
        sys.exit(1)
except Exception as e:
    print(f"Request failed: {e}")
    sys.exit(1)
