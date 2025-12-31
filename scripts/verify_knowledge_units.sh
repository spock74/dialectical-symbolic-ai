#!/bin/bash
# Scripts to test Knowledge Unit APIs

BASE_URL="http://localhost:3000/api"

echo "1. Creating a dummy unit by extracting (simulated)..."
# We can't simulate extraction easily without a file. 
# But we can assume there might be units if the user has used the app.
# Or we can inspect the list.

echo "2. Listing Units..."
curl -s "$BASE_URL/knowledge-units" | python3 -m json.tool

echo "3. Testing Load (simulated id 'default')..."
curl -X POST -s "$BASE_URL/knowledge-units/default/load" | python3 -m json.tool

echo "4. Testing Unload (simulated id 'default')..."
curl -X POST -s "$BASE_URL/knowledge-units/default/unload" | python3 -m json.tool

echo "5. Testing Delete (simulated id 'non-existent')..."
curl -X DELETE -s "$BASE_URL/knowledge-units/non-existent-unit-123" | python3 -m json.tool

# Check if 'default' exists and test operations on it if list is valid
# This script mainly verifies endpoints are reachable and respond.
