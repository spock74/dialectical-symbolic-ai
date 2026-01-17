#!/bin/bash

echo "=== STEP 0: RESET ==="
curl -X DELETE http://localhost:3000/api/admin/reset > /dev/null 2>&1
echo "Memory Cleared."
echo ""

echo "=== STEP 1: INGESTION (Teaching the System) ==="
# We provide the axioms and instance data.
curl -X POST http://localhost:3000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "Fact 1: Alice is a founder of a simulation company. Fact 2: Alice was born in Germany. Fact 3: Bob was born in France. Definition 1: X1 is the set of founders of simulation companies. Definition 2: X2 is the set of people born in Germany.",
    "useMemory": true,
    "source": "logic_ingest_step",
    "bypassSDialect": false
  }'
echo ""
echo "Ingestion Complete. Waiting 5s for processing..."
sleep 5

echo "=== STEP 2: QUERY (Testing Logic Engine) ==="
# We ask the question WITHOUT providing the context again.
# The system must retrieve X1 and X2 definitions from memory and resolve the query.
curl -s -X POST http://localhost:3000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "Who is in the intersection of X1 and X2?",
    "useMemory": true,
    "source": "logic_query_step",
    "bypassSDialect": false
  }'
