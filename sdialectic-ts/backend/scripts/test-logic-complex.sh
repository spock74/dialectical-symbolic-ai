#!/bin/bash

# Clear previous state to ensure a clean test
curl -X DELETE http://localhost:3000/api/admin/reset > /dev/null 2>&1

echo "Sending Complex Logic Query..."
curl -X POST http://localhost:3000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "X1 is the set of entities connected via the relation “founder” to “well-known simulation software company”. X2 is the set of entities connected via the relation “famous person” to “Asia”. X3 is the set of entities connected via the relation “famous person” to “Europe”. X4 is the set of entities obtained by performing set operations on X2 and X3. Then, which entities are in the intersection of X1 and X4?",
    "useMemory": true,
    "source": "complex_logic_test",
    "bypassSDialect": false
  }'
