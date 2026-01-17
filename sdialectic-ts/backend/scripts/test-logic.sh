#!/bin/bash

curl -X POST http://localhost:3000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "Seja X1 o conjunto formado pelos fundadores de empresas de simulação. Seja X2 o conjunto das pessoas que nasceram na Alemanha. Quem está na interseção de X1 e X2?",
    "useMemory": true,
    "source": "logic_test_context",
    "bypassSDialect": false
  }'
