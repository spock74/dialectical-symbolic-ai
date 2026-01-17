#!/bin/bash

# Clear previous state
curl -X DELETE http://localhost:3000/api/admin/reset > /dev/null 2>&1

echo "Sending Concrete Instance Logic Query..."
curl -X POST http://localhost:3000/api/chat \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "Contexto: Alice é fundadora de uma empresa de simulação. Alice nasceu na Alemanha. Bob nasceu na França. Definições: X1 é o conjunto dos fundadores de empresas de simulação. X2 é o conjunto das pessoas que nasceram na Alemanha. Pergunta: Quem está na interseção de X1 e X2?",
    "useMemory": true,
    "source": "logic_instance_test",
    "bypassSDialect": false
  }'
