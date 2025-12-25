Demência é uma categoria genérica de doenças cerebrais que gradualmente e a longo prazo causam diminuição da capacidade de raciocínio e memória, a tal ponto que interfere com a função normal da pessoa. Outros sintomas comuns são problemas emocionais, problemas de linguagem e diminuição da motivação. Geralmente a consciência da pessoa não é afetada. Para um diagnóstico de demência é necessário que haja uma alteração da função mental normal da pessoa e um declínio superior ao que seria expectável devido à idade. Este grupo de doenças afeta também de forma significativa os cuidadores da pessoa.

O tipo mais comum de demência é a doença de Alzheimer, responsável por 50 a 70% dos casos. Entre outras causas comuns estão a demência vascular (25%), demência com corpos de Lewy (15%) e demência frontotemporal. Entre outras possíveis causas, menos prováveis, estão a hidrocefalia de pressão normal, doença de Parkinson, sífilis e doença de Creutzfeldt-Jakob. A mesma pessoa pode manifestar mais de um tipo de demência. Uma minoria de casos é de origem hereditária. No DSM-5, a demência foi reclassificada como perturbação neurocognitiva, com vários graus de gravidade. O diagnóstico tem por base a história da doença e exames cognitivos, complementados por exames imagiológicos e análises ao sangue para despistar outras possíveis causas. Um dos exames cognitivos mais usados é o mini exame do estado mental. As medidas para prevenir a demência consistem em diminuir os fatores de risco como a hipertensão arterial, tabagismo, diabetes e obesidade. Não está recomendado o rastreio na população em geral.



"Atue como um Arquiteto de Software Principal. Estou modernizando o 'SDialectic', um sistema neuro-simbólico.

Mudança Arquitetural: Estamos removendo a dependência externa do binário SBCL (Common Lisp). O Kernel Lógico será reescrito em TypeScript puro, mas manterá a capacidade de processar comandos no estilo Lisp (S-Expressions) que o LLM gera.

Stack: Node.js, TypeScript, Genkit 1.25+, Google Gemini 2.5 Flash Lite.

Sua Missão: Implementar o 'TS-Symbolic-Kernel'

Passo 1: O Motor de Grafo (src/logic/graph-engine.ts) Crie uma classe robusta KnowledgeGraph que substitua o bootstrap.lisp.

Estrutura de Dados: Use Map<string, Concept> para nós e um Map<string, Relation[]> indexado pelo sujeito para acesso O(1).

Métodos Core:

addNode(id: string, type: string)

addRelation(s: string, p: string, o: string): Deve verificar duplicatas semanticamente.

getGraphSnapshot(): Retorna o estado atual para visualização.

saveState(filepath: string): Persiste o grafo como JSON (substituindo o knowledge.lisp).

loadState(filepath: string): Hidrata o grafo do JSON.

Passo 2: Implementar Causalidade (Judea Pearl) Adicione métodos de Intervenção na classe KnowledgeGraph:

doIntervention(targetNode: string, forcedValue: any):

Cria um snapshot do estado atual (para rollback).

Mutilação do Grafo: Remove todas as arestas onde object === targetNode (corta as causas do nó).

Define o valor forçado no nó.

resetIntervention(): Restaura o snapshot anterior.

Passo 3: O Interpretador 'Tiny-Lisp' (src/logic/lisp-interpreter.ts) Crie um parser leve que receba strings geradas pelo LLM e execute métodos do Graph.

Input: <lisp>(adicionar-relacao "A" "e_um" "B")</lisp> ou (inferir).

Mapping:

Parseie a S-Expression (pode usar regex avançado ou uma lib minúscula como sexpr-plus).

Mapeie:

adicionar-memoria -> graph.addNode(...)

adicionar-relacao -> graph.addRelation(...)

inferir -> graph.infer() (Implemente uma lógica simples de transitividade por enquanto).

undo -> graph.resetIntervention()

Passo 4: Integração com Genkit No arquivo de fluxo (reflective-loop.ts):

Instancie o KnowledgeGraph como um Singleton.

Dentro do fluxo, quando o Gemini 2.5 Flash Lite responder com tags <lisp>, passe o conteúdo para o lisp-interpreter.ts.

Diferencial: Retorne o resultado da execução (ex: "Fato adicionado", "Erro de sintaxe") de volta para o contexto do LLM, permitindo que ele se corrija."