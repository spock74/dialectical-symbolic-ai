;;; SDialectic Persistent Knowledge Base
(in-package :s-dialectic)
(limpar-memoria)

;;; --- Memories ---
(adicionar-memoria "Zehn" "Um nome")
(adicionar-memoria "ZEHN" "Implicit Concept")
(adicionar-memoria "LLM" "Um modelo de linguagem")
(adicionar-memoria "Clorofila" "Um pigmento que absorve a luz do sol para realizar a fotossíntese.")
(adicionar-memoria "Fotossíntese" "Processo de produção de energia química")
(adicionar-memoria "fotossíntese" "Um processo biológico")
(adicionar-memoria "plantas" "Organismos autotróficos")
(adicionar-memoria "algas" "Organismos autotróficos")
(adicionar-memoria "cianobactérias" "Organismos autotróficos")
(adicionar-memoria "carbono" "Elemento químico")
(adicionar-memoria "água" "Substância química")
(adicionar-memoria "oxigênio" "Elemento químico")
(adicionar-memoria "atmosfera da Terra" "Parte do ambiente")
(adicionar-memoria "energia química" "Tipo de energia")
(adicionar-memoria "respiração celular" "Processo biológico")
(adicionar-memoria "moleculas de carboidratos" "Tipos de moléculas")
(adicionar-memoria "açúcares" "Tipos de açúcares")
(adicionar-memoria "amidos" "Tipos de amidos")
(adicionar-memoria "FOTOSSÍNTESE" "Implicit Concept")
(adicionar-memoria "PLANTAS E OUTROS ORGANISMOS" "Implicit Concept")
(adicionar-memoria "SIM" "Implicit Concept")
(adicionar-memoria "DO GREGO PHŌS (Φῶς), &QUOT;LUZ&QUOT;, E SÚNTHESIS (ΣΎΝΘΕΣΙς), &QUOT;COLOCAR JUNTO&QUOT;" "Implicit Concept")

;;; --- Relations ---
(adicionar-relacao 'ZEHN 'E-UM-LLM 'LLM)
(adicionar-relacao 'FOTOSSÍNTESE 'REALIZADO POR 'PLANTAS E OUTROS ORGANISMOS)
(adicionar-relacao 'FOTOSSÍNTESE 'PRODUZIR SEU PRÓPRIO ALIMENTO 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'CONVERTER ENERGIA LUMINOSA EM ENERGIA QUÍMICA 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'LIBERAR POSTERIORMENTE PARA ALIMENTAR AS ATIVIDADES DO ORGANISMO 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'ARMAZENADA EM MOLÉCULAS DE CARBOIDRATOS 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'SINTETIZAR A PARTIR DO DIÓXIDO DE CARBONO E DA ÁGUA 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'NOME FOTOSSÍNTESE 'DO GREGO PHŌS (Φῶς), &QUOT;LUZ&QUOT;, E SÚNTHESIS (ΣΎΝΘΕΣΙς), &QUOT;COLOCAR JUNTO&QUOT;)
(adicionar-relacao 'FOTOSSÍNTESE 'PARTE DESSA ENERGIA QUÍMICA É ARMAZENADA EM MOLÉCULAS DE CARBOIDRATOS 'SIM)
(adicionar-relacao 'FOTOSSÍNTESE 'MAIOR PARTE DA ENERGIA NECESSÁRIA PARA A VIDA NO PLANETA 'SIM)

;;; --- Rules ---

(format t "
;;; Knowledge Base Loaded Successfully.
")
