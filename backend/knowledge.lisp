;;; SDialectic Persistent Knowledge Base
(in-package :s-dialectic)
(limpar-memoria)

;;; --- Memories ---
(adicionar-memoria "Zehn" "Um nome")
(adicionar-memoria "ZEHN" "Implicit Concept")
(adicionar-memoria "LLM" "Um modelo de linguagem")
(adicionar-memoria "Clorofila" "Um pigmento que absorve a luz do sol para realizar a fotossíntese.")
(adicionar-memoria "Fotossíntese" "Processo de produção de energia química")

;;; --- Relations ---
(adicionar-relacao 'ZEHN 'E-UM-LLM 'LLM)

;;; --- Rules ---

(format t "
;;; Knowledge Base Loaded Successfully.
")
