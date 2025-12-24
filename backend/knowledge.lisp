;;; SDialectic Persistent Knowledge Base
(in-package :s-dialectic)
(limpar-memoria)

;;; --- Custom Definitions (Functions & Macros) ---
;;; --- Memories ---
(adicionar-memoria "A" "Entidade A")
(adicionar-memoria "B" "Entidade B")
(adicionar-memoria "C" "Entidade C")

;;; --- Relations ---
(adicionar-relacao "A" "É UM" "B")
(adicionar-relacao "B" "É UM" "C")
(adicionar-relacao "A" "É UM" "C")

;;; --- Rules ---

(format t "
;;; Knowledge Base Loaded Successfully.
")
