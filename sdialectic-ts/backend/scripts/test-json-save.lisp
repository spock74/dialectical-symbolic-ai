;; 0. Load Bootstrap
(load "backend/lisp/bootstrap.lisp")

(in-package :s-dialectic)

;; 1. Clear State
(limpar-memoria)

;; 2. Populate Mock Data
(adicionar-relacao "Subject" "PREDICATE" "Object" :category :PROPERTY)
(adicionar-relacao "A" "CAUSES" "B" :category :CAUSAL)

;; 3. Test Serialization
(format t "~%--- JSON OUTPUT CHECK ---~%")
(princ (listar-dados-json))
(format t "~%--- END JSON CHECK ---~%")
