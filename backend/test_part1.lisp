
(in-package :cl-user)
(format t "~&[Test] Loading Stage 1...~%")
(setf *print-length* nil)
(setf *print-level* nil)
(setf *print-lines* nil)
(setf *print-base* 10)
(setf *print-case* :upcase)

(defpackage :s-dialectic
  (:use :cl)
  (:export 
    :adicionar-memoria
    :recuperar-memoria
    :listar-memorias
    :limpar-memoria
    :adicionar-relacao
    :buscar-relacoes
    :listar-relacoes
    :listar-dados-json
    :adicionar-regra
    :inferir
    :listar-regras
    :carregar-regras-essenciais
    :salvar-estado
    :carregar-estado
    :reset-total
    :definir-ferramenta
    :definir-funcao
    :definir-macro))

(in-package :s-dialectic)
(format t "[Test] Package :S-DIALECTIC defined successfully.~%")
(quit)
