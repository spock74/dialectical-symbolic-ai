(load "lisp/bootstrap.lisp")
(in-package :s-dialectic)

(format t "~%--- Testando Inferencia ---~%")
(adicionar-relacao 'A "É UM" 'B)
(adicionar-relacao 'B "É UM" 'C)

(format t "~%Antes da inferencia: ~a~%" (length *relations*))
(inferir)
(format t "Depois da inferencia: ~a~%" (length *relations*))

(format t "~%Relacoes:")
(dolist (r *relations*)
  (format t "~%  ~a -[~a]-> ~a" 
          (relation-subject r) 
          (relation-predicate r) 
          (relation-object r)))

(assert (find-if (lambda (r) 
                   (and (eq (relation-subject r) (normalizar-termo 'A))
                        (eq (relation-object r) (normalizar-termo 'C))))
                 *relations*))

(format t "~%~%TESTE SUCESSO! A inferencia transitiva funcionou.~%")
(quit)
