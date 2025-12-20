;;; NeuroLisp Cognitive Bootstrap
;;; Provides high-level primitives for the LLM to manage state and logic.

(defpackage :neurolisp
  (:use :cl)
  (:export :*memoria* :lembrar :recuperar :esquecer :listar-memorias :definir-ferramenta))

(in-package :neurolisp)

;;; --- Memory System ---

(defvar *memoria* (make-hash-table :test 'equal)
  "Global associative memory for the AI agent.")

(defun lembrar (chave valor)
  "Stores a fact in memory. Key should be a string or symbol."
  (setf (gethash (string chave) *memoria*) valor)
  (format nil "Memorizado: ~a = ~a" chave valor))

(defun recuperar (chave)
  "Retrieves a fact from memory."
  (let ((val (gethash (string chave) *memoria*)))
    (if val
        val
        (format nil "Nao encontrado: ~a" chave))))

(defun esquecer (chave)
  "Removes a fact from memory."
  (remhash (string chave) *memoria*)
  (format nil "Esquecido: ~a" chave))

(defun listar-memorias ()
  "Lists all stored keys and values."
  (let ((result nil))
    (maphash (lambda (k v) (push (list k v) result)) *memoria*)
    result))

;;; --- Tooling System ---

(defmacro definir-ferramenta (nome args &body corpo)
  "Defines a new tool (function) available to the agent."
  `(progn
     (defun ,nome ,args ,@corpo)
     (format nil "Ferramenta aprendida: ~a" ',nome)))

;;; --- Initialize User Package ---
(in-package :cl-user)
(use-package :neurolisp)

;; Macro para permitir que o LLM use (lisp ...) como wrapper sem erro
;; Definido diretamente no CL-USER para garantir visibilidade
(defmacro lisp (&body body)
  `(progn ,@body))

;; Inicialização
(format t "~%NeuroLisp Cognitive Bootstrap Carregado.~%")
(format t "Use (lembrar 'chave 'valor), (recuperar 'chave), (listar-memorias)~%")
