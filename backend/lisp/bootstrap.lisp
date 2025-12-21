;;; NeuroLisp Cognitive Bootstrap
;;; Provides high-level primitives for the LLM to manage state and logic.

;;; N(in-package :cl-user)

(format t "~&Iniciando Bootstrap SDialectic no CL-USER...~%")

;; 1. Definir a Macro LISP imediatamente para o CL-USER
;; Isso permite que (lisp ...) funcione imediatamente
(defmacro lisp (&body body)
  `(progn ,@body))

;; 2. Definir pacote principal S-DIALECTIC
(defpackage :s-dialectic
  (:use :cl)
  (:export :adicionar-memoria
           :recuperar-memoria
           :listar-memorias
           :limpar-memoria
           :definir-ferramenta))

(in-package :s-dialectic)

;;; --- Memory System ---

(defvar *knowledge-graph* (make-hash-table :test 'equal)
  "Armazena o grafo de conhecimento como pares chave-valor.")

(format t "~&Pacote S-DIALECTIC carregado.~%")

(defun adicionar-memoria (chave valor)
  "Stores a fact in memory. Key should be a string or symbol."
  (setf (gethash (string chave) *knowledge-graph*) valor)
  (format nil "Memorizado: ~a = ~a" chave valor))

(defun recuperar-memoria (chave)
  "Retrieves a fact from memory."
  (let ((val (gethash (string chave) *knowledge-graph*)))
    (if val
        val
        (format nil "Nao encontrado: ~a" chave))))

(defun limpar-memoria ()
  "Clears all memory."
  (clrhash *knowledge-graph*)
  "Memoria limpa.")

(defun listar-memorias ()
  "Lists all stored keys and values."
  (let ((result nil))
    (maphash (lambda (k v) (push (list k v) result)) *knowledge-graph*)
    result))

;;; --- Aliases for Legacy LLM Support ---
(defun lembrar (chave valor)
  (format t "~&[WARN] Deprecated function 'lembrar' called. Redirecting...~%")
  (adicionar-memoria chave valor))
(export 'lembrar)


;;; --- Tooling System ---

(defmacro definir-ferramenta (nome args &body corpo)
  "Defines a new tool (function) available to the agent."
  `(progn
     (defun ,nome ,args 
       (declare (ignorable ,@args))
       ,@corpo)
     (format nil "Ferramenta aprendida: ~a" ',nome)))

;;; --- Initialize User Package ---
(in-package :cl-user)
(use-package :s-dialectic)

;; Macro para permitir que o LLM use (lisp ...) como wrapper sem erro
;; Definido diretamente no CL-USER para garantir visibilidade
(defmacro lisp (&body body)
  `(progn ,@body))

;; Inicialização
(format t "~%SDialectic Cognitive Bootstrap Carregado.~%")
(format t "Use (adicionar-memoria 'chave 'valor), (recuperar-memoria 'chave), (listar-memorias)~%")
