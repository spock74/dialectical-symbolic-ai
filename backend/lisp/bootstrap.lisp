;;; NeuroLisp Cognitive Bootstrap
;;; Provides high-level primitives for the LLM to manage state and logic.

;;; N(in-package :cl-user)

(format t "~&Iniciando Bootstrap SDialectic no CL-USER...~%")

;; 1. Definir a Macro LISP imediatamente para o CL-USER
(defmacro lisp (&body body)
  `(progn ,@body))

;; 2. Definir pacote principal S-DIALECTIC
(defpackage :s-dialectic
  (:use :cl)
  (:export :adicionar-memoria
           :recuperar-memoria
           :listar-memorias
           :limpar-memoria
           :definir-ferramenta
           ;; New Graph capabilities
           :adicionar-relacao
           :listar-relacoes
           :listar-dados-json
           ;; Persistence
           :salvar-estado
           :carregar-estado
           ;; Symbolic Inference
           :adicionar-regra
           :inferir))

(in-package :s-dialectic)

;;; --- Memory System ---

(defvar *knowledge-graph* (make-hash-table :test 'equal)
  "Armazena o grafo de conhecimento como pares chave-valor. (Legacy support)")

;; Structural Definitions
(defstruct concept
  (name nil :type symbol)
  (type :concept :type symbol)
  (properties nil :type list)) ; a-list of properties

(defstruct relation
  (subject nil :type symbol)
  (predicate nil :type symbol)
  (object nil :type symbol)
  (certainty 1.0 :type float)
  (provenance :user :type symbol)) ; :user or :inference

(defstruct rule
  (name nil :type symbol)
  (conditions nil :type list) ; List of pattern triples: ((?x is-a ?y) (?y is-a ?z))
  (consequences nil :type list)) ; List of outcome triples: ((?x is-a ?z))

(defvar *relations* nil
  "Stores relations as a list of relation structs.")

(defvar *rules* nil
  "Stores inference rules.")

(format t "~&Pacote S-DIALECTIC carregado.~%")

;;; --- JSON Helpers (Manual Implementation to avoid dependencies) ---

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string with all occurrences of part replaced by replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun escape-json-string (str)
  "Escapes backslashes and quotes for JSON compatibility."
  (let ((s (format nil "~a" str))) ;; Ensure it is a string
    (setf s (replace-all s "\\" "\\\\"))
    (setf s (replace-all s "\"" "\\\""))
    (setf s (replace-all s (string #\Newline) "\\n"))
    s))

(defun to-json-pair (key val)
  (format nil "{\"key\": \"~a\", \"value\": \"~a\"}"
          (escape-json-string key)
          (escape-json-string val)))

(defun to-json-triple (sub pred obj prov)
  (format nil "{\"source\": \"~a\", \"target\": \"~a\", \"relation\": \"~a\", \"provenance\": \"~a\"}"
          (escape-json-string sub)
          (escape-json-string obj)
          (escape-json-string pred)
          (escape-json-string prov)))

;;; --- Core Functions ---

(defun adicionar-memoria (chave valor)
  "Stores a fact in memory. Key should be a string or symbol."
  (setf (gethash (string chave) *knowledge-graph*) (string valor))
  (format nil "Memorizado: ~a" chave))

(defun recuperar-memoria (chave)
  "Retrieves a fact from memory."
  (let ((val (gethash (string chave) *knowledge-graph*)))
    (if val
        val
        (format nil "Nao encontrado: ~a" chave))))

(defun limpar-memoria ()
  "Clears all memory."
  (clrhash *knowledge-graph*)
  (setf *relations* nil)
  "Memoria limpa.")

(defun adicionar-relacao (sujeito predicado objeto)
  "Adds a structured relation between concepts."
  (let ((s (intern (string-upcase (string sujeito))))
      (p (intern (string-upcase (string predicado))))
        (o (intern (string-upcase (string objeto)))))
    
    ;; [Auto-Discovery] Ensure Nodes exist for Subject and Object
    (unless (gethash (string s) *knowledge-graph*)
      (setf (gethash (string s) *knowledge-graph*) "Implicit Concept"))
    (unless (gethash (string o) *knowledge-graph*)
      (setf (gethash (string o) *knowledge-graph*) "Implicit Concept"))

    ;; Avoid duplicates
    (unless (find-if (lambda (r) 
                       (and (eq (relation-subject r) s)
                            (eq (relation-predicate r) p)
                            (eq (relation-object r) o)))
                     *relations*)
      (push (make-relation :subject s :predicate p :object o :provenance :user) *relations*)
      (format nil "Relacao estruturada: ~a -[~a]-> ~a" s p o))))

(defun adicionar-regra (nome condicoes consequencias)
  "Adds a logical rule. Example: (adicionar-regra 'transitivity '((?x part-of ?y) (?y part-of ?z)) '((?x part-of ?z)))"
  (let ((nova-regra (make-rule :name nome :conditions condicoes :consequences consequencias)))
    (push nova-regra *rules*)
    (format nil "Regra aprendida: ~a" nome)))

;;; --- Inference Engine (Basic Forward Chaining) ---

(defun variable-p (x)
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defun unify (var val bindings)
  "Simple Unification."
  (cond
    ((equal var val) bindings)
    ((variable-p var)
     (let ((existing (assoc var bindings)))
       (if existing
           (if (eq (cdr existing) val) bindings nil)
           (cons (cons var val) bindings))))
    (t nil)))

(defun subst-bindings (term bindings)
  (if (variable-p term)
      (or (cdr (assoc term bindings)) term)
      term))

(defun match-pattern (pattern fact bindings)
  "Unifies a pattern ((?x p ?y)) with a fact (subject p object). Returns updated bindings or NIL."
  (let ((pat-s (first pattern))
        (pat-p (second pattern))
        (pat-o (third pattern))
        (fact-s (relation-subject fact))
        (fact-p (relation-predicate fact))
        (fact-o (relation-object fact)))
    
    ;; Check Predicate Match (Exactness required for now)
    (unless (or (variable-p pat-p) (eq pat-p fact-p))
      (return-from match-pattern nil))

    ;; Match Subject
    (let ((b2 (unify pat-s fact-s bindings)))
      (unless b2 (return-from match-pattern nil))
      ;; Match Object
      (unify pat-o fact-o b2))))

(defun variable-p (x)
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defun unify (var val bindings)
  "Simple Unification."
  (cond
    ((equal var val) bindings)
    ((variable-p var)
     (let ((existing (assoc var bindings)))
       (if existing
           (if (eq (cdr existing) val) bindings nil)
           (cons (cons var val) bindings))))
    (t nil)))

(defun apply-rule (rule facts)
  "Tries to apply a rule to the known facts. Returns list of NEW facts."
  (let ((new-facts nil))
    (labels ((find-matches (remaining-conditions current-bindings)
               (if (null remaining-conditions)
                   ;; All conditions pattern-matched! Generate consequences.
                   (dolist (cons-pattern (rule-consequences rule))
                     (let ((new-s (subst-bindings (first cons-pattern) current-bindings))
                           (new-p (subst-bindings (second cons-pattern) current-bindings))
                           (new-o (subst-bindings (third cons-pattern) current-bindings)))
                       (unless (find-if (lambda (f) (and (eq (relation-subject f) new-s)
                                                         (eq (relation-predicate f) new-p)
                                                         (eq (relation-object f) new-o)))
                                        facts)
                         (push (make-relation :subject new-s :predicate new-p :object new-o :provenance :inference) new-facts))))
                   ;; Else, try to match next condition
                   (let ((condition (first remaining-conditions)))
                     (dolist (fact facts)
                       (let ((new-bindings (match-pattern condition fact current-bindings)))
                         (when new-bindings
                           (find-matches (rest remaining-conditions) new-bindings))))))))
      (find-matches (rule-conditions rule) nil))
    new-facts))

(defun subst-bindings (term bindings)
  (if (variable-p term)
      (or (cdr (assoc term bindings)) term)
      term))

(defun inferir ()
  "Runs forward chaining inference until no new facts are derived."
  (let ((derived-count 0)
        (iteration 0)
        (max-iterations 10)) ; prevent infinite loops
    (loop
      (incf iteration)
      (let ((new-this-round nil))
        (dolist (rule *rules*)
          (let ((inferred (apply-rule rule *relations*)))
            (dolist (f inferred)
              ;; Double check duplicate in global *relations* inside the loop to be safe
              (unless (find-if (lambda (r) 
                                 (and (eq (relation-subject r) (relation-subject f))
                                      (eq (relation-predicate r) (relation-predicate f))
                                      (eq (relation-object r) (relation-object f))))
                               *relations*)
                (push f new-this-round)
                (push f *relations*)))))
        
        (when (null new-this-round) (return))
        (when (> iteration max-iterations) (return))
        (setf derived-count (+ derived-count (length new-this-round)))))
    (format nil "Inferencia concluida. ~a novos fatos derivados." derived-count)))

(defun listar-memorias ()
  "Lists all stored keys and values."
  (let ((result nil))
    (maphash (lambda (k v) (push (list k v) result)) *knowledge-graph*)
    result))

(defun listar-relacoes ()
  *relations*)

(defun listar-dados-json ()
  "Returns the entire state (memories + relations) as a valid JSON string."
  (let ((mem-strings nil)
        (rel-strings nil))
    ;; Serialize Memories
    (maphash (lambda (k v)
               (push (to-json-pair k v) mem-strings))
             *knowledge-graph*)
    
    ;; Serialize Relations (Structs)
    (dolist (r *relations*)
      (push (to-json-triple (relation-subject r) (relation-predicate r) (relation-object r) (relation-provenance r)) rel-strings))
    
    ;; Combine
    (format nil "{ \"nodes\": [~{~a~^, ~}], \"edges\": [~{~a~^, ~}] }"
            mem-strings
            rel-strings)))

;;; --- Persistence ---

(defun salvar-estado (filepath)
  "Saves the current state to a file using Lisp S-expressions."
  (with-open-file (stream filepath :direction :output 
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (let ((mem-list nil))
      (maphash (lambda (k v) (push (cons k v) mem-list)) *knowledge-graph*)
      (print mem-list stream)
      (print *relations* stream)))
  (format nil "Estado salvo em ~a" filepath))

(defun carregar-estado (filepath)
  "Loads state from a file."
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((mem-list (read stream))
              (rel-list (read stream)))
          ;; Restore Hash Table
          (clrhash *knowledge-graph*)
          (dolist (pair mem-list)
            (setf (gethash (car pair) *knowledge-graph*) (cdr pair)))
          ;; Restore Relations
          (setf *relations* rel-list)
          (format nil "Estado carregado de ~a. ~a memorias, ~a relacoes."
                  filepath (length mem-list) (length rel-list))))
    (error (e) (format nil "Erro ao carregar: ~a" e))))

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
(defmacro lisp (&body body)
  `(progn ,@body))

;; Inicialização
(format t "~%SDialectic Cognitive Bootstrap 2.0 Carregado.~%")
(format t "JSON Support: ENABLED. Persistence: ENABLED. Graph: RELATIONAL.~%")
