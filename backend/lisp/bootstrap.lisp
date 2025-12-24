;;; NeuroLisp Cognitive Bootstrap
;;; Provides high-level primitives for the LLM to manage state and logic.

(in-package :cl-user)

(format t "~&Iniciando Bootstrap SDialectic no CL-USER...~%")

;; 1. Definir a Macro LISP imediatamente para o CL-USER
(defmacro lisp (&body body)
  `(progn ,@body))

;; Ensure no truncation for large outputs
(setf *print-length* nil)
(setf *print-level* nil)
(setf *print-lines* nil)
(setf *print-base* 10)
(setf *print-case* :upcase)

;; Alias for easier recovery
(defun top () (abort))

;; 2. Definir pacote principal S-DIALECTIC
(defpackage :s-dialectic
  (:use :cl)
    (:export :adicionar-memoria
           :recuperar-memoria
           :listar-memorias
           :limpar-memoria
           :definir-ferramenta
           :definir-funcao
           :definir-macro
           :buscar-relacoes
           ;; New Graph capabilities
           :adicionar-relacao
           :listar-relacoes
           :listar-regras
           :listar-dados-json
           :print-graph-json
           ;; Persistence
           :salvar-estado
           :carregar-estado
           :reset-total
           ;; Symbolic Inference
           :adicionar-regra
           :inferir
           :carregar-regras-essenciais))

(in-package :s-dialectic)




;;; --- Memory System ---

(defvar *knowledge-graph* (make-hash-table :test 'equal)
  "Armazena o grafo de conhecimento como pares chave-valor. (Legacy support)")

(defvar *custom-definitions* nil
  "Source code of custom tools (functions/macros) defined by the LLM.")

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

;;; --- Core Helpers ---

(defun variable-p (x)
  "Checks if a term is a logic variable (starts with ?)."
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defun normalizar-termo (x)
  "Normalizes a term into a symbol in the S-DIALECTIC package. Everything becomes a symbol in S-DIALECTIC."
  (cond
    ((symbolp x) (intern (string-upcase (symbol-name x)) :s-dialectic))
    ((stringp x) (intern (string-upcase x) :s-dialectic))
    (t x)))

(defun normalizar-tripla (tripla)
  "Normalizes each element of a triple (S P O)."
  (mapcar #'normalizar-termo tripla))

;;; --- Core Functions ---

(defun adicionar-memoria (chave valor)
  "Stores a fact in memory. Key should be a string or symbol. Detects redundancy."
  (let* ((k (string-upcase (string chave)))
         (v (string valor))
         (existing (gethash k *knowledge-graph*)))
    (if (and existing (string-equal existing v))
        (format nil "AVISO: O conceito '~a' ja esta na memoria com esta descricao." k)
        (progn
          (setf (gethash k *knowledge-graph*) v)
          (format nil "Memorizado: ~a" k)))))

(defun recuperar-memoria (chave)
  "Retrieves a fact from memory."
  (let ((val (gethash (string-upcase (string chave)) *knowledge-graph*)))
    (if val
        val
        (format nil "Nao encontrado: ~a" chave))))



(defun adicionar-relacao (sujeito predicado objeto)
  "Adds a structured relation between concepts. Detects redundancy."
  (let ((s (normalizar-termo sujeito))
        (p (normalizar-termo predicado))
        (o (normalizar-termo objeto)))
    
    ;; Avoid duplicates
    (if (find-if (lambda (r) 
                    (and (eq (relation-subject r) s)
                         (eq (relation-predicate r) p)
                         (eq (relation-object r) o)))
                  *relations*)
        (format nil "AVISO: A relacao ~a -[~a]-> ~a ja existe." s p o)
        (progn
          ;; [Auto-Discovery] Ensure Nodes exist for Subject and Object
          (unless (gethash (string s) *knowledge-graph*)
            (setf (gethash (string s) *knowledge-graph*) "Implicit Concept"))
          (unless (gethash (string o) *knowledge-graph*)
            (setf (gethash (string o) *knowledge-graph*) "Implicit Concept"))
          
          (push (make-relation :subject s :predicate p :object o :provenance :user) *relations*)
          (format nil "Relacao estruturada: ~a -[~a]-> ~a" s p o)))))

(defun buscar-relacoes (conceito)
  "Searches for all relations involving the given concept (as subject or object)."
  (let* ((c (normalizar-termo conceito))
         (found (remove-if-not (lambda (r) 
                                 (or (eq (relation-subject r) c)
                                     (eq (relation-object r) c)))
                               *relations*)))
    (if found
        (mapcar (lambda (r) 
                  (format nil "~a -[~a]-> ~a" 
                          (relation-subject r) 
                          (relation-predicate r) 
                          (relation-object r))) 
                found)
        (format nil "Nenhuma relacao formal encontrada para: ~a" conceito))))

(defun adicionar-regra (nome condicoes consequencias)
  "Adds a logical rule. Automatically normalizes terms."
  (let ((n (normalizar-termo nome))
        (conds (mapcar #'normalizar-tripla condicoes))
        (consq (mapcar #'normalizar-tripla consequencias)))
    (let ((nova-regra (make-rule :name n :conditions conds :consequences consq)))
      (push nova-regra *rules*)
      (format nil "Regra aprendida: ~a" n))))

(defun carregar-regras-essenciais ()
  "Adiciona regras lógicas básicas ao sistema."
  (adicionar-regra 'TRANSITIVIDADE-CATEGORICA
                   '((?x "É UM" ?y) (?y "É UM" ?z))
                   '((?x "É UM" ?z)))
  "Regras essenciais carregadas.")

;;; --- Inference Engine (Basic Forward Chaining) ---

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
  "Tries to match a pattern triple against a relation fact."
  (let ((pat-s (normalizar-termo (first pattern)))
        (pat-p (normalizar-termo (second pattern)))
        (pat-o (normalizar-termo (third pattern)))
        (fact-s (relation-subject fact))
        (fact-p (relation-predicate fact))
        (fact-o (relation-object fact)))
    (unless (or (variable-p pat-p) (eq pat-p fact-p))
      (return-from match-pattern nil))
    (let ((b2 (unify pat-s fact-s bindings)))
      (when b2 (unify pat-o fact-o b2)))))

(defun limpar-memoria ()
  "Clears all memory and custom definitions."
  (clrhash *knowledge-graph*)
  (setf *relations* nil)
  (setf *rules* nil)
  (setf *custom-definitions* nil)
  (carregar-regras-essenciais)
  "Memoria limpa.")

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
    (if (> derived-count 0)
        (format nil "Inferencia concluida. ~a novos fatos derivados." derived-count)
        "Nenhum fato novo derivado.")))

(defun listar-memorias (&rest args)
  "Lists all stored keys and values. Ignores extra arguments from LLMs."
  (declare (ignore args))
  (let ((result nil))
    (maphash (lambda (k v) (push (list k v) result)) *knowledge-graph*)
    result))

(defun listar-relacoes (&rest args)
  "Lists all structured relations. Ignores extra arguments."
  (declare (ignore args))
  *relations*)

(defun listar-regras (&rest args)
  "Lists all learned rules. Ignores extra arguments."
  (declare (ignore args))
  *rules*)

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
  "Saves the current state to a file as executable Lisp commands."
  (with-open-file (stream filepath :direction :output 
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (format stream ";;; SDialectic Persistent Knowledge Base~%")
    (format stream "(in-package :s-dialectic)~%")
    (format stream "(limpar-memoria)~%~%")

    (format stream ";;; --- Custom Definitions (Functions & Macros) ---~%")
    (dolist (def (reverse *custom-definitions*))
      (format stream "~s~%~%" def))

    (format stream ";;; --- Memories ---~%")
    (maphash (lambda (k v) 
               (format stream "(adicionar-memoria ~s ~s)~%" k v))
             *knowledge-graph*)
    
    (format stream "~%;;; --- Relations ---~%")
    (dolist (r (reverse *relations*)) ;; Reverse to maintain original order if important
      (format stream "(adicionar-relacao ~s ~s ~s)~%" 
              (string (relation-subject r))
              (string (relation-predicate r))
              (string (relation-object r))))

    (format stream "~%;;; --- Rules ---~%")
    (dolist (rule (reverse *rules*))
      ;; Skip essential rules to avoid duplicates upon reload
      (unless (eq (rule-name rule) 'TRANSITIVIDADE-CATEGORICA)
        (format stream "(adicionar-regra '~a '~s '~s)~%" 
                (rule-name rule) 
                (rule-conditions rule) 
                (rule-consequences rule))))
    
    (format stream "~%(format t \"~&;;; Knowledge Base Loaded Successfully.~%\")~%"))
  (format nil "Estado salvo em ~a" filepath))

(defun carregar-estado (filepath)
  "Loads state from a file by executing it."
  (if (probe-file filepath)
      (handler-case
          (progn
            (load filepath)
            (format nil "Estado carregado de ~a. Atualmente: ~a memorias, ~a relacoes, ~a regras."
                    filepath 
                    (hash-table-count *knowledge-graph*) 
                    (length *relations*)
                    (length *rules*)))
        (error (e) (format nil "Erro ao carregar script: ~a" e)))
      (format nil "Arquivo nao encontrado: ~a" filepath)))

(defun reset-total (filepath)
  "Completely clears RAM and overwrites the persistence file to a clean state."
  (limpar-memoria)
  (salvar-estado filepath)
  (format nil "Resiliencia: Memoria e arquivo ~a resetados com sucesso." filepath))

;;; --- Aliases for Legacy LLM Support ---
(defun lembrar (chave valor)
  (format t "~&[WARN] Deprecated function 'lembrar' called. Redirecting...~%")
  (adicionar-memoria chave valor))
(export 'lembrar)

;;; --- Tooling System ---

(defmacro definir-funcao (nome args &body corpo)
  "Defines a new function and persists its source."
  (let ((def `(defun ,nome ,args (declare (ignorable ,@args)) ,@corpo)))
    `(progn
       (eval ',def)
       (pushnew ',def *custom-definitions* :test #'equal)
       (format nil "Funcao aprendida e persistida: ~a" ',nome))))

(defmacro definir-macro (nome args &body corpo)
  "Defines a new macro and persists its source."
  (let ((def `(defmacro ,nome ,args ,@corpo)))
    `(progn
       (eval ',def)
       (pushnew ',def *custom-definitions* :test #'equal)
       (format nil "Macro aprendida e persistida: ~a" ',nome))))

(defmacro definir-ferramenta (nome args &body corpo)
  "Legacy alias for definir-funcao."
  `(definir-funcao ,nome ,args ,@corpo))

;;; --- Initialize User Package ---
(in-package :cl-user)

;; Use shadowing-import to ensure we win over any cl-user garbage
(shadowing-import '(s-dialectic:adicionar-memoria
                    s-dialectic:recuperar-memoria
                    s-dialectic:listar-memorias
                    s-dialectic:limpar-memoria
                    s-dialectic:buscar-relacoes
                    s-dialectic:adicionar-relacao
                    s-dialectic:listar-relacoes
                    s-dialectic:listar-regras
                    s-dialectic:listar-dados-json
                    s-dialectic:print-graph-json
                    s-dialectic:salvar-estado
                    s-dialectic:carregar-estado
                    s-dialectic:reset-total
                    s-dialectic:adicionar-regra
                    s-dialectic:inferir
                    s-dialectic:carregar-regras-essenciais
                    s-dialectic:lembrar
                    s-dialectic:definir-ferramenta
                    s-dialectic:definir-funcao
                    s-dialectic:definir-macro) :cl-user)

;; Macro para permitir que o LLM use (lisp ...) como wrapper sem erro
(defmacro lisp (&body body)
  `(progn ,@body))

;; Inicialização
(format t "~%SDialectic Cognitive Bootstrap 2.0 Carregado.~%")
(format t "JSON Support: ENABLED. Persistence: ENABLED. Graph: RELATIONAL.~%")

(in-package :s-dialectic)

(defun print-graph-json ()
  "Explicitly prints the graph JSON to stdout to avoid REPL truncation."
  (princ (listar-dados-json))
  (terpri)
  (values))

(in-package :cl-user)
(s-dialectic::carregar-regras-essenciais)
