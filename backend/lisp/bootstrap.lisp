;;; ============================================================================
;;; SDialectic: Neuro-Symbolic Cognitive Bootstrap (SBCL Native Version)
;;; ============================================================================
;;; Este arquivo é carregado em estágios pelo 'sbcl-process.ts'
;;; Não remova os marcadores ';;; --- SECTION-BARRIER ---'

;;; ----------------------------------------------------------------------------
;;; ESTÁGIO 1: DEFINIÇÃO DE PACOTES E AMBIENTE
;;; ----------------------------------------------------------------------------

(in-package :cl-user)

(format t "~&[Bootstrap] Stage 1: Initializing Packages...~%")

(setf *debugger-hook*
      (lambda (condition hook)
        (declare (ignore hook))
        (format t "LISP-ERROR: ~a~%" condition)
        (finish-output)
        (abort)))

;;; --- Configurações de Ambiente ---
;; Configurações de Impressão para evitar truncamento de JSON
(setf *print-length* nil)
(setf *print-level* nil)
(setf *print-lines* nil)
(setf *print-base* 10)
(setf *print-case* :upcase)

;; Definição do Pacote Principal
(defpackage :s-dialectic
  (:use :cl)
  (:export 
    ;; Memória
    :adicionar-memoria
    :recuperar-memoria
    :listar-memorias
    :limpar-memoria
    
    ;; Grafo & Relações
    :adicionar-relacao
    :buscar-relacoes
    :listar-relacoes
    :listar-dados-json
    
    ;; Inferência & Regras
    :adicionar-regra
    :inferir
    :listar-regras
    :carregar-regras-essenciais
    
    ;; Persistência & Controle
    :salvar-estado
    :carregar-estado
    :reset-total
    
    ;; Metaprogramação (Ferramentas)
    :definir-ferramenta
    :definir-funcao
    :definir-macro))

;; Entrar no pacote
(in-package :s-dialectic)

(format t "[Bootstrap] Package :S-DIALECTIC defined.~%")

;;; --- SECTION-BARRIER ---
;;; ----------------------------------------------------------------------------
;;; ESTÁGIO 2: ESTRUTURAS DE DADOS E VARIÁVEIS GLOBAIS
;;; ----------------------------------------------------------------------------
;;; O compilador precisa que estas estruturas existam antes de compilar as funções

(format t "~&[Bootstrap] Stage 2: Defining Structs and Variables...~%")

;; 1. Estruturas (Schema)
(defstruct concept
  (name nil :type symbol)
  (type :concept :type symbol)
  (properties nil :type list))

(defstruct relation
  (subject nil :type symbol)
  (predicate nil :type symbol)
  (object nil :type symbol)
  (certainty 1.0f0 :type float)
  (provenance :user :type symbol)) ; :user ou :inference

(defstruct rule
  (name nil :type symbol)
  (conditions nil :type list)   ; Lista de triplas padrão: ((?x "e_um" ?y))
  (consequences nil :type list)) ; Lista de triplas resultado

;; 2. Variáveis Globais (Estado)

(defvar *knowledge-graph* (make-hash-table :test 'equal)
  "Armazena os nós (conceitos) e suas descrições")

(defvar *relations* nil
  "Lista linear de todas as relações (arestas) do sistema.")

(defvar *rules* nil
  "Base de conhecimento de regras lógicas.")

(defvar *custom-definitions* nil
  "Armazena código fonte de macros/funções criadas pelo LLM para persistência.")

(format t "[Bootstrap] Memory structures allocated.~%")

;;; --- SECTION-BARRIER ---
;;; ----------------------------------------------------------------------------
;;; ESTÁGIO 3: LÓGICA CORE E FUNÇÕES
;;; ----------------------------------------------------------------------------

(format t "~&[Bootstrap] Stage 3: Loading Logic Kernel...~%")

;;; --- Helpers de JSON (Dependency-Free) ---

(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string :start2 old-pos :test test)
          do (write-string string out :start old-pos :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun escape-json-string (str)
  "Sanitiza strings para JSON válido."
  (let ((s (format nil "~a" str))) 
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

;;; --- Normalização Lógica ---

(defun variable-p (x)
  "Verifica se é uma variável lógica (começa com ?)."
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defun normalizar-termo (x)
  "Converte strings ou símbolos soltos para símbolos do pacote S-DIALECTIC."
  (cond
    ((symbolp x) (intern (string-upcase (symbol-name x)) :s-dialectic))
    ((stringp x) (intern (string-upcase x) :s-dialectic))
    (t x)))

(defun normalizar-tripla (tripla)
  (mapcar #'normalizar-termo tripla))

;;; --- Funções de Memória (Nodes) ---

(defun adicionar-memoria (chave valor)
  "Adiciona ou atualiza um nó no grafo."
  (let* ((k (string-upcase (string chave)))
         (v (string valor)))
    (setf (gethash k *knowledge-graph*) v)
    (format nil "Memorizado: ~a" k)))

(defun recuperar-memoria (chave)
  (let ((val (gethash (string-upcase (string chave)) *knowledge-graph*)))
    (if val val "NIL")))

(defun listar-memorias (&rest args)
  (declare (ignore args))
  (let ((result nil))
    (maphash (lambda (k v) (push (list k v) result)) *knowledge-graph*)
    result))


(defun limpar-memoria ()
  (clrhash *knowledge-graph*)
  (setf *relations* nil)
  (setf *rules* nil)
  (setf *custom-definitions* nil)
  (carregar-regras-essenciais)
  "Memoria limpa.")

;;; --- Funções de Relação (Edges) ---

(defun adicionar-relacao (sujeito predicado objeto &rest extra-args)
  "Adiciona uma aresta ao grafo, checando duplicatas."
  (let ((s (normalizar-termo sujeito))
        (p (normalizar-termo predicado))
        (o (normalizar-termo objeto)))
    
    ;; Se houver argumentos extras, podemos logar ou ignorar. 
    ;; Aqui vamos apenas prosseguir com os 3 primeiros para evitar quebra.
    (unless (null extra-args)
       (format t "AVISO: adicionar-relacao recebeu argumentos extras e foram ignorados: ~a~%" extra-args))

    ;; Verifica duplicidade (O(N) - Pode ser otimizado futuramente)
    (if (find-if (lambda (r) 
                   (and (eq (relation-subject r) s)
                        (eq (relation-predicate r) p)
                        (eq (relation-object r) o)))
                 *relations*)
        (format nil "AVISO: Relacao ~a-~a-~a ja existe." s p o)
        (progn
          ;; Auto-Discovery: Garante que nós existam
          (unless (gethash (string s) *knowledge-graph*)
            (setf (gethash (string s) *knowledge-graph*) "Conceito Implicito"))
          (unless (gethash (string o) *knowledge-graph*)
            (setf (gethash (string o) *knowledge-graph*) "Conceito Implicito"))
          
          (push (make-relation :subject s :predicate p :object o :provenance :user) *relations*)
          (format nil "Relacao adicionada: ~a -[~a]-> ~a" s p o)))))

(defun buscar-relacoes (conceito)
  "Encontra todas as arestas conectadas a um conceito."
  (let* ((c (normalizar-termo conceito))
         (found (remove-if-not (lambda (r) 
                                 (or (eq (relation-subject r) c)
                                     (eq (relation-object r) c)))
                               *relations*)))
    (if found
        (mapcar (lambda (r) 
                  (format nil "(~a ~a ~a)" 
                          (relation-subject r) 
                          (relation-predicate r) 
                          (relation-object r))) 
                found)
        (format nil "Nenhuma relacao para: ~a" conceito))))

(defun listar-relacoes (&rest args)
  (declare (ignore args))
  (mapcar (lambda (r) 
            (format nil "(~a ~a ~a)" 
                    (relation-subject r) 
                    (relation-predicate r) 
                    (relation-object r))) 
          *relations*))

;;; --- Exportação JSON (Para Frontend ReactFlow) ---

(defun listar-dados-json ()
  "Serializa todo o grafo para JSON."
  (let ((mem-strings nil)
        (rel-strings nil))
    ;; Nós
    (maphash (lambda (k v) (push (to-json-pair k v) mem-strings)) *knowledge-graph*)
    ;; Arestas
    (dolist (r *relations*)
      (push (to-json-triple (relation-subject r) (relation-predicate r) (relation-object r) (relation-provenance r)) rel-strings))
    
    (format nil "{ \"nodes\": [~{~a~^, ~}], \"edges\": [~{~a~^, ~}] }"
            mem-strings
            rel-strings)))

(defun print-graph-json ()
  (princ (listar-dados-json))
  (values))

;;; --- Motor de Inferência (Forward Chaining) ---

(defun unify (var val bindings)
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

(defun apply-rule (rule facts)
  (let ((new-facts nil))
    (labels ((find-matches (remaining-conditions current-bindings)
               (if (null remaining-conditions)
                   ;; Gerar consequências
                   (dolist (cons-pattern (rule-consequences rule))
                     (let ((new-s (subst-bindings (first cons-pattern) current-bindings))
                           (new-p (subst-bindings (second cons-pattern) current-bindings))
                           (new-o (subst-bindings (third cons-pattern) current-bindings)))
                       (unless (find-if (lambda (f) (and (eq (relation-subject f) new-s)
                                                         (eq (relation-predicate f) new-p)
                                                         (eq (relation-object f) new-o)))
                                        facts)
                         (push (make-relation :subject new-s :predicate new-p :object new-o :provenance :inference) new-facts))))
                   ;; Continuar buscando matches
                   (let ((condition (first remaining-conditions)))
                     (dolist (fact facts)
                       (let ((new-bindings (match-pattern condition fact current-bindings)))
                         (when new-bindings
                           (find-matches (rest remaining-conditions) new-bindings))))))))
      (find-matches (rule-conditions rule) nil))
    new-facts))

(defun inferir ()
  (let ((derived-count 0) (iteration 0) (max-iterations 10))
    (loop
      (incf iteration)
      (let ((new-this-round nil))
        (dolist (rule *rules*)
          (let ((inferred (apply-rule rule *relations*)))
            (dolist (f inferred)
              (unless (find-if (lambda (r) (and (eq (relation-subject r) (relation-subject f))
                                                (eq (relation-predicate r) (relation-predicate f))
                                                (eq (relation-object r) (relation-object f))))
                               *relations*)
                (push f new-this-round)
                (push f *relations*)))))
        
        (when (null new-this-round) (return))
        (when (> iteration max-iterations) (return))
        (setf derived-count (+ derived-count (length new-this-round)))))
    (format nil "Inferencia: ~a novos fatos derivados." derived-count)))

(defun adicionar-regra (nome condicoes consequencias)
  (let ((n (normalizar-termo nome))
        (conds (mapcar #'normalizar-tripla condicoes))
        (consq (mapcar #'normalizar-tripla consequencias)))
    (push (make-rule :name n :conditions conds :consequences consq) *rules*)
    (format nil "Regra ~a aprendida." n)))

(defun carregar-regras-essenciais ()
  (adicionar-regra 'TRANSITIVIDADE
                   '((?x "e_um" ?y) (?y "e_um" ?z))
                   '((?x "e_um" ?z)))
  "Regras base carregadas.")


(defun listar-regras (&rest args)
  (declare (ignore args))
  (mapcar #'rule-name *rules*))

;;; --- Persistência ---

(defun salvar-estado (filepath)
  (with-open-file (stream filepath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(in-package :s-dialectic)~%")
    (format stream "(limpar-memoria)~%~%")
    (dolist (def (reverse *custom-definitions*)) (format stream "~s~%~%" def))
    (maphash (lambda (k v) (format stream "(adicionar-memoria ~s ~s)~%" k v)) *knowledge-graph*)
    (dolist (r (reverse *relations*))
      (format stream "(adicionar-relacao ~s ~s ~s)~%" (string (relation-subject r)) (string (relation-predicate r)) (string (relation-object r))))
    (format stream "~%"))
  (format nil "Salvo em ~a" filepath))

(defun carregar-estado (filepath)
  (if (probe-file filepath)
      (progn (load filepath) (format nil "Carregado de ~a" filepath))
      (format nil "Arquivo nao encontrado: ~a" filepath)))

(defun reset-total (filepath)
  (limpar-memoria)
  (salvar-estado filepath)
  "Estado resetado.")

;;; --- Metaprogramação (Ferramentas) ---

(defmacro definir-funcao (nome args &body corpo)
  (let ((def `(defun ,nome ,args (declare (ignorable ,@args)) ,@corpo)))
    `(progn
       (eval ',def)
       (pushnew ',def *custom-definitions* :test #'equal)
       (format nil "Funcao definida: ~a" ',nome))))

(defmacro definir-macro (nome args &body corpo)
  (let ((def `(defmacro ,nome ,args ,@corpo)))
    `(progn
       (eval ',def)
       (pushnew ',def *custom-definitions* :test #'equal)
       (format nil "Macro definida: ~a" ',nome))))

(defmacro definir-ferramenta (nome args &body corpo)
  `(definir-funcao ,nome ,args ,@corpo))

;;; --- Inicialização Final ---

;; Carregar regras iniciais
(carregar-regras-essenciais)

(format t "~&[Bootstrap] Stage 3 Complete. SDialectic Kernel Ready.~%")
(values)
