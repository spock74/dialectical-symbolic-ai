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
    ;; Validação Axiomática (Novo)
    :validar-axioma
    
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
  (properties nil :type list)
  (description "" :type string)
  (vector nil :type (or null vector)))

(defstruct relation
  (subject nil :type symbol)
  (predicate nil :type symbol)
  (object nil :type symbol)
  (certainty 1.0f0 :type float)

  (provenance :user :type symbol)
  (category :generic :type symbol)) ; e.g., :CAUSAL, :HIERARCHY

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

(defun to-json-triple (rel)
  (let ((sub (relation-subject rel))
        (obj (relation-object rel))
        (pred (relation-predicate rel))
        (prov (relation-provenance rel))
        (cat (relation-category rel)))
    (format nil "{\"source\": \"~a\", \"target\": \"~a\", \"relation\": \"~a\", \"provenance\": \"~a\", \"category\": \"~a\"}"
            (escape-json-string sub)
            (escape-json-string obj)
            (escape-json-string pred)
            (escape-json-string prov)
            (escape-json-string cat))))

;;; --- Math Helpers (Topologia) ---

(defun dot-product (v1 v2)
  (let ((sum 0.0))
    (loop for i from 0 below (length v1)
          do (incf sum (* (aref v1 i) (aref v2 i))))
    sum))

(defun magnitude (v)
  (sqrt (dot-product v v)))

(defun cosine-similarity (v1 v2)
  (if (or (null v1) (null v2) 
          (/= (length v1) (length v2)) 
          (zerop (magnitude v1)) (zerop (magnitude v2)))
      0.0
      (/ (dot-product v1 v2) (* (magnitude v1) (magnitude v2)))))

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

(defun normalizar-condicao (cond)
  (cond
    ((eq (first cond) 'similar-p)
     (list 'similar-p (second cond) (third cond) (fourth cond)))
    ((eq (first cond) 'not)
     (list 'not (normalizar-condicao (second cond))))
    (t (normalizar-tripla cond))))

;; ================================================================
;; Regra de Proteção A-03: Imutabilidade Matemática

(defun validar-axioma (expressao valor-proposto)
  (let ((valor-real (handler-case (eval (read-from-string expressao))
                      (error () nil))))
    (if (and (numberp valor-real) 
             (numberp valor-proposto))
        (if (= valor-real valor-proposto)
            t
            (error "VIOLAÇÃO AXIOMÁTICA: A matemática é imutável. ~a não é ~a." 
                   expressao valor-proposto))
        t)))

;;; --- Funções de Memória (Nodes) ---

(defun adicionar-memoria (chave valor &key vector)
  (let* ((k (string-upcase (string chave)))
         (v-str (string valor))
         (existing (gethash k *knowledge-graph*)))
    (if (concept-p existing)
        (progn
          (setf (concept-description existing) v-str)
          (when vector (setf (concept-vector existing) vector)))
        (setf (gethash k *knowledge-graph*)
              (make-concept :name (intern k :s-dialectic)
                            :description v-str
                            :vector vector)))
    (format nil "Memorizado: ~a" k)))

(defun atualizar-vetor (chave vector)
  (let* ((k (string-upcase (string chave)))
         (existing (gethash k *knowledge-graph*)))
    (if (concept-p existing)
        (setf (concept-vector existing) vector)
        (setf (gethash k *knowledge-graph*)
              (make-concept :name (intern k :s-dialectic)
                            :description "Vectorized Node"
                            :vector vector)))
    (format nil "Vetor atualizado para: ~a" k)))

(defun recuperar-memoria (chave)
  (let ((node (gethash (string-upcase (string chave)) *knowledge-graph*)))
    (cond
      ((concept-p node) (concept-description node))
      ((stringp node) node)
      (t "NIL"))))

(defun buscar-conceito (chave)
  (gethash (string-upcase (string chave)) *knowledge-graph*))

(defun buscar-proximos (target-vector &key (threshold 0.7) (limit 5))
  (let ((candidates nil))
    (maphash (lambda (k node)
               (declare (ignore k))
               (when (and (concept-p node) (concept-vector node))
                 (let ((sim (cosine-similarity target-vector (concept-vector node))))
                   (when (>= sim threshold)
                     (push (cons sim node) candidates)))))
             *knowledge-graph*)
    (let ((sorted (sort candidates #'> :key #'car)))
      (subseq sorted 0 (min (length sorted) limit)))))

(defun buscar-similares (chave &key (threshold 0.7) (limit 5))
  (let ((node (gethash (string-upcase (string chave)) *knowledge-graph*)))
    (if (and (concept-p node) (concept-vector node))
        (let ((results (buscar-proximos (concept-vector node) :threshold threshold :limit limit)))
          (mapcar (lambda (pair)
                    (list (concept-name (cdr pair)) (car pair)))
                  results))
        "NIL (Sem vetor ou nao encontrado)")))

(defun check-similarity (val1 val2 threshold)
  (let ((k1 (string-upcase (string val1)))
        (k2 (string-upcase (string val2))))
    (let ((n1 (gethash k1 *knowledge-graph*))
          (n2 (gethash k2 *knowledge-graph*)))
      (if (and (concept-p n1) (concept-vector n1)
               (concept-p n2) (concept-vector n2))
          (>= (cosine-similarity (concept-vector n1) (concept-vector n2)) threshold)
          nil))))

(defun listar-memorias (&rest args)
  (declare (ignore args))
  (let ((result nil))
    (maphash (lambda (k v) 
               (let ((desc (if (concept-p v) (concept-description v) v)))
                 (push (list k desc) result))) 
             *knowledge-graph*)
    result))

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
                   (dolist (cons-pattern (rule-consequences rule))
                     (let ((new-s (subst-bindings (first cons-pattern) current-bindings))
                           (new-p (subst-bindings (second cons-pattern) current-bindings))
                           (new-o (subst-bindings (third cons-pattern) current-bindings)))
                       (unless (find-if (lambda (f) (and (eq (relation-subject f) new-s)
                                                         (eq (relation-predicate f) new-p)
                                                         (eq (relation-object f) new-o)))
                                         facts)
                         (push (make-relation :subject new-s :predicate new-p :object new-o :provenance :inference) new-facts))))
                   (let ((condition (first remaining-conditions)))
                     (if (eq (first condition) 'similar-p)
                         (let* ((var (second condition))
                                (term (third condition))
                                (thresh (fourth condition))
                                (val (subst-bindings var current-bindings)))
                           (if (symbolp val)
                               (maphash (lambda (k node) 
                                          (declare (ignore k))
                                          (when (and (concept-p node) 
                                                     (check-similarity (concept-name node) term thresh))
                                            (let ((new-bindings (unify var (concept-name node) current-bindings)))
                                              (when new-bindings
                                                (find-matches (rest remaining-conditions) new-bindings)))))
                                        *knowledge-graph*)
                               (when (check-similarity val term thresh)
                                 (find-matches (rest remaining-conditions) current-bindings))))
                         (if (eq (first condition) 'not)
                             (let ((inner (second condition)))
                               (cond
                                 ((eq (first inner) 'similar-p)
                                  (let* ((var (second inner))
                                         (term (third inner))
                                         (thresh (fourth inner))
                                         (val (subst-bindings var current-bindings)))
                                    (when (or (variable-p val) (not (check-similarity val term thresh)))
                                      (when (not (variable-p val))
                                        (find-matches (rest remaining-conditions) current-bindings)))))
                                 (t
                                  (let ((matches-found nil))
                                    (dolist (fact facts)
                                      (when (match-pattern inner fact current-bindings)
                                        (setf matches-found t)))
                                    (unless matches-found
                                      (find-matches (rest remaining-conditions) current-bindings))))))
                             (dolist (fact facts)
                               (let ((new-bindings (match-pattern condition fact current-bindings)))
                                 (when new-bindings
                                   (find-matches (rest remaining-conditions) new-bindings))))))))))
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
        (conds (mapcar #'normalizar-condicao condicoes))
        (consq (mapcar #'normalizar-tripla consequencias)))
    (push (make-rule :name n :conditions conds :consequences consq) *rules*)
    (format nil "Regra ~a aprendida." n)))

(defun carregar-regras-essenciais ()
  (adicionar-regra 'TRANSITIVIDADE
                   '((?x "e_um" ?y) (?y "e_um" ?z))
                   '((?x "e_um" ?z)))
  "Regras base carregadas.")

(defun limpar-memoria ()
  (clrhash *knowledge-graph*)
  (setf *relations* nil)
  (setf *rules* nil)
  (setf *custom-definitions* nil)
  (carregar-regras-essenciais)
  "Memoria limpa.")

;;; --- Funções de Relação (Edges) ---

(defun tenta-avaliar-matematica (str)
  "Tenta converter string em numero ou executar expressao matematica."
  (handler-case 
      (let ((val (read-from-string str)))
        (cond 
          ((numberp val) val)
          ((listp val) (eval val))
          (t nil)))
    (error () nil)))

(defun adicionar-relacao (sujeito predicado objeto &rest extra-args)
  (let ((s (normalizar-termo sujeito))
        (p (normalizar-termo predicado))
        (o (normalizar-termo objeto))
        (cat :generic))
    (loop for (key val) on extra-args by #'cddr
          do (case key
               (:category (setf cat (if (stringp val) (intern (string-upcase val) :keyword) val)))
               (t (ignore-errors))))
    (when (member (string p) '("IS_EQUIVALENT_TO" "EQUALS" "=" "SAME_AS") :test #'string-equal)
      (let ((val-s (tenta-avaliar-matematica (string s)))
            (val-o (tenta-avaliar-matematica (string o))))
        (when (and (numberp val-s) (numberp val-o))
          (unless (= val-s val-o)
             (error "VIOLAÇÃO AXIOMÁTICA: O Kernel Lisp recusa a relação ~a = ~a. Na minha realidade, ~a != ~a."
                    s o val-s val-o)))))
    (progn
      (unless (gethash (string s) *knowledge-graph*)
        (setf (gethash (string s) *knowledge-graph*) "Conceito Implicito"))
      (unless (gethash (string o) *knowledge-graph*)
        (setf (gethash (string o) *knowledge-graph*) "Conceito Implicito"))
      (unless (find-if (lambda (r) 
                     (and (eq (relation-subject r) s)
                          (eq (relation-predicate r) p)
                          (eq (relation-object r) o)))
                   *relations*)
          (push (make-relation :subject s :predicate p :object o :provenance :user :category cat) *relations*)
          (format nil "Relacao adicionada: ~a -[~a (~a)]-> ~a" s p cat o)))))

(defun buscar-relacoes (conceito)
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
  (let ((mem-strings nil)
        (rel-strings nil))
    (maphash (lambda (k v) 
               (let ((desc (if (concept-p v) (concept-description v) v)))
                 (push (to-json-pair k desc) mem-strings))) 
             *knowledge-graph*)
    (dolist (r *relations*)
      (push (to-json-triple r) rel-strings))
    (format nil "{ \"nodes\": [~{~a~^, ~}], \"edges\": [~{~a~^, ~}] }"
            mem-strings
            rel-strings)))

(defun print-graph-json ()
  (princ (listar-dados-json))
  (values))

;;; --- Persistência ---

(defun salvar-estado (filepath)
  (with-open-file (stream filepath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(in-package :s-dialectic)~%")
    (format stream "(limpar-memoria)~%~%")
    (dolist (def (reverse *custom-definitions*)) (format stream "~s~%~%" def))
    (maphash (lambda (k v) 
               (if (concept-p v)
                   (format stream "(adicionar-memoria ~s ~s :vector ~s)~%" k (concept-description v) (concept-vector v))
                   (format stream "(adicionar-memoria ~s ~s)~%" k v))) 
             *knowledge-graph*)
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

(defmacro definir-regra (nome conds consqs)
  `(adicionar-regra ,nome ,conds ,consqs))

;;; --- META-LEARNING: GERAÇÃO DE REGRAS IMUNOLÓGICAS ---

(defmacro aprender-lei-proibitiva (nome classe-perigosa vitima-protegida threshold)
  "Cria uma regra que bloqueia interação baseada em similaridade vetorial."
  (let ((rule-name (intern (string-upcase (format nil "LEI-AUTO-~a" nome)) :s-dialectic)))
    `(adicionar-regra ',rule-name
       ;; CONDIÇÕES:
       ;; 1. Existe uma vítima (ex: Grávida)
       '((?vitima "TEM_ESTADO" ,vitima-protegida)
         ;; 2. Existe um agente que é SIMILAR à classe perigosa (ex: Teratogênico)
         ;; O 'similar-p' usa os embeddings para generalizar!
         (similar-p ?agente ,classe-perigosa ,threshold))
       
       ;; CONSEQUÊNCIA (BLOQUEIO):
       ;; O sistema aprende que a relação é proibida
       '((not (?vitima "PODE_INTERAGIR" ?agente))))))

;; Exemplo de uso (O que o LLM vai gerar):
;; (aprender-lei-proibitiva "TERATOGENIA" "TERATOGENICO" "GRAVIDA" 0.85)

;;; --- Inicialização Final ---

(carregar-regras-essenciais)

(format t "~&[Bootstrap] Stage 3 Complete. SDialectic Kernel Ready.~%")
(values)
