;;; S-Dialectic Kernel (Minimal Stable Version)
;;; ---------------------------------------------------------------------------
(in-package :cl-user)

(defpackage :s-dialectic
  (:use :cl)
  (:export #:adicionar-memoria
           #:adicionar-relacao
           #:adicionar-regra
           #:recuperar-memoria
           #:listar-memorias
           #:salvar-estado
           #:carregar-estado
           #:snapshot-memoria
           #:limpar-memoria
           #:atualizar-vetor
           #:snapshot-memoria
           #:limpar-memoria
           #:atualizar-vetor
           #:recuperar-foco-semantico
           #:salvar-rapido
           #:carregar-rapido
           #:eval-safe
           #:definir-dialetica
           #:reset-total))

(in-package :s-dialectic)

;;; --- Globals ---
(defparameter *knowledge-graph* (make-hash-table :test #'equal))
(defparameter *relations* nil)
(defparameter *rules* nil)
(defparameter *relations-index* (make-hash-table :test #'equal))

(defstruct concept
  description
  vector
  type)

(defstruct relation
  subject
  predicate
  object
  category
  provenance)

(defstruct rule
  name
  conditions
  implications)

;;; --- Utils ---
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string with all occurrences of part replaced by replacement."
  (with-output-to-string (out)
    (loop with part-len = (length part)
          for old-pos = 0 then (+ pos part-len)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun normalizar-termo (termo)
  (string-trim " " (string-upcase (format nil "~a" termo))))

;;; --- SECTION-BARRIER ---
;;; --- Core Logic ---

(defun tenta-avaliar-matematica (expr-str)
  "Tenta avaliar expressao matematica segura. Retorna numero ou nil."
  (handler-case 
      (let ((sexp (read-from-string expr-str nil)))
        (if (and (listp sexp)
                 (member (first sexp) '(+ - * /) :test #'eq))
            (eval sexp)
            (if (numberp sexp) sexp nil)))
    (error () nil)))

(defun adicionar-memoria (chave valor &key vector)
  (let ((k (normalizar-termo chave)))
    (setf (gethash k *knowledge-graph*) 
          (make-concept :description valor :vector vector))
    
    (when vector
      (setf *vector-cache* (delete k *vector-cache* :key #'car :test #'string=))
      (push (cons k vector) *vector-cache*))
      
    (format nil "Memoria: ~a" k)))

(defparameter *vector-cache* nil) ;; List of (key . vector-ref) tuples

(defun atualizar-vetor (chave vetor)
  "Atualiza o vetor de embedding e mantem um cache para busca rapida."
  (let* ((k (normalizar-termo chave))
         (existing (gethash k *knowledge-graph*)))
    (if existing
        (if (concept-p existing)
            (setf (concept-vector existing) vetor)
            (setf (gethash k *knowledge-graph*) 
                  (make-concept :description existing :vector vetor)))
        (setf (gethash k *knowledge-graph*) 
              (make-concept :description "Stub vetorial" :vector vetor)))
    
    ;; Update Cache (Remove old entry if exists, push new)
    (setf *vector-cache* (delete k *vector-cache* :key #'car :test #'string=))
    (push (cons k vetor) *vector-cache*)
    
    (format nil "Vetor atualizado para ~a" k)))

(defun adicionar-relacao (sujeito predicado objeto &rest extra-args)
  (let ((s (normalizar-termo sujeito))
        (p (normalizar-termo predicado))
        (o (normalizar-termo objeto))
        (cat :generic))
    
    (loop for (key val) on extra-args by #'cddr
          do (case key
               (:category (setf cat (if (stringp val) (intern (string-upcase val) :keyword) val)))
               (t (ignore-errors))))
    
    ;; Math Axiom Check
    (when (member (string p) '("IS_EQUIVALENT_TO" "EQUALS" "=" "SAME_AS") :test #'string-equal)
      (let ((val-s (tenta-avaliar-matematica (string s)))
            (val-o (tenta-avaliar-matematica (string o))))
        (when (and (numberp val-s) (numberp val-o))
          (unless (= val-s val-o)
             (error "VIOLACAO AXIOMATICA: ~a != ~a" val-s val-o)))))

    (unless (gethash (string s) *knowledge-graph*)
      (setf (gethash (string s) *knowledge-graph*) "Conceito Implicito"))
    (unless (gethash (string o) *knowledge-graph*)
      (setf (gethash (string o) *knowledge-graph*) "Conceito Implicito"))
      

    (let ((key (format nil "~a|~a|~a" s p o)))
      (unless (gethash key *relations-index*)
        (setf (gethash key *relations-index*) t)
        (push (make-relation :subject s :predicate p :object o :category cat) *relations*)
        (format nil "Relacao adicionada: ~a -[~a]-> ~a" s p o)))))

(defun adicionar-regra (nome condicoes implicacoes)
  (let ((r (make-rule :name nome :conditions condicoes :implications implicacoes)))
    (push r *rules*)
    (push r *rules*)
    (format nil "Regra adicionada: ~a" nome)))

;;; --- Math / Semantics ---

(defun magnitude (v)
  (if (or (null v) (= (length v) 0)) 0.0
      (sqrt (reduce #'+ (map 'vector (lambda (x) (* x x)) v)))))

(defun cosine-similarity (v1 v2)
  (if (or (null v1) (null v2)) 0.0
      (let ((mag1 (magnitude v1))
            (mag2 (magnitude v2)))
        (if (or (zerop mag1) (zerop mag2)) 0.0
            (let ((dot (reduce #'+ (map 'vector #'* v1 v2))))
              (/ dot (* mag1 mag2)))))))

(defun recuperar-foco-semantico (chave &key (limit 5) (threshold 0.4))
  "Retorna os N conceitos mais similares usando o *vector-cache*."
  (let* ((k (normalizar-termo chave))
         (concept (gethash k *knowledge-graph*))
         (target-vec (if (concept-p concept) (concept-vector concept) nil))
         (results nil))
    (if target-vec
        (progn
          ;; Iterate cached list instead of hash table
          (dolist (pair *vector-cache*)
             (let ((other-k (car pair))
                   (other-vec (cdr pair)))
               (unless (string= k other-k)
                 (let ((sim (cosine-similarity target-vec other-vec)))
                   (when (> sim threshold)
                     (push (cons other-k sim) results))))))
                     
          ;; Sort by similarity desc
          (setf results (sort results #'> :key #'cdr))
          ;; Take top N
          (mapcar #'car (subseq results 0 (min (length results) limit))))
        (list "NIL (Sem Vetor)"))))
        (list "NIL (Sem Vetor)"))))

;;; --- SECTION-BARRIER ---
;;; --- Persistence ---

(defun salvar-estado (filepath)
  (with-open-file (stream filepath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(in-package :s-dialectic)~%")
    ;; Persist Rules first (Convention)
    (dolist (rule (reverse *rules*))
      (format stream "(adicionar-regra '~a '~s '~s)~%"
              (rule-name rule) (rule-conditions rule) (rule-implications rule)))
    
    (maphash (lambda (k v)
               (if (concept-p v)
                   (format stream "(adicionar-memoria ~s ~s :vector ~a)~%" k (concept-description v) (concept-vector v))
                   (format stream "(adicionar-memoria ~s ~s)~%" k v)))
             *knowledge-graph*)
    (dolist (r (reverse *relations*))
      (format stream "(adicionar-relacao ~s ~s ~s :category ~s)~%" 
              (relation-subject r) (relation-predicate r) (relation-object r) (relation-category r)))
    (format nil "Estado salvo em ~a" filepath)))

(defun carregar-estado (filepath)
  (load filepath)
  (format nil "Estado carregado de ~a" filepath))

;;; --- Metaprogramming / Sandbox ---

(defun eval-safe (expr)
  "Avalia uma expressao Lisp capturando erros. Retorna (values result error-message)."
  (handler-case 
      (values (eval expr) nil)
    (error (e) (values nil (format nil "Erro na avaliacao: ~a" e)))))

(defmacro definir-dialetica (nome args &body body)
  "Define uma nova funcao ou macro dialetica dinamicamente.
   Uso: (definir-dialetica verificar-conflito (a b) ...)"
  `(progn
     (defun ,nome ,args ,@body)
     (export ',nome)
     (format nil "Dialetica definida: ~a" ',nome)))

;;; --- Fast Persistence (Structure IO) ---

(defun hash-to-alist (ht)
  (let ((lst nil))
    (maphash (lambda (k v) (push (cons k v) lst)) ht)
    lst))

(defun alist-to-hash (lst)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair lst)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defun salvar-rapido (filepath)
  "Salva o estado interno imprimindo as estruturas de dados diretamente."
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((dados (list :graph (hash-to-alist *knowledge-graph*)
                         :relations *relations*
                         :rules *rules*)))
        (print dados stream))))
  (format nil "Fast Save em ~a" filepath))

(defun carregar-rapido (filepath)
  "Carrega o estado lendo as estruturas de dados."
  (with-open-file (stream filepath :direction :input)
    (let ((dados (read stream)))
      (setf *knowledge-graph* (alist-to-hash (getf dados :graph)))
      (setf *relations* (getf dados :relations))
      (setf *rules* (getf dados :rules))
      ;; Rebuild index
      (clrhash *relations-index*)
      (dolist (r *relations*)
        (let ((k (format nil "~a|~a|~a" (relation-subject r) (relation-predicate r) (relation-object r))))
          (setf (gethash k *relations-index*) t)))
          
      ;; Rebuild Vector Cache
      (setf *vector-cache* nil)
      (maphash (lambda (k v)
                 (when (and (concept-p v) (concept-vector v))
                   (push (cons k (concept-vector v)) *vector-cache*)))
               *knowledge-graph*)))
  (format nil "Fast Load de ~a" filepath))

;;; Snapshot logic required by GraphEngine
(defparameter *snapshot-mem-keys* nil)
(defparameter *snapshot-rel-count* 0)

(defun snapshot-memoria ()
  (setf *snapshot-mem-keys* (make-hash-table :test #'equal))
  (maphash (lambda (k v) 
             (declare (ignore v))
             (setf (gethash k *snapshot-mem-keys*) t)) 
           *knowledge-graph*)
  (setf *snapshot-rel-count* (length *relations*))
  (format nil "Snapshot Taken: ~a keys" (hash-table-count *snapshot-mem-keys*)))


(defun limpar-memoria ()
  (clrhash *knowledge-graph*)
  (setf *relations* nil)
  (clrhash *relations-index*)
  "Memoria limpa.")

(defun reset-total ()
  (limpar-memoria)
  "Estado total resetado.")

(defun recuperar-foco-atencional (termos-chave &key (limit-neighbors 20))
  "Retorna uma lista de strings representando relacoes conectadas aos termos-chave."
  (declare (ignorable limit-neighbors))
  (let ((termos-norm (mapcar #'normalizar-termo termos-chave))
        (resultado nil)
        (contagem 0))
    (dolist (r *relations*)
      (when (< contagem limit-neighbors) 
        (let ((s (relation-subject r))
              (o (relation-object r)))
          (when (or (member s termos-norm :test #'string-equal)
                    (member o termos-norm :test #'string-equal))
            (push (format nil "(~s ~s ~s)" s (relation-predicate r) o) resultado)
            (incf contagem)))))
    (if resultado
        resultado
        (list "NIL"))))


;;; --- JSON Export Helpers ---

(defun escape-json-string (str)
  (if (stringp str)
      (with-output-to-string (out)
        (loop for char across str do
              (case char
                (#\" (write-string "\\\"" out))
                (#\\ (write-string "\\\\" out))
                (#\Newline (write-string "\\n" out))
                (#\Return (write-string "\\r" out))
                (#\Tab (write-string "\\t" out))
                (t (write-char char out)))))
      ""))

(defun listar-dados-json ()
  (let ((concepts nil)
        (rels nil))
    (maphash (lambda (k v)
               (let ((desc (if (concept-p v) (concept-description v) v)))
                 (push (format nil "{\"key\":\"~a\",\"type\":\"concept\",\"value\":\"~a\"}" 
                               (escape-json-string k)
                               (escape-json-string (format nil "~a" desc))) 
                       concepts)))
             *knowledge-graph*)
    (dolist (r *relations*)
      (push (format nil "{\"source\":\"~a\",\"target\":\"~a\",\"relation\":\"~a\",\"category\":\"~a\"}"
                    (escape-json-string (relation-subject r))
                    (escape-json-string (relation-object r))
                    (escape-json-string (relation-predicate r))
                    (escape-json-string (format nil "~a" (relation-category r))))
            rels))
    (format nil "{\"nodes\":[~{~a~^,~}],\"edges\":[~{~a~^,~}]}" concepts rels)))

;;; --- Finalize ---
(format t "~&[Bootstrap] Minimal Kernel Ready + JSON Support.~%")
(values)
