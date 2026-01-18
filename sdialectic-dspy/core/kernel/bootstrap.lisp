;;; SDIALECTIC KERNEL v2.0
;;; Axiomatic Validation Layer
;;; Running on SBCL 2.6.0

(defpackage :sdialectic-kernel
  (:use :cl)
  (:export :validate-fact :check-consistency))

(in-package :sdialectic-kernel)

;; --- AXIOMS ---

(defvar *forbidden-patterns*
  '((contraindicated pregnancy isotretinoin)
    (eat poison)
    (equals 1 1 3)) ;; Math hallucination trap
  "Patterns that strictly violate safety rules.")

;; --- VALIDATION LOGIC ---

(defun validate-s-expression (sexpr)
  "Validates syntax and checks against axioms."
  (handler-case
      (let ((result (eval sexpr)))
        (format t "VALID: ~a~%" result)
        t)
    (error (c)
      (format t "ERROR: ~a~%" c)
      nil)))

(defun check-fact (fact)
  "Checks a single fact."
  (if (member fact *forbidden-patterns* :test #'equal)
      (format nil "VIOLATION: ~a" fact)
      nil))

(defun validate-batch (input)
  "Validates a list of facts. Returns SAFE only if ALL are safe."
  ;; Determine if input is a single fact or a list of facts
  (let ((facts (if (listp (car input)) input (list input))))
    (let ((violations (remove-if #'null (mapcar #'check-fact facts))))
      (if violations
          (format t "~{~a~^; ~}~%" violations)
          (format t "SAFE: All ~a facts validated.~%" (length facts))))))

;;; LOAD PERSISTED KNOWLEDGE
(let ((knowledge-file "core/kernel/imported_knowledge.lisp"))
  (if (probe-file knowledge-file)
      (progn
        (format t "LOADING KNOWLEDGE: ~a~%" knowledge-file)
        (load knowledge-file))
      (format t "NO KNOWLEDGE FILE FOUND at ~a~%" knowledge-file)))

;;; REPL ENTRY POINT
(defun main-loop ()
  (loop
    (format t "~%READY> ")
    (force-output)
    (let ((input (read)))
      (validate-batch input))))

(main-loop)

