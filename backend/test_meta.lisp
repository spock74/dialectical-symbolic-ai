;;; SDialectic Persistent Knowledge Base
(in-package :s-dialectic)
(limpar-memoria)

;;; --- Custom Definitions (Functions & Macros) ---
(DEFUN TEST-FUNC (X) (DECLARE (IGNORABLE X)) (FORMAT NIL "HELLO-~a" X))

(DEFMACRO TEST-MACRO (VAR VAL) `(ADICIONAR-MEMORIA ,VAR ,VAL))

;;; --- Memories ---
(adicionar-memoria "META-KEY" "META-VAL")

;;; --- Relations ---

;;; --- Rules ---
(adicionar-regra 'TRANSITIVIDADE-CATEGORICA '((S-DIALECTIC::?X
                                               S-DIALECTIC::|É UM|
                                               S-DIALECTIC::?Y)
                                              (S-DIALECTIC::?Y
                                               S-DIALECTIC::|É UM|
                                               S-DIALECTIC::?Z)) '((S-DIALECTIC::?X
                                                                    S-DIALECTIC::|É UM|
                                                                    S-DIALECTIC::?Z)))

(format t "
;;; Knowledge Base Loaded Successfully.
")
