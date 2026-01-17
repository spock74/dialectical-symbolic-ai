
(load "backend/lisp/bootstrap.lisp")

(in-package :s-dialectic)

(defun float-equal (a b &optional (epsilon 0.0001))
  (< (abs (- a b)) epsilon))

(defun test-vector-math ()
  (format t "~%--- Testing Vector Math Primitives ---~%")
  
  (let* ((v1 #(1.0 2.0 3.0))
         (v2 #(4.0 5.0 6.0))
         (v3 #(2.0 2.0))
         (sum (vector-add v1 v2))
         (sub (vector-sub v2 v1))
         (dist (euclidean-distance v1 v2))
         (centroid (vector-centroid (list v1 v2))))
    
    ;; Test ADD
    (if (and (= (aref sum 0) 5.0) (= (aref sum 1) 7.0) (= (aref sum 2) 9.0))
        (format t "PASS: Vector Add~%")
        (format t "FAIL: Vector Add ~a~%" sum))

    ;; Test SUB
    (if (and (= (aref sub 0) 3.0) (= (aref sub 1) 3.0) (= (aref sub 2) 3.0))
        (format t "PASS: Vector Sub~%")
        (format t "FAIL: Vector Sub ~a~%" sub))

    ;; Test DIST (sqrt(3^2 + 3^2 + 3^2) = sqrt(27) ~ 5.196)
    (if (float-equal dist 5.196152)
        (format t "PASS: Euclidean Dist (~f)~%" dist)
        (format t "FAIL: Euclidean Dist ~f~%" dist))
        
    ;; Test Centroid ((1+4)/2, (2+5)/2, (3+6)/2) = (2.5, 3.5, 4.5)
    (if (and (= (aref centroid 0) 2.5) (= (aref centroid 1) 3.5))
        (format t "PASS: Centroid~%")
        (format t "FAIL: Centroid ~a~%" centroid))

    ;; Test Geometric Consistency (TransE)
    ;; S=[1,1] + R=[2,2] => Predicted=[3,3]
    ;; Case 1: Perfect Match O=[3,3]. Dist=0. Score = 1/(1+0) = 1.0
    (let* ((S #(1.0 1.0))
           (R #(2.0 2.0))
           (O_good #(3.0 3.0))
           (O_bad  #(10.0 10.0)) ;; Dist( (3,3), (10,10) ) = sqrt(49+49) = 9.899
           (score-good (geometric-consistency-score S R O_good))
           (score-bad  (geometric-consistency-score S R O_bad)))
      
      (if (float-equal score-good 1.0)
          (format t "PASS: TransE Score (Perfect Match) = ~f~%" score-good)
          (format t "FAIL: TransE Score (Perfect Match) = ~f~%" score-good))

      (if (< score-bad 0.1)
          (format t "PASS: TransE Score (Mismatch) = ~f~%" score-bad)
          (format t "FAIL: TransE Score (Mismatch) = ~f (Expected < 0.1)~%" score-bad))

      ;; Test High Level API (Inside same let* to access S, R, O)
      (let ((stat-good (verify-triplet-consistency S R O_good))
            (stat-bad  (verify-triplet-consistency S R O_bad)))
          
        (if (eq stat-good :ROBUST)
            (format t "PASS: Verify ROBUST~%")
            (format t "FAIL: Verify ROBUST (Got ~a)~%" stat-good))
        
        (if (eq stat-bad :HALLUCINATION)
            (format t "PASS: Verify HALLUCINATION~%")
            (format t "FAIL: Verify HALLUCINATION (Got ~a)~%" stat-bad)))

    ;; Test Integrated Logic (adicionar-relacao)
    ;; 1. Add concepts with vectors (Simulate Injection)
    (adicionar-memoria "SUBJ_TEST" "Desc" :vector S)
    (adicionar-memoria "OBJ_TEST" "Desc" :vector O_good)
    (atualizar-vetor "PRED_TEST" R) ;; Predicate as a concept/vector holder
    
    (let ((res-robust (adicionar-relacao "SUBJ_TEST" "PRED_TEST" "OBJ_TEST")))
       (if (search "CONSISTENCY: ROBUST" res-robust)
           (format t "PASS: Integration ROBUST~%")
           (format t "FAIL: Integration ROBUST (Got: ~s)~%" res-robust)))

    (let ((res-hallu (adicionar-relacao "SUBJ_TEST" "PRED_TEST" "NO_VEC")))
       ;; Should be normal (no vectors or partial) -> UNKNOWN or default
       (if (not (search "CONSISTENCY" res-hallu))
           (format t "PASS: Integration UNKNOWN (No tags)~%")
           (format t "FAIL: Integration UNKNOWN (Got: ~s)~%" res-hallu)))
  ))
  (format t "--- Tests Complete ---~%")
  (sb-ext:exit))

(test-vector-math)
