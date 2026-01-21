;;; ===============================================================
;;; LABORATORY WORK 4
;;; Variant 6
;;; ===============================================================

;;; ---------------------------------------------------------------
;;; PART 1: Refactored Functional Selection Sort
;;; ---------------------------------------------------------------

(defun selection-sort-functional (lst &key (key #'identity) (test #'<))
  "Functional Selection Sort using higher-order functions.
   - lst: The list to sort.
   - key: Function to apply to elements before comparison.
   - test: Comparator function (default is <).
   Returns a new sorted list."

  (labels ((remove-first (target sequence)
             (cond
               ((null sequence) nil)
               ((equal (car sequence) target) (cdr sequence))
               (t (cons (car sequence) (remove-first target (cdr sequence))))))

           (find-best-pair (pairs)
             (reduce (lambda (best current)
                       (if (funcall test (car current) (car best))
                           current
                           best))
                     pairs))

           (sort-pairs (pairs)
             (if (null pairs)
                 nil
                 (let* ((best-pair (find-best-pair pairs))
                        (remaining-pairs (remove-first best-pair pairs))))
                   (cons (cdr best-pair)
                         (sort-pairs remaining-pairs))))))

    (let ((keyed-list (mapcar (lambda (x) (cons (funcall key x) x)) lst)))
      
      (sort-pairs keyed-list))))


;;; ---------------------------------------------------------------
;;; PART 2: Closure Generator (Variant 6)
;;; ---------------------------------------------------------------


(defun rpropagation-reducer (&key (comparator #'<))
  "Returns a closure to be used with REDUCE.
   Arguments:
     - comparator: Function to compare elements (default <).
   Usage in REDUCE:
     - :from-end must be T.
     - :initial-value must be NIL (to signify the end of the list)."
  
  (lambda (elem acc)
    (if (null acc)
        (list elem)

        (let ((prev-best (car acc)))
          (if (funcall comparator elem prev-best)
              (cons elem acc)
              (cons prev-best acc))))))


;;; ===============================================================
;;; UNIT TESTS
;;; ===============================================================

(defun run-tests ()
  (format t "~%========== PART 1: SORTING TESTS ==========~%")

  (format t "Test 1 (Basic numbers): ~a~%" 
          (selection-sort-functional '(3 1 4 1 5 9 2 6) :test #'<))
  
  (format t "Test 2 (Strings by length): ~a~%" 
          (selection-sort-functional '("apple" "banana" "kiwi" "fig") 
                                     :key #'length 
                                     :test #'<))

  (format t "Test 3 (Pairs by second element): ~a~%" 
          (selection-sort-functional '((:a 10) (:b 5) (:c 20)) 
                                     :key #'second 
                                     :test #'<))

  (format t "Test 4 (Descending order): ~a~%" 
          (selection-sort-functional '(1 5 2 8 3) 
                                     :test #'>))

  (format t "~%========== PART 2: REDUCER TESTS ==========~%")

  ;; Scenario A: Standard less-than propagation
  ;; (3 2 1 2 3) -> Right to left:
  ;; 3 vs nil -> (3)
  ;; 2 vs 3 (2 < 3 is T) -> (2 3)
  ;; 1 vs 2 (1 < 2 is T) -> (1 2 3)
  ;; 2 vs 1 (2 < 1 is NIL) -> Replace 2 with 1 -> (1 1 2 3)
  ;; 3 vs 1 (3 < 1 is NIL) -> Replace 3 with 1 -> (1 1 1 2 3)
  (format t "Test A (3 2 1 2 3): ~a~%" 
          (reduce (rpropagation-reducer) 
                  '(3 2 1 2 3) 
                  :from-end t 
                  :initial-value nil))

  ;; Scenario B: Mixed
  ;; (3 1 4 2)
  ;; 2 vs nil -> (2)
  ;; 4 vs 2 (4 < 2 is NIL) -> Replace with 2 -> (2 2)
  ;; 1 vs 2 (1 < 2 is T)   -> Keep 1 -> (1 2 2)
  ;; 3 vs 1 (3 < 1 is NIL) -> Replace with 1 -> (1 1 2 2)
  (format t "Test B (3 1 4 2):   ~a~%" 
          (reduce (rpropagation-reducer) 
                  '(3 1 4 2) 
                  :from-end t 
                  :initial-value nil))

  ;; Scenario C: Greater-than comparator
  ;; (1 2 3) with >
  ;; 3 vs nil -> (3)
  ;; 2 vs 3 (2 > 3 is NIL) -> Replace with 3 -> (3 3)
  ;; 1 vs 3 (1 > 3 is NIL) -> Replace with 3 -> (3 3 3)
  (format t "Test C (1 2 3) [>]: ~a~%" 
          (reduce (rpropagation-reducer :comparator #'>) 
                  '(1 2 3) 
                  :from-end t 
                  :initial-value nil))
  
  (format t "~%========== TESTS FINISHED ==========~%"))

;;; Run the tests
(run-tests)
