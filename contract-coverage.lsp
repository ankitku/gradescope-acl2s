

;; Given a defdata type, return a list of recognizers corresponding to
;; the contract cases that we require the user to include.
(defun get-contract-cases-for-type (ty)
  (case ty
    ((:tl :bv :lobv :lobv5 :lochar :lostring :alist) '(endp consp))
    (:bv5 '(bv5p))
    ((:nat :bit) '(posp zp))
    (:all '(allp))
    (:char '(charp))
    (otherwise (error "Couldn't figure out the contract cases for type ~S"
                      ty))))



;; Given a list of definec arguments
;; e.g. an even-length list consisting of the concatenation of lists like (arg :ty)
;; return a list of lists of recognizer applications, where each list
;; of recognizer applications corresponds to one contract case for the
;; definec that the student should write a test case for.
;; The applications use the argument names from the definec argument list.
(defun contract-coverage-cases (args &optional (ic nil))
  (let ((contract-case-preds
         (loop for arg-name in args by #'cddr
               for arg-ty in (cdr args) by #'cddr
               collect (mapcar #'(lambda (c) (list c arg-name))
                               (get-contract-cases-for-type arg-ty)))))
    (mapcar
     (lambda (pred) (app ic pred))
     (apply #'alexandria:map-product 'list contract-case-preds))))

;; Look for a function call for the function with the
;; given name in a sexpr.
(defun find-function-call (fn-name sexpr)
  (trivia:match sexpr
                ((list* 'quote args) nil)
                ((list* f args)
                 (if (equal f fn-name)
                     sexpr
                   (loop for arg in args
                         for fc = (find-function-call fn-name arg)
                         when fc return it)))
                (otherwise nil)))

;; Given a check= form, search for an occurrence of the given function
(defun extract-function-call-from-test (fn-def test)
  (assert (equal (car test) 'check=))
  (let* ((name (second fn-def))
        (args (third fn-def))
        (arg-names (loop for arg-name in args by #'cddr collect arg)))
    (find-function-call name test)))

;; Given a contract case, a function def, and a list of tests, find
;; the test for which the function's call satisfies the contract case,
;; if any exists.
(defun find-test-satisfying-case (fn-def contract-case tests)
  (loop for test in tests
        when (let* ((name (second fn-def))
                    (args (third fn-def))
                    (arg-names (loop for arg-name in args by #'cddr collect arg-name))
                    (fn-call (extract-function-call-from-test fn-def test)))
               (if (not fn-call)
                   nil
                 (let* ((assignments (mapcar #'list arg-names (cdr fn-call)))
                        (res (acl2s-compute `(let ,assignments (and ,@contract-case)))))
                   (cond ((car res) (error "An error occurred when checking whether the test case ~S satisfied the contract case ~S" test contract-case))
                         ((second res) test)
                         (t nil)))))
        return it))

#|
(find-test-satisfying-case '(definec write-mem (a :address d :data m :memory) :memory
  (match m
    (nil (acons a d nil))
    (((!a . &) . r) (acons a d r))
    ((f . r) (cons f (write-mem a d r)))))
                           '((IS-ZEROP A) (CONSP M) (DATAP D))
                           '((check= (write-mem 0 'a '((1 . 0))) '((1 . 0) (0 . a)))))
|#

;; Check that the user provided at least one test for each contract
;; case for the given function
(defun check-all-contract-coverage-cases-provided (fn-def contract-cases tests)
  (block ret
    (loop for contract-case in contract-cases
          do (when (not (find-test-satisfying-case fn-def contract-case tests))
               (return-from ret (cons nil (format nil "None of your provided tests satisfied the contract coverage case ~a" (cons 'and contract-case))))))
    (cons t "Covered all contract cases.")))

;;(contract-coverage-cases '(s :lobv5 m :all d :nat))


(defun filter-cases (contract-cases ic)
  (match contract-cases
    (() '())
    ((cc . rst) (b* ((q `(=> ,(app (cons 'and cc)
                                   (bvp 'b) (bvp 'b1) (bvp 'b2) (bvp 'v)
                                   (allp 'x) (natp 'n) (locharp 'm) (lobv5p 's))
                             ,ic))
                     (- (print q))
                     (res (acl2s-compute q))
                     (- (print res)))
                  (if (and  (not (first res))
                            (second res))
                      (cons cc (contract-cases rst ic))
                    (contract-cases rst ic))))))

(defun extract-ic (fundef)
  (cond
   ((v (== :ic (fifth fundef))
       (== :pre (fifth fundef)))
    (if (v (== (car (sixth fundef)) 'and)
           (== (car (sixth fundef)) '^))
        (cdr (sixth fundef))
      (list (sixth fundef))))
   (& nil)))
      

                 
;; Check that the user provided at least one test for each non-trivial
;; contract case for the given function
(defun check-contract-coverage (name function-def-alist test-alist)
  (let ((tests-entry (assoc name test-alist :test 'equal))
        (fn-def-entry (assoc name function-def-alist :test 'equal)))
    (cond ((not tests-entry)
           (cons nil (format nil "No check=? forms were found after the definition of ~a." name)))
          ((not fn-def-entry)
           (cons nil (format nil "Couldn't find the definition for ~a" name)))
          (t
           (b* ((ic (extract-ic (cdr fn-def-entry)))
                (contract-cases
                 (remove-if (lambda (x)
                              (valid-prop? `(not ,(cons 'and x))))
                            (contract-coverage-cases
                             (third (cdr fn-def-entry))
                             ic))))
             (check-all-contract-coverage-cases-provided (cdr fn-def-entry) contract-cases (cdr tests-entry)))))))

