(include-book "interface/top")

;; Instructor ACL2s forms here
;; ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
(definec <<= (x :all y :all) :bool
  (or (== x y)
      (<< x y)))

(definec insert (a :all x :tl) :tl
  (match x
    (() (list a))
    ((e . es) (if (<<= a e)
                  (cons a x)
                (cons e (insert a es))))))

(definec isort (x :tl) :tl
  (match x
    (() ())
    ((e . es) (insert e (isort es)))))

(definec less (a :all x :tl) :tl
  (match x
    (() ())
    ((e . es) (if (<< e a)
                  (cons e (less a es))
                (less a es)))))

(definec notless (a :all x :tl) :tl
  (match x
    (() ())
    ((e . es) (if (<<= a e)
                  (cons e (notless a es))
                (notless a es)))))

(definec qsort (x :tl) :tl
  (match x 
    (() ())
    ((e . es) (app (qsort (less e es))
                   (list e)
                   (qsort (notless e es))))))

;; ------ ------ ------ ------ ------ ------ ------ ------ ------ ------


:q

;; load acl2s grading infrastructure
(load "autograder_raw_code.lsp")
(in-package "ACL2S")

;; a function to compare instr and submitted functions
(defun query-equiv (checkform)
  (handler-case
      (setq res (itest?-query checkform))
    (error (c)
           (cons nil "There was an error while checking your submission : ~a"
                 c)))
  (cond
   ((car res)
    (cons nil (format nil "[Incorrect definition, try with
these counterexamples : ~a]" (cdr res))))
   (t (cons t (format nil "[Correct]")))))

;; first, load the instructor file
;; (or have its contents before :q as shown above)
(load-acl2s-file "instr.lisp")

(defun run-tests ()
  (setq er-text "")
  (extract-submissions) ;;extracts files in "submissions" folder on gradescope

  
  (b* ((fname "hwk1.lisp")
       (res (check-file-submission fname))
       (-   (grade "check_submission" 0 res))
       ((unless (car res)) nil)
       ((mv eb ert) (load-acl2s-file fname '(erp rat-errp ack)))
       ((when eb) (setq er-text (concatenate 'string er-text ert))))
    
    ;; Grade form to grade student submission
    (grade "test1"          ;; test case name
           5                ;; points allocated to this test
           (query-equiv '(=> (tlp l) (== (isort l)      ;; custom check function 
                                         (qsort l)))))) ;; should return (bool . string)


  (b* ((fname "hwk2.lisp")
       (res (check-file-submission fname))
       (-   (grade "check_submission" 0 res))
       ((unless (car res)) nil)
       ((mv eb ert) (load-acl2s-file fname '(erp rat-errp ack)))
       ((when eb) (setq er-text (concatenate 'string er-text ert))))    

    ;;TODO : RESET test settings after loading student file
    ;; we want a macro that can be used, to instead of grade, maybe check.
    ;; "Checking that you did not redefine this function. If this fails, make
    ;; sure you 1) have the latest version of the homework 2) Use the exact
    ;; definition from the homework."
    ;; Macro gets arg: name of the function we are checking. Checking for a
    ;; redef.
    ;; see xdoc world, to check for redef of function defs, defthms.

    ;; Reset random seed before every test/ or increase number of ctrex
    ;; grade should reset the seed to 42.
    ;; amount of testing

    ;; Contact frontdesk, check if anything is reqd. for gradescope.

    ;; Drew code to query defdata seed, func redef..
    
    ;; Test Data Equivalence
    (grade "test-saexpr"          
           5                
           (query-equiv '(== (instr-saexprp l)
                             (saexprp l))))

    
    ;; Test Function Equivalence
    (grade "testing lookup function"          
           2                
           (query-equiv '(=> (and (varp v) (assignmentp a))
                             (== (instr-lookup v a)
                                 (lookup v a)))))
    

    (grade "test-saeval"          
           5                
           (query-equiv '(=> (and (instr-saexprp e) (assignmentp a))
                             (== (instr-saeval e a)
                                 (saeval e a)))))


    ;; Test Properties
    (grade "test-m-ack-1"
           2
           (query-equiv '(=> (and (natp n)
                                  (natp m)
                                  (and (! (zp n)) (zp m)))
                             (d< (m-ack (1- n) 1) (m-ack n m)))))

    (grade "test-m-ack-2"
           2
           (query-equiv '(=> (and (natp n)
                                  (natp m)
                                  (and (! (zp n)) (! (zp m))))
                             (d< (m-ack n (1- m)) (m-ack n m)))))

    (grade "test-m-ack-3"
           2
           (query-equiv '(=> (and (natp n)
                                  (natp m)
                                  (and (! (zp n)) (! (zp m))))
                             (d< (m-ack (1- n) (ack n (1- m))) (m-ack n m)))))
    
           

    )


  ;; Finish grading
  (finish-grading))


;; the following command is necessary to create an executable
;; named run_autograder according to gradescope specification
(save-exec "run_autograder" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::run-tests)' --disable-debugger")
