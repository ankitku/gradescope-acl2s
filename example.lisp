(include-book "acl2s/interface/acl2s-utils/top" :dir :system)

;; Instructor ACL2s forms here
;; ------ ------ ------ ------ ------ ------ ------ ------ ------ ------


;; ------ ------ ------ ------ ------ ------ ------ ------ ------ ------


:q

(in-package "ACL2S")
(import 'acl2s-interface::(acl2s-event acl2s-query acl2s-compute))

;; load acl2s grading infrastructure
(load "autograder_raw_code.lsp")
(load "autograder-utils.lsp")

;; first, load the instructor file
;; (or have its contents before :q as shown above)
(load-acl2s-file "instr-hwk1.lisp")

(defun run-tests ()
  (initialize) ;;sets up required parameters and files needed for grading

  (b* ((fname "hwk1.lisp")
       (res (check-file-submission fname))
       (-   (grade "check submission" 0 res))
       ((unless (car res)) nil)
       ((mv eb ert) (load-acl2s-file fname))
       ((when eb) (collect-error ert))) ;;ert
    ;;collects errors generated when grading
    
    ;; Grade form to grade student submission
    (grade "test right rotate"          ;; test case name
           5                ;; points allocated to this test
           (query-equiv '(=> (^ (natp n) (tlp l))
                             (== (rr n l)           ;; custom check function 
                                 (instr-rr n l))))) ;; should return (bool . string)



    (grade "test efficient right rotate"          
           5                
           (query-equiv '(=> (^ (natp n) (tlp l))
                             (== (err n l)      
                                 (instr-rr n l)))))


    (grade "test n->b"          
           5                
           (query-equiv '(=> (nat n) (== (instr-n-to-b n)      
                                         (n-to-b n)))))


    (grade "test b->n"          
           5                
           (query-equiv '(=> (bitvector b) (== (instr-b-to-n b)      
                                               (b-to-n b)))))
    )


  ;; (b* ((fname "hwk2.lisp")
  ;;      (res (check-file-submission fname))
  ;;      (-   (grade "check_submission" 0 res))
  ;;      ((unless (car res)) nil)
  ;;      ((mv eb ert) (load-acl2s-file fname '(erp rat-errp ack)))
  ;;      ((when eb) (setq er-text (concatenate 'string er-text ert))))    

  ;;   ;;TODO : RESET test settings after loading student file
  ;;   ;; we want a macro that can be used, to instead of grade, maybe check.
  ;;   ;; "Checking that you did not redefine this function. If this fails, make
  ;;   ;; sure you 1) have the latest version of the homework 2) Use the exact
  ;;   ;; definition from the homework."
  ;;   ;; Macro gets arg: name of the function we are checking. Checking for a
  ;;   ;; redef.
  ;;   ;; see xdoc world, to check for redef of function defs, defthms.

  ;;   ;; Reset random seed before every test/ or increase number of ctrex
  ;;   ;; grade should reset the seed to 42.
  ;;   ;; amount of testing

  ;;   ;; Contact frontdesk, check if anything is reqd. for gradescope.

  ;;   ;; Drew code to query defdata seed, func redef..
    
  ;;   ;; Test Data Equivalence
  ;;   (grade "test-saexpr"          
  ;;          5                
  ;;          (query-equiv '(== (instr-saexprp l)
  ;;                            (saexprp l))))

    
  ;;   ;; Test Function Equivalence
  ;;   (grade "testing lookup function"          
  ;;          2                
  ;;          (query-equiv '(=> (and (varp v) (assignmentp a))
  ;;                            (== (instr-lookup v a)
  ;;                                (lookup v a)))))
    

  ;;   (grade "test-saeval"          
  ;;          5                
  ;;          (query-equiv '(=> (and (instr-saexprp e) (assignmentp a))
  ;;                            (== (instr-saeval e a)
  ;;                                (saeval e a)))))


  ;;   ;; Test Properties
  ;;   (grade "test-m-ack-1"
  ;;          2
  ;;          (query-equiv '(=> (and (natp n)
  ;;                                 (natp m)
  ;;                                 (and (! (zp n)) (zp m)))
  ;;                            (d< (m-ack (1- n) 1) (m-ack n m)))))

  ;;   (grade "test-m-ack-2"
  ;;          2
  ;;          (query-equiv '(=> (and (natp n)
  ;;                                 (natp m)
  ;;                                 (and (! (zp n)) (! (zp m))))
  ;;                            (d< (m-ack n (1- m)) (m-ack n m)))))

  ;;   (grade "test-m-ack-3"
  ;;          2
  ;;          (query-equiv '(=> (and (natp n)
  ;;                                 (natp m)
  ;;                                 (and (! (zp n)) (! (zp m))))
  ;;                            (d< (m-ack (1- n) (ack n (1- m))) (m-ack n m)))))
    
           

  ;;   )


  ;; Finish grading
  (finish-grading))


;; the following command is necessary to create an executable
;; named run_autograder according to gradescope specification
(save-exec "run_autograder" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::run-tests)' --disable-debugger")
