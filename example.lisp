(include-book "interface/top")

;; Instructor can define their forms here
(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

:q

;; load acl2s grading infrastructure
(load "autograder_raw_code.lsp")
(in-package "ACL2S")

;; a function to compare instr and submitted functions
(defun check-function (checkform)
  (let* ((res (itest?-query checkform)))
    (if (car res)
	(cons nil (format nil "[Mistake in function definition, try with these counterexamples : ~a]" (cdr res)))
      (cons t (format nil "[Correct]")))))

;; first, load the instructor file
;; (or have its contents before :q as shown)
(load-acl2s-file "instr.lisp")


(defun run-tests ()
  ;; Load the student submission
  (load-acl2s-file "hwsubmission.lisp")

  ;; Grade form to grade student submission
  (grade "test1"          ;; test case name
	 5                ;; points allocated to this test
	 (check-function '(=> (tlp l) (== (len2 l)      ;; custom check function 
					  (len3 l)))))  ;; should return (bool . string)
  ;; We know that student must have submitted len3 function, if that was asked in the test
  ;; Finish grading
  (finish-grading))


;; the following command is necessary to create an executable
;; named run_autograder according to gradescope specification
(save-exec "run_autograder" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::run-tests)' --disable-debugger")

