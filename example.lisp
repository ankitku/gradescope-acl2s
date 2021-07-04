(include-book "interface/top")

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

:q

;; load autograder infrastructure
(load "autograder_raw_code.lsp")

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
  
  ;; then, load the student submission
  (load-acl2s-file "hwsubmission.lisp")

  ;; grade form to grade student submission
  (grade "test1"
	 5
	 (check-function '(=> (tlp l) (== (len2 l)
					  (len3 l)))))
  
  ;; finish grading
  (finish-grading))


;; the following command is necessary to create an executable
;; named run_autograder according to gradescope specification
(save-exec "run_autograder" nil
           :init-forms '((set-gag-mode nil)
                         (value :q))
           :toplevel-args "--eval '(declaim (sb-ext:muffle-conditions style-warning))' --eval '(acl2s::run-tests)' --disable-debugger")

