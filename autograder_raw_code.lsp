;;--------------------------------------------------------------------------------------
;; Author : Ankit Kumar
;; The following code enables grading student's versions of solutions loaded in ACL2s. 
;; Wrapping up tests in grade function will record grades and finish-grading will 
;; generate an output results.json file.
;;--------------------------------------------------------------------------------------

:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :jsown)
(ql:quickload :cl-fad)
(ql:quickload :uiop)
(in-package "ACL2S")

;; Check if file was submitted
(defun check-file-submission (fname)
  (let ((res (probe-file fname)))
    (if (== res nil)
        (cons nil (format nil "Submission ~a not found. Did you submit the correct file?" fname))
      (cons t "File submitted"))))

;; loads ACL2s file
;; check if definitions in notouch were redefined

(defun load-acl2s-file (filename &optional (notouch nil))
  (set-ld-redefinition-action nil state)
  ;;error-bit indicates if an error was encountered
  (setq eb nil)
  (defparameter *j* (open filename))
  (loop
   (setq sexp
         ;; propagate errors - printed on stdout which is visible
         ;; useful for must-fail, stack exhaustion..
         ;; report function redefinitions/warning/error
         
         ;; dispatch specific errors, like redef error
         ;; if definec, defining prohibited func, and see error, report
         (handler-case
             (let ((*readtable* acl2::*acl2-readtable*))
               (read *j* nil 'eof))
           (error (c)
                  (format t "Error while reading input file : ~a~&" c)
                  (setq eb t)
                  (return))))
   (when (equal sexp 'eof) (return))
   (when (equal (car sexp) 'set-ld-redefinition-action)
     (progn (format t "Attempt to set redefinition feature. Terminating file read operation. Make sure you ~&1) have the latest version of the homework and ~&2) use the exact definitions from the homework.")
            (setq eb t)
            (return)))
   (handler-case (acl2s-event sexp)
     (error (c)
            (format t "Error while admitting expression ~a.~&~a~&" sexp c)
            (setq eb t)
            (return))))
  (let ((redefined (intersection-equal
                    notouch
                    (acl2::redefined-names state))))
    (when (consp redefined)
      (progn (format t "Following definitions, functions or properties were redefined : ~a~&" redefined)
             (setq eb t))))
  eb)

;; loads lisp file
(defun load-lisp-file (filename)
  (with-open-file (s filename)
    (read s)))

;; extracts files from submissions folder (in Gradescope)
(defun extract-submissions ()
  (b* (((unless (cl-fad:directory-exists-p "submission/")) nil))
       (cl-fad:walk-directory "submission"
                              (lambda (child)
                                (rename-file child
                                             (make-pathname :defaults child
                                                            :directory (butlast (pathname-directory child))))))))
  
(setf *test-score-jsons* nil)
(setf *total-score* 0)

(defun grade (name points test)
  :ic (and (boolp (car test)) (natp points))
  (b* ((iscorrect (car test))
       (output    (cdr test))
       (score     (if iscorrect points 0)))
    (setf *total-score* (+ *total-score* score))
    (setf *test-score-jsons* (cons (jsown:new-js
				    ("name" name)
				    ("score" score)
				    ("max_score" points)
				    ("output" output))
				   *test-score-jsons*))))
  

(defun finish-grading ()
  (with-open-file (str "results/results.json"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format str (jsown:to-json
		 (jsown:new-js
                  ;; hidden or visible stdout
                  ("stdout_visibility" "visible")
		  ("tests" (reverse *test-score-jsons*))
		  ("score" *total-score*)))))
  (sb-ext:exit))


;; Example Usage
#|

(grade "test1" 10 (t . "correct"))
(grade "test2" 10 (nil . "incorrect, here are counterexamples : ((a 1) (b 2))"))

(finish-grading)

|#

