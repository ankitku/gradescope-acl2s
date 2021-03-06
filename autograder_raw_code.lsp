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



(defun mem-tree (x tr)
  (cond
   ((atom tr) (equal x tr))
   ((endp tr) nil)
   (t (or (mem-tree x (car tr))
          (mem-tree x (cdr tr))))))

;; loads ACL2s file
;; check if definitions in notouch were redefined

(defun load-acl2s-file (filename &optional (notouch nil))
  (set-ld-redefinition-action nil state)
  ;;error-bit eb indicates if an error was encountered
  (setq eb nil)
  (setq ert "") ;; error accumulator
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
                  (setq ert (format nil "Error while reading input file : ~a~&" (get-captured-output)))
                  (setq eb t)
                  (return))))
   (when (equal sexp 'eof) (return))
   (when (mem-tree 'set-ld-redefinition-action sexp)
     (progn (setq ert (format nil "Attempt to set redefinition feature. Terminating file read operation. Make sure you ~&1) have the latest version of the homework and ~&2) use the exact definitions from the homework."))
            (setq eb t)
            (return)))
   (handler-case (let ((res (acl2s-event sexp)))
                   (when (car res)
                     (progn (setq ert (format nil "ACL2S Error : ~a.~&" (get-captured-output)))
                            (setq eb t)
                            (return))))
     (error (c)
            (setq ert (format nil "Error while admitting expression ~a.~&~a~&" sexp (get-captured-output)))
            (setq eb t)
            (return))))
  (let ((redefined (intersection-equal
                    notouch
                    (acl2::redefined-names state))))
    (when (consp redefined)
      (progn (setq ert (format nil "Following definitions, functions or properties were redefined : ~a~&" redefined))
             (setq eb t))))
  (mv eb ert))

;; loads lisp file
(defun load-lisp-file (filename)
  (with-open-file (s filename)
    (read s)))

;; extracts files from submissions folder (in Gradescope) before running tests
(defun extract-submissions ()
  (b* (((unless (cl-fad:directory-exists-p "submission/")) nil))
       (cl-fad:walk-directory "submission"
                              (lambda (child)
                                (rename-file child
                                             (make-pathname :defaults child
                                                            :directory (butlast (pathname-directory child))))))))


(defun collect-error (str)
  (setq er-text
        (concatenate 'string er-text str)))


(defun initialize ()
  ;; initializations required before grading
  (acl2::set-ld-verbose nil state)
  (acl2::set-ld-prompt nil state)
  (set-ld-redefinition-action nil state)
  (set-capture-output t)

  (setq er-text "")
  (setf *test-score-jsons* nil)
  (setf *total-score* 0)

  (extract-submissions)) ;;extracts files in "submissions" folder on gradescope




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
                                    ("visibility" "visible")
				    ("output" output))
				   *test-score-jsons*))))

(defun partial-grade (name points test)
  :ic (and (boolp (car test)) (natp points))
  (b* ((iscorrect (car test))
       (output    (second test))
       (score     (third test)))
    (setf *total-score* (+ *total-score* score))
    (setf *test-score-jsons* (cons (jsown:new-js
				    ("name" name)
				    ("score" score)
				    ("max_score" points)
                                    ("visibility" "visible")
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
                  ("output" er-text)
		  ("tests" (reverse *test-score-jsons*))
		  ("score" *total-score*)))))
  (sb-ext:exit))


;; Example Usage
#|

(grade "test1" 10 (t . "correct"))
(grade "test2" 10 (nil . "incorrect, here are counterexamples : ((a 1) (b 2))"))

(finish-grading)

|#

