;;--------------------------------------------------------------------------------------
;; Authors : Ankit Kumar, Andrew Walter
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
(import 'acl2s-interface::(get-captured-output set-capture-output))

;; adapted from https://stackoverflow.com/a/8335390
(defun f-no-trail (stream arg &optional colon at digits)
  (declare (ignore colon at))
  (prin1 (cond ((= (round arg) arg) (round arg))
               (digits (float (/ (round (* arg (expt 10 digits)))
                                 (expt 10 digits))))
               (t (float arg)))
         stream))

;; Check if file was submitted
(defun check-file-submission (fname)
  (if (or (null fname)
          (null (probe-file fname)))
      (cons nil "A file submission was not found. Did you submit all the files?")
    (cons t "File submitted")))

;; loads ACL2s file
;; check if definitions in notouch were redefined

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun read-file-semicolon (infile)
  (setq pointer nil)
  (dolist (line (uiop:read-file-lines infile))
    (when pointer (progn (setq res line) (return)))
    (when (natp (search "delete any characters from this file" line))
      (setq pointer t)))
  (string-trim " " res))

(defun should-eval-using-acl2s-event? (sexp)
  (and (consp sexp)
       (or (in (car sexp)
               '(defun defunc definec definecd
                       defconst def-const defdata defdata-alias
                       set-well-founded-relation
                       set-irrelevant-formals-ok
                       defdata-subtype test? thm in-theory
                       must-fail ACL2S-DEFAULTS must-pass
                       property check= check set-ignore-ok
                       set-termination-method
                       set-well-founded-relation
                       ))
           (natp (search "SET-DEFUNC" (string (car sexp))))
           (natp (search "SET-ACL2" (string (car sexp)))))))
  

;; end-of-init-callback, if provided, is a function that will be
;; called to help determine when any unwanted initialization code at
;; the beginning of the file is completed. It should take in a string
;; and return a generalized boolean, non-nil if the current line
;; indicates that we have read past the end of the initialization
;; code, and nil otherwise.

;; pre-eval-callback, if provided, is a function that will be called
;; after each post-initialization-code S-expression is read, and
;; before it is evaluated. It will be called with the read
;; S-expression, and is expected to return a generalized boolean. If
;; it returns a non-nil value, the S-expression will be evaluated. If
;; it returns nil, the S-expression will not be evaluated (it will be
;; skipped).

;; eval-callback, if provided, is a function that will be called after
;; each post-initialization-code S-expression is read and
;; evaluated. It will be called with the read S-expression and the
;; result of evaluation, and is called only for side-effects.
(defun load-acl2s-file (filename &key (notouch nil) (eval-callback nil) (pre-eval-callback nil) (end-of-init-callback nil))
  (acl2s-event '(set-ld-redefinition-action nil state))
  (let ((any-read-error nil))
    (with-open-file (file-stream filename :direction :input)
      ;; Code to handle eclipse crap, only if needed
      (when (natp (search "END INITIALIZATION" (read-file filename)))
        (loop for line = (read-line file-stream)
              ;; Don't read any of the crap added by the eclipse version of acl2s
              when (or (natp (search "END INITIALIZATION" line))
                       (natp (search "(acl2::in-package \"ACL2S\")" line))
                       (and (functionp end-of-init-callback) (funcall end-of-init-callback line)))
              return nil
              when (natp (search "(set-defunc" line))
              do (progn (acl2s-event (read-from-string line))
                        (return))))
      (loop for sexp =
            (handler-case
                (let ((*readtable* acl2::*acl2-readtable*))
                  (read file-stream nil 'eof))
              (error (c)
                     (update-errors (format nil "Error while reading input file : ~a" (get-captured-output)))
                     (setf any-read-error t)
                     nil))
            when (equal sexp 'eof) return nil
            when (mem-tree 'set-ld-redefinition-action sexp) do
            (progn (setq ert (format nil "Attempt to set redefinition feature. Terminating file read operation. Make sure you 1) have the latest version of the homework and 2) use the exact definitions from the homework."))
                   (return))
            do (handler-case
                   (let ((res (cond ((and (functionp pre-eval-callback) (not (funcall pre-eval-callback sexp))) nil)
                                    ((should-eval-using-acl2s-event? sexp) (acl2s-event sexp))
                                    ((and (consp sexp) (equal (car sexp) 'set-gag-mode)) nil)
                                    (t (acl2s-compute sexp)))))
                     (when (functionp eval-callback) (funcall eval-callback sexp res))
                     (when (car res)
                       (update-errors (format nil "ACL2S Error : ~a." (get-captured-output)))))
                 (error (c)
                        (update-errors (format nil "Error while admitting expression ~a. ~a" sexp (get-captured-output))))))
      (let ((redefined (intersection-equal
                        notouch
                        (acl2::redefined-names state))))
        (when (consp redefined)
          (progn (update-errors (format nil "Following definitions, functions or properties were redefined : ~a" redefined))))))
    any-read-error))

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

(defun update-errors (str)
  (setq ert (concatenate 'string ert str)))

(defun show-errors ()
  (format nil ert))


(defun initialize ()
  ;; initializations required before grading
  (acl2::set-ld-verbose nil state)
  (acl2::set-ld-prompt nil state)
  (set-ld-redefinition-action nil state)
  (set-capture-output t)

  (setq ert "") ;;error-accumulator
  (setf *test-score-jsons* nil)
  (setf *total-score* 0)

  ; don't extract submissions if grading file of ANY name
  (extract-submissions)
  )

(defparameter *AUTOGRADER-OUTPUT-VISIBILITY* "visible"
  "The visibility specifier to use for autograder output. See
  https://gradescope-autograders.readthedocs.io/en/latest/specs/#controlling-test-case-visibility
  for more information on the possible values that this variable may
  take.")

(defun grade (name points test &key (visibility nil))
  (assert (and (boolp (car test)) (rationalp points)))
  (b* ((iscorrect (car test))
       (output    (cdr test))
       (score     (if iscorrect points 0)))
    (setf *total-score* (+ *total-score* score))
    (let ((ret-obj
           (jsown:new-js
	    ("name" name)
	    ("score" score)
	    ("max_score" points)
            ("visibility" (or visibility *AUTOGRADER-OUTPUT-VISIBILITY*))
	    ("output" output))))
      (when (equal points 0)
        (setf (jsown:val ret-obj "status") (if iscorrect "passed" "failed")))
      (push ret-obj *test-score-jsons*))))

(defun concat-all-strings (strings &key (separator (format nil "~%")))
  (let ((res ""))
    (loop for string in strings
          for i from (- (len strings) 1) downto 0
          do (setf res (concatenate 'string res string (if (> i 0) separator ""))))
    res))


(defun gradespec-actionp (x)
  (or (equal x :=)
      (equal x :+)
      (equal x :part)))

#| Gradespecs and partial-grade

The `partial-grade` macro is designed to take in "gradespecs", which
describe an action that should be taken to check a problem, and the
consequences for that action's failure.

Gradespecs are lists of the form:
(subname points test)
or
(subname action points test)

e.g., action is optional. If it is not provided, action is set to
:part.

Subname should be a printable value; this is what this check will be
referred to as in the output shown to the student.

Action is either :+, :=, or :part if it is provided. This will be
discussed later.

Points should evaluate to a number.

Test should evaluate to (pass? . msg), where msg is a string and pass?
is non-nil if the student should recieve full points for this part of
the problem.

`partial-grade` will evaluate the gradespecs in the order provided. If
running a gradespec's test results in an error, the student will
recieve 0 points for that gradespec's portion of the problem.

The maximum possible score for a problem is calculated only using the
point values provided for gradespecs with no action or an action of
:part.

If a gradespec has an action of :part or if none is provided, the
student will recieve full credit (the point value assocated with the
gradespec) if the test passes. The returned message will be printed in
either case.

If a gradespec has an action of :+, the student will recieve a bonus
to their score (whatever point value is associated with the
gradespec) if the test passes.

If a gradespec has an action of := and the test fails, the student
will recieve exactly the given number of points for the entire
problem, and only the message that the test produces will be shown to
the user. If the test passes, no information is shown to the
user. This is intended to support checks like "did you provide this
function", where none of the subsequent checks should run if the
student did not satisfy the check.

If any gradespec has a points value of 0 and does not have the :=
action, it is considered to be a "diagnostic check". If any diagnostic
check fails, the entire result will be displayed as failed in
Gradescope, regardless of and independently from the number of points
earned.

|#

(defmacro partial-grade (name &rest gradespecs)
  (let* ((partial-score-max 0)
         (partial-grade-return-block-name (gensym "PARTIAL-GRADE-RETURN-"))
         (expanded-gradespecs
          (loop for gradespec in gradespecs
                collect (progn (when (not (or (equal (length gradespec) 3)
                                              (and (equal (length gradespec) 4)
                                                   (gradespec-actionp (second gradespec)))))
                                 (error "Improperly formed gradespec passed to partial-grade: ~S" gradespec))
                               (let ((expanded-gradespec (if (equal (length gradespec) 4) gradespec (list (first gradespec) :part (second gradespec) (third gradespec)))))
                                 (when (equal (second expanded-gradespec) :part)
                                   (setf partial-score-max (+ partial-score-max (third expanded-gradespec))))
                                 expanded-gradespec)))))
    `(let ((score 0)
           (outputs nil)
           (diagnostic-failure nil))
       (block ,partial-grade-return-block-name
         ,@(loop for gradespec in expanded-gradespecs
                 collect (let* ((subname (car gradespec))
                                (action (second gradespec))
                                (points (third gradespec))
                                (test (fourth gradespec)))
                           `(handler-case ,test
                              (error (e)
                                     ,(if (equal action :=)
                                          `(progn (setf score ,points)
                                                  (setf outputs (list (format nil "~a: ERROR" ,subname)))
                                                  (return-from ,partial-grade-return-block-name))
                                        `(progn
                                           (when (equal ,points 0) (setf diagnostic-failure t))
                                           (push (format nil "~a: 0/~/acl2s::f-no-trail/ (ERROR)" ,subname ,points) outputs))))
                              (:no-error (v)
                                         ,(if (equal action :=)
                                              `(when (not (car v))
                                                 (setf score ,points)
                                                 (setf outputs (list (cdr v)))
                                                 (return-from ,partial-grade-return-block-name))
                                            `(progn (when (car v) (setf score (+ ,points score)))
                                                    (when (and (equal ,points 0) (not (car v)))
                                                      (setf diagnostic-failure t))
                                                    (push (format nil "~a: ~/acl2s::f-no-trail//~/acl2s::f-no-trail/ (~a)" ,subname
                                                                  (if (car v) ,points 0)
                                                                  ,points
                                                                  (cdr v))
                                                          outputs))))))))
       (setf *total-score* (+ *total-score* score))
       (let ((ret-obj (jsown:new-js
	      ("name" ,name)
	      ("score" score)
	      ("max_score" ,partial-score-max)
              ("visibility" *AUTOGRADER-OUTPUT-VISIBILITY*)
	      ("output" (concat-all-strings (reverse outputs))))))
         (when diagnostic-failure (setf (jsown:val ret-obj "status") "failed"))
         (push ret-obj *test-score-jsons*)))))

(defun finish-grading ()
  (with-open-file (stream "results/results.json"
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (write-string (jsown:to-json
		   (jsown:new-js
                    ;; stdout is either "hidden" or "visible"
                    ("stdout_visibility" "hidden")
                                        ;("output" (show-errors))
		    ("tests" (reverse *test-score-jsons*))
		    ("score" *total-score*)))
                  stream))
  (sb-ext:exit))


;; Example Usage
#|

(grade "test1" 10 (t . "correct"))
(grade "test2" 10 (nil . "incorrect, here are counterexamples : ((a 1) (b 2))"))

(finish-grading)

|#

