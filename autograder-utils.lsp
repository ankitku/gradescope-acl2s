(ql:quickload :trivia)
(ql:quickload :cl-ppcre)
(ql:quickload :trivia.ppcre)
(setf ppcre:*allow-named-registers* t)
(ql:quickload :alexandria)

(import 'acl2s-interface-extras::itest?-query)

;; a function to compare instr and submitted functions
(defun query-equiv (checkform)
  (setq eb nil)
  (setq query-error "")
  (handler-case
      (setq res (itest?-query checkform))
    (error (c)
           (setq eb t)
           (setq query-error (format nil "There was an error while checking your submission : ~a" (get-captured-output)))
           (update-errors query-error)))
  (cond
   (eb (cons nil query-error))
   ((car res)
    (cons nil (format nil "Incorrect definition, try with these counterexamples : ~a" (cdr res))))
   (t (cons t (format nil "Correct")))))

(defun check-names (hwk-file-name)
  (let ((str (read-file hwk-file-name)))
    (if (equal nil (search "Names of ALL group members:" str))
        (cons nil (format nil "Prompt for names of group members missing. Make sure you do not edit comments other than filling in ...s"))
      (if (natp (search "Names of ALL group members: ." str))
          (cons nil (format nil "Please fill in names of group members"))
        (cons t (format nil "OK"))))))

(defun check-alist (ial sal)
  (cond
   ((endp ial) 0)
   (t (if (equal (cdar ial) (cdr (assoc-equal (caar ial) sal)))
          (1+ (check-alist (cdr ial) sal))
        (check-alist (cdr ial) sal)))))

(defun grade-alist (ial sal)
  (handler-case
      (let ((score (check-alist ial sal)))
        (list (if (== score (len ial)) t nil)
              (format nil "~a/~a correct" score (len ial)) score))
    (error (c)
           (list nil (format nil "There was an error while checking your submission : ~a" (get-captured-output)) 0))))

(defun valid-prop? (x)
  (let ((res (acl2s-query `(thm ,x))))
    (and (not (car res)) (second res))))

(defun valid-acl2s-term? (x)
  (let ((res (acl2s-query `(valid-acl2s-termp ',x state))))
    (and (not (car res)) (second res))))

(defun check-is-acl2s-expression (const-name)
  (if (valid-acl2s-term? (symbol-value const-name))
      '(t . "OK")
    (cons nil (format nil "~a doesn't appear to be a valid ACL2s expression." const-name))))

(defun formula-is-equivalent (vars orig const-name)
  (let ((res (itest?-query `(=> (and ,@(mapcar #'(lambda (v) (list 'boolp v)) vars))
                                (iff ,orig ,(symbol-value const-name))))))
    (if (not (car res))
        '(t . "OK")
      (cons nil (format nil "Your simplification is not equivalent to the original formula:~%try ~S" (second res))))))

;; Essentially this is a destructive in-place acons
;; It mutates the given alist by consing (cons key value) onto the front of it.
(defmacro push-onto-alist-entry! (key value alist)
  (let ((tmpvar (gensym)))
    `(let ((,tmpvar (assoc ,key ,alist)))
       (if ,tmpvar (push ,value (cdr ,tmpvar))
         (setf ,alist (acons ,key (list ,value) ,alist))))))

#|
(let ((abc nil))
  (push-onto-alist-entry! 'a 1 abc)
  (push-onto-alist-entry! 'a 2 abc)
  (push-onto-alist-entry! 'b 3 abc)
  abc)
|#

(defun get-instructor-fn-name (fn-name)
  (intern (concatenate 'string "INSTR-" (symbol-name fn-name)) (symbol-package fn-name)))

(defun get-definec-body (s)
   (trivia:match s
                 ((list* (or 'definec 'definecd) _ _ _ body) (car (last body)))
                 (otherwise nil)))

(defun top-level-form-is (expected name function-def-alist)
  (let ((fn-def-entry (assoc name function-def-alist :test 'equal)))
    (if (not fn-def-entry)
        (cons nil (format nil "Couldn't find the definition for ~a" name))
      (let ((body (get-definec-body (cdr fn-def-entry))))
        (cond ((not body) (cons nil (format nil "The body of the function ~a was empty" name)))
              ((equal (car body) expected) '(t "OK"))
              (t (cons nil (format nil "The body of function ~a was expected to start with the form ~a but instead we found ~a" name expected (car bodY)))))))))

(defun fdef-rules (fns)
  (match fns
   (() '())
   ((fn . rst) (cons (intern
                      (concatenate
                       'string (symbol-name fn) "-DEFINITION-RULE"))
                     (fdef-rules rst)))))

(defun check-property (num property-alist instr-property fnames
                           &optional (hyps nil))
  (let ((prop-entry (assoc-equal num property-alist)))
    (if (not prop-entry)
        (cons nil (format nil "Could not find a solution for Property ~a."  num))
      (let ((extract-res (acl2s-query `(extract-property ',(cddr prop-entry) state))))
        (if (car extract-res)
            (cons nil (format nil "Could not handle Property ~a" (cdr
                                                                  prop-entry)))
          (b* ((extract-res2 (acl2s-query `(extract-property ',instr-property
                                                             state)))
               ((when (car extract-res2)) (cons nil (format nil "Could not handle Instructor Property ~a" instr-property)))
               (instr-property (second extract-res2))
               (student-property (second extract-res))
               (disable-f-def-symbols (cons 'disable (fdef-rules fnames)))
               ;; This is so that if we crap out due to
               ;; prover-step-limit being reached, we'll be able to
               ;; tell
               ;; (- (save-result '(t nil)))
               (res (if hyps
                        (acl2s-query `(thm ;;:timeout 30
                                       (implies ,hyps
                                                (iff ,instr-property ,student-property)
                                                :hints (("Goal" :in-theory ,disable-f-def-symbols)))
                                       :ld-error-action :error
                                       :prover-step-limit 150000))
                      (acl2s-query `(thm ;;:timeout 30
                                     (iff ,instr-property ,student-property)
                                     :hints (("Goal" :in-theory ,disable-f-def-symbols)))
                                   :ld-error-action :error
                                   :prover-step-limit 150000)))
               (res (car res))
               (- (print res)))
            (if res
                '(nil . "Incorrect property")
              '(t . "Correct property"))))))))

(defun check-function-is-defined (name)
  (if (fboundp name)
      '(t . "OK")
    (cons nil (format nil "Function ~a is not defined!" name))))

(defun check-function-admitted (fn-name fn-trm function-def-alist expected-fn-form)
  (b* ((res (acl2s-compute fn-trm))
       ((when (car res)) `(nil . ,(format nil
                                          "Function ~a was not admitted."
                                          fn-name)))
       (student-fn-form
        (cdr (assoc-equal fn-name function-def-alist)))
       ((unless (equal student-fn-form expected-fn-form))
        `(nil . ,(format nil "Body of function ~a is not the same as given in
  HWK 5." fn-name))))
    `(t . ,(format nil "Function ~a was successfully admitted!" fn-name))))



