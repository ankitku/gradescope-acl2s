(load "coverage.lsp")

;; Check that the user's check= forms result in 100% expression
;; coverage for the given function.
(defun check-expression-coverage (name function-def-alist test-alist)
  (let ((tests-entry (assoc-equal name test-alist))
        (fn-def-entry (assoc-equal name function-def-alist)))
    (cond ((not tests-entry)
           (cons nil (format nil "No check=? forms were found after the definition of ~a." name)))
          ((not fn-def-entry)
           (cons nil (format nil "Couldn't find the definition for ~a" name)))
          (t
           (let+ (((&values summary non-executed) (get-function-coverage (cdr fn-def-entry) (cdr tests-entry)))
                  (expr-pct (cdr (assoc :percent (cdr (assoc :expression summary)))))
                  (gen-name (intern (concatenate 'string "COVERAGE-" (symbol-name name)))))
             (if (< expr-pct 100)
                   (cons nil (format nil "Achieved ~a% expression coverage - you need to cover the subexpressions ~a" expr-pct (mapcar #'(lambda (non-exec-form) (cl-ppcre:regex-replace-all (symbol-name gen-name) non-exec-form (symbol-name name))) non-executed)))
               (cons t "100% expression coverage.")))))))
