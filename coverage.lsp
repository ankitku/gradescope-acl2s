(in-package "ACL2S")
:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :trivia)
(ql:quickload :let-plus)
(ql:quickload :cl-fad)
(require :sb-cover)
(import 'let-plus::(let+ &values))

(defun definec-args-to-names (args)
  (loop for arg in args by #'cddr collect arg))

(defun replace-function-calls (x old new)
  (trivia:match x
    ((list* 'quote _) x)
    ((list* 'match on cases)
     `(match ,(replace-function-calls on old new)
        ,@(mapcar #'(lambda (match-case) (list (first match-case) (replace-function-calls (second match-case) old new))) cases)))
    ((list* 'cond cases)
     (cons 'cond
           (mapcar #'(lambda (cond-case) (list (replace-function-calls (first cond-case) old new)
                                               (replace-function-calls (second cond-case) old new)))
                   cases)))
    ((list* fn args)
     (cons (if (equal fn old) new fn)
           (mapcar #'(lambda (arg) (replace-function-calls arg old new)) args)))
    (otherwise x)))

(defun definec-to-defun (s &key (prefix "TRANSLATED-"))
  (trivia:match s
    ((list* (or 'definec 'definecd) name args ret-ty body)
     (b* ((new-name (intern (concatenate 'string prefix (symbol-name name))))
          (ic (if (in (car body) '(:ic :pre))
                  (second body)
                nil)))
       (values `(defun ,new-name ,(definec-args-to-names args)
                  ;;(declare (optimize sb-cover:store-coverage-data))
                  ,@(mapcar #'(lambda (form) (replace-function-calls form name new-name)) (last body)))
               new-name
               ic)))
    (otherwise nil)))

#|
(definec-to-defun '(definec del (e :all l :tl) :tl
                     (match l
                       (nil nil)
                       ((!e . r) (del e r))
                       ((f . r) (cons f (del e r))))))

(definec-to-defun '(definecd del (e :all l :tl) :tl
                     :ic (in e l)
                     :oc (tlp (del e l))
                     (match l
                       (nil nil)
                       ((!e . r) (del e r))
                       ((f . r) (cons f (del e r))))))


(multiple-value-list (definec-to-defun '(definecd del (e :all l :tl) :tl
                     :ic (in e l)
                     :oc (tlp (del e l))
                     (match l
                       (nil nil)
                       ((!e . r) (del e r))
                       ((f . r) (cons f (del e r))))) :prefix "COVERAGE-"))
|#

(defun translate-check= (check=)
  (assert (equal (car check=) 'check=))
  (list 'equal (second check=) (third check=)))

(defun get-function-coverage (definec check=s)
  (let ((res (multiple-value-list (definec-to-defun definec :prefix "COVERAGE-"))))
    (if (not (car res))
        (error "Couldn't translate ~S." definec)
      (let ((name (second res))
            (translated-defun (car res))
            (f (cl-fad:open-temporary :direction :output :template "%.lisp")))
        (write '(in-package "ACL2S") :stream f)
        (write translated-defun :stream f)
        (loop for check= in check=s
              do (write (replace-function-calls (translate-check= check=) (second definec) name) :stream f))
        (close f)
        (with-compilation-unit (:policy '(optimize sb-cover:store-coverage-data))
                               (let+ (((&values compiled-name warningsp failurep) (compile-file (namestring (pathname f)))))
                                 (load (namestring compiled-name)))
                               (sb-cover::refresh-coverage-info (namestring (pathname f)))
                               (let+ (((&values summary non-executed) (coverage-summary-for-file (namestring (pathname f)))))
                                 (delete-file f)
                                 (values summary non-executed)))))))

#|
(translate-and-compile-definecs-for-coverage
 '((definec del (e :all l :tl) :tl
                     (match l
                       (nil nil)
                       ((!e . r) (del e r))
                       ((f . r) (cons f (del e r)))))))
|#

(defun coverage-summary (file counts states source)
  (loop for mode in '(:expression :branch)
        collect (let ((count (getf counts mode)))
                  `(,mode (:covered . ,(sb-cover::ok-of count))
                          (:all . ,(sb-cover::all-of count))
                          (:percent . ,(sb-cover::percent count))))))

;;(coverage-del 'a '(b a b a a c))
;;(coverage-del 'a nil)

(defun state-not-executedp (state)
  (or (equal state 2)
      (equal state 6)
      (equal state 10)))

;; adapted from sbcl/contrib/sb-cover/cover.lisp
(defun coverage-report-not-executed (file counts states source)
  (let ((current-acc nil)
        (res nil))
    (loop for last-state = nil then state
          with line = 1
          for col from 1
          for char across source
          for state across states
          do (when (equal char #\Newline)
               (setf state nil)
               (setf col 0)
               (incf line))
          do (unless (eq state last-state)
               (when (and last-state (state-not-executedp last-state))
                 (push current-acc res)
                 (setf current-acc nil)))
          do (when (state-not-executedp state)
               ;;(when (null current-acc)
               ;;  (push (cons line col) current-acc))
               (push char current-acc)))
    (when current-acc (push current-acc res))
    (mapcar #'(lambda (acc) (format nil "~{~A~}" (reverse acc))) (reverse res))))

;; adapted from sbcl/contrib/sb-cover/cover.lisp
(defun coverage-summary-for-file (file &key (external-format :default))
  (let* ((source (sb-cover::detabify (sb-cover::read-file file external-format)))
         (states (make-array (length source)
                             :initial-element 0
                             :element-type '(unsigned-byte 4)))
         (hashtable (sb-cover::code-coverage-hashtable))
         ;; Convert the code coverage records to a more suitable format
         ;; for this function.
         (expr-records (sb-cover::convert-records (gethash file hashtable) :expression))
         (branch-records (sb-cover::convert-records (gethash file hashtable) :branch))
         ;; Cache the source-maps
         (maps (with-input-from-string (stream source)
                 (loop with sb-cover::*current-package* = (find-package "ACL2S")
                       with map = nil
                       with form = nil
                       with eof = nil
                       for i from 0
                       do (setf (values form map)
                                (handler-case
                                    (sb-cover::read-and-record-source-map stream)
                                  (end-of-file ()
                                    (setf eof t))
                                  (error (error)
                                    (warn "Error when recording source map for toplevel form ~A:~%  ~A" i error)
                                    (values nil
                                            (make-hash-table)))))
                       until eof
                       when map
                       collect (cons form map)))))
    (mapcar (lambda (map)
              (maphash (lambda (k locations)
                         (declare (ignore k))
                         (dolist (location locations)
                           (destructuring-bind (start end suppress) location
                             (when suppress
                               (sb-cover::fill-with-state source states 15 (1- start)
                                                          end)))))
                       (cdr map)))
            maps)
    ;; Go through all records, find the matching source in the file,
    ;; and update STATES to contain the state of the record in the
    ;; indexes matching the source location. We do this in two stages:
    ;; the first stage records the character ranges, and the second stage
    ;; does the update, in order from shortest to longest ranges. This
    ;; ensures that for each index in STATES will reflect the state of
    ;; the innermost containing form.
    (let ((counts (list :branch (make-instance 'sb-cover::sample-count :mode :branch)
                        :expression (make-instance 'sb-cover::sample-count
                                                   :mode :expression))))
      (let ((records (append branch-records expr-records))
            (locations nil))
        (dolist (record records)
          (destructuring-bind (mode path state) record
            (let* ((path (reverse path))
                   (tlf (car path))
                   (source-form (car (nth tlf maps)))
                   (source-map (cdr (nth tlf maps)))
                   (source-path (cdr path)))
              (cond ((eql mode :branch)
                     (let ((count (getf counts :branch)))
                       ;; For branches mode each record accounts for two paths
                       (incf (sb-cover::ok-of count)
                             (ecase state
                               (5 2)
                               ((6 9) 1)
                               (10 0)))
                       (incf (sb-cover::all-of count) 2)))
                    (t
                     (let ((count (getf counts :expression)))
                       (when (eql state 1)
                         (incf (sb-cover::ok-of count)))
                       (incf (sb-cover::all-of count)))))
              (if source-map
                  (handler-case
                      (multiple-value-bind (start end)
                          (sb-cover::source-path-source-position (cons 0 source-path)
                                                                 source-form
                                                                 source-map)
                        (push (list start end source state source-path) locations))
                    (error ()
                      (warn "Error finding source location for source path ~A in file ~A~%" source-path file)))
                  (warn "Unable to find a source map for toplevel form ~A in file ~A~%" tlf file)))))
        ;; Now process the locations, from the shortest range to the longest
        ;; one. If two locations have the same range, the one with the higher
        ;; state takes precedence. The latter condition ensures that if
        ;; there are both normal- and a branch-states for the same form,
        ;; the branch-state will be used.
        #|
        ;; could use the source-path value to try and provide more useful output
        ;; when showing which expressions have not been executed
        (loop for location in locations
              do (when (state-not-executedp (fourth location))
                   (print (fifth location))
                   (print (subseq (third location) (first location) (second location)))))
        |#
        (setf locations (sort locations #'> :key #'fourth))
        (dolist (location ;;(stable-sort locations #'<
                 (sort locations #'<
                                       :key (lambda (location)
                                              (- (second location)
                                                 (first location)))))
          (destructuring-bind (start end source state source-path) location
            (sb-cover::fill-with-state source states state start end))))
      (values (coverage-summary file counts states source)
              (coverage-report-not-executed file counts states source)))))

(get-function-coverage '(definec foo (x :tl) :nat (match x (nil 0) ((& . r) (1+ (foo r))))) '((check= (foo nil) 0) (check= (foo '(1 2 3)) 3)))
