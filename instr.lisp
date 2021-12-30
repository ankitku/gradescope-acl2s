#|

A sample homework, contaning instructor versions of solutions.

Problems have been picked from Pete Monolios's class on Computer Aided Reasoning.

|#




#|

 We will define the syntax and semantics for SAEL, the Simple
 Arithmetic Expression Language. So, SAEL is our object language. The
 metalanguage we (you) will use to define SAEL is ACL2s. The
 meta-metalanguage we (I, in this assignment) use to discuss all of
 this is Mathematical English.

 A simple arithmetic expression, saexpr, is one of the following:

 - a rational number (we use the builtin ACL2s type rational)

 - a variable (we use the builtin ACL2s type var)

 - a list of the form 
   
   (- <saexpr>)
   (/ <saexpr>)

   where <saexpr> is an arithmetic expression

 - a list of the form

   (<saexpr> <oper> <saexpr>)

   where <oper> is one of +, -, *, / or ^ 
   and both <saexpr>'s are simple arithmetic expressions.

|#

; It really isn't important to know the exact definition of varp,
; although you can query ACL2s with ":pe varp". Just notice that none
; of the operators are vars, but symbols such as x, y, z, etc are
; vars.

(check= (varp '-) nil)
(check= (varp '+) nil)
(check= (varp '*) nil)
(check= (varp '/) nil)
(check= (varp '^) nil)
(check= (varp 'x) t)

;; Use defdata to define the unary operators. Fill in the ...s
;; below. If you have questions, ask on Piazza.
(defdata instr-uoper (enum '(- /)))

;; Use defdata to define boper the binary operators

(defdata instr-boper (enum '(+ - * / ^)))

(check= (instr-boperp '*) t)
(check= (instr-boperp '^) t)
(check= (instr-boperp '!) nil)

;; Use defdata to define saexpr. We will want names for all the
;; subtypes, so use the following template. Note that data definitions
;; can be mutually recursive.

(defdata
  (instr-saexpr (or rational  ; or is the same as oneof
              var
              usaexpr   ; unary sael expression
              bsaexpr)) ; binary sael expression
  (usaexpr (list instr-uoper instr-saexpr))
  (bsaexpr (list instr-saexpr instr-boper instr-saexpr)))

;; We now have a formal definition of the SAEL language!  That is, we
;; have a recognizer saexprp, which is defined in our metalanguage,
;; ACL2s, and which recognizes expression in the object language,
;; SAEL. We formalized the syntax of SAEL expressions.

(check= (instr-saexprp '((x + y) - (/ z))) t)
(check= (instr-saexprp '(x + y + z)) nil)

;; We are now going to define the semantics of SAEL expressions.

;; First, some helper definitions.
;; An assignment is an alist from var's to rationals.
;; An alist is just a list of conses.
;; In my RAP notes and in the Harrison book, the term "valuation" is
;; also used instead of assignment.

(defdata assignment (alistof var rational))

(check= (assignmentp '((x . 1) (y . 1/2))) t)

;; This is nil because (1), (1/2) are not rationals.
(check= (assignmentp '((x 1) (y 1/2))) nil)

;; Now, on to the semantics.

;; How do we assign semantics to SAEL expressions such as
;; (x + y)? 

;; We will define saeval (below), a function that given an saexpr and
;; an assignment evaluates the expression, using the assignment to
;; assign values to var's.

;; If a var appears in the saexpr but not in the assignment, then
;; the value of the var should be 1.

;; Use the following helper function to lookup the value of v in a
;; (remember return 1 if v isn't in a)

(definecd instr-lookup (v :var a :assignment) :rational
  (cond
    ((endp a) 1)
    ((equal (caar a) v) (cdar a)))
    (t (instr-lookup v (cdr a)))))

;; What happens when we divide by 0? We are going to throw an
;; error. So, we will model that as follows.

;; A singleton type
(defdata er 'error)

;; *er* is defined using the enumerator for er, which always retuns
;; 'error; if we change er, we will not have to change *er*. Instead
;; of using the symbol 'error, use the constant *er* in the code you
;; write. 
(defconst *er* (nth-er-builtin 0))

;; Since the evaluation of arithmetic expressions can yield errors, we
;; will define the type that evaluation can return to include errors.
(defdata rat-err (oneof rational er))

;; There are some decisions to be made.

;; First: if an error occurs during evaluation, then you must return
;; the error; there is no mechanism for masking errors.

;; Second: If you divide by 0, that is an error.

;; Third: How do we define exponentiation? For example, do we allow
;; (-1 ^ 1/2) or (2 ^ 1/2) or (0 ^ -1)? All three are problematic, for
;; different reasons. In the first case, the result is i, but we only
;; allow rationals, not complex numbers. In the second case, we get an
;; irrational. In the third case, ^ is undefined, as it would require
;; us to divide by 0.

;; To stay in the realm of rationals, given an expression of the form
;; (x ^ y), we return an error if (1) y is not an integer or (2) x=0
;; and y<0.

;; By the way, the builtin exponentiation function in ACL2s is expt.

;; Feel free to define helper functions as needed.


(definec instr-saeval (e :instr-saexpr a :assignment) :rat-err
  (match e
    (:rational e)
    (:var (instr-lookup e a))
    (:usaexpr
     (let ((v (instr-saeval (second e) a)))
       (if (== v *er*)
           *er*
         (match e
           (('- &) (- 0 v))
           (('/ &) (if (== v 0)
                       *er*
                     (/ 1 v)))))))
    (:bsaexpr
     (let ((v1 (instr-saeval (first e) a)))
       (if (== v1 *er*)
           *er*
         (let ((v2 (instr-saeval (third e) a)))
           (if (== v2 *er*)
               *er*
             (match e
               ((& '+ &) (+ v1 v2))
               ((& '- &) (- v1 v2))
               ((& '* &) (* v1 v2))
               ((& '^ &) (cond
                          ((^ (== v1 0) (< v2 0)) *er*)
                          ((! (integerp v2)) *er*)
                          (t (expt v1 v2))))
               ((& '/ &) (if (== v2 0)
                             *er*
                           (/ v1 v2)))))))))))











; Configuration: update as per instructions
(modeling-admit-all)

#|

Q2. Consider the following definition.

|#

(definec ack (n :nat m :nat) :pos
  :skip-tests t ; ack is slow, so skip testing
  (cond ((zp n) (1+ m))
        ((zp m) (ack (1- n) 1))
        (t (ack (1- n) (ack n (1- m))))))


#| 

 ACL2s accepts the definition, but your job is to come up with a
 measure function and ACL2s proofs of the corresponding proof
 obligations. 

|#

; Define, m-ack, a measure function for ack.
; Q2a. We are using the definition on page 129

; Note: fill in the ...'s above, as you can use the generalized
; measure functions, as mentioned in Section 5.5.

; Q2b. Fill in the definition
(definec instr-m-ack (n :nat m :nat) :lex
  `(1 ,n ,(+ n m)))
