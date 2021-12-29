#|

 Copyright © 2021 by Pete Manolios 
 CS 4820 Fall 2021

 Homework 2.
 Due: 9/23 (Midnight)

 For this assignment, work in groups of 2-3. Send me exactly one
 solution per team and make sure to follow the submission instructions
 on the course Web page. In particular, make sure that the subject of
 your email submission is "CS 4820 HWK 2".

 The group members are:
 Ankit Kumar
 Nick Ding (put the names of the group members here)

|#

#|

 In this homework, we will explore the distinction between meta and
 object languages. See pg. 11 of the Harrison book.

 Here is a little story to set the context. In CS2800 and, briefly, in
 this class, we reviewed the syntax and semantics of the ACL2s
 language. What language did we use to do that? English. Technically,
 we would say that English is the metalanguage used to study the
 object language ACL2s. Notice "object" here has nothing to do with
 objects from object-oriented languages. More accurately, we might say
 mathematical English or CS English is the metalanguage, as fields
 such as mathematics and CS use a version of English better suited for
 their purposes, eg, in CS English, words like "implies," "tree,"
 "induction" and "reasoning" have special meanings.
 
 Mathematical English is not a formal language, hence, just like
 English, it is ambiguous (though not to the same extent) and one can
 take many shortcuts in describing things that require appropriate
 context or even guessing to understand.  What we're going to do in
 this homework is to use a formal language, ACL2s, as the metalanguage
 and we are going to define and reason about an arithmetic expression
 language that includes errors.

 We are also going to use a gradual verification methodology supported
 by ACL2s. This is ongoing, early stage work that allows us to design
 systems that get verified gradually. 

 One final note. We will see how to use ACL2s as both the metalanguage
 and the object language, in the extra credit problems, which I
 encourage you to do. 

|#

(in-package "ACL2S")

;; First, update ACL2s, by running the script clean-gen-acl2-acl2s.sh

#|
  
 Here is a short overview of gradual verification. ACL2s supports the
 design, development and verification of computational systems by
 allowing you to set certain parameters. There are four built-in
 configurations we are going to consider. In order of most to least
 permissive, they are:
 
 (modeling-start): This is the configuration you will start with. In
 this configuration, ACL2s will accept anything you tell it, unless it
 can very quickly find a counterexample. ACL2s will not prove
 termination, nor will it prove function and body contracts nor will
 it prove named properties (defthms).
 
 (modeling-validate-defs): In this configuration, ACL2s will now try
 proving termination and function/ body contracts, but if it can't it
 will accept your definitions anyway. It will also give itself a bit
 more time for testing.
 
 (modeling-admit-defs): In this configuration, ACL2s will require
 proofs of termination and function/body contracts. It will also give
 itself more time for testing.
 
 (modeling-admit-all): In this configuration, ACL2s will require
 proofs of properties. You can enable/disable proofs locally. More on
 that below.


|#

; Start with this configuration and once you are done with the
; homework, go to the next configuration, cranking up the rigor as far
; as you can. You must get to at least (modeling-validate-defs), but
; (modeling-admit-defs) is better and (modeling-admit-all) is the
; best.

(modeling-admit-all)

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
(defdata uoper (enum '(- /)))

;; Use defdata to define boper the binary operators

(defdata boper (enum '(+ - * / ^)))

(check= (boperp '*) t)
(check= (boperp '^) t)
(check= (boperp '!) nil)

;; Use defdata to define saexpr. We will want names for all the
;; subtypes, so use the following template. Note that data definitions
;; can be mutually recursive.

(defdata
  (saexpr (or rational  ; or is the same as oneof
              var
              usaexpr   ; unary sael expression
              bsaexpr)) ; binary sael expression
  (usaexpr (list uoper saexpr))
  (bsaexpr (list saexpr boper saexpr)))

;; We now have a formal definition of the SAEL language!  That is, we
;; have a recognizer saexprp, which is defined in our metalanguage,
;; ACL2s, and which recognizes expression in the object language,
;; SAEL. We formalized the syntax of SAEL expressions.

(check= (saexprp '((x + y) - (/ z))) t)
(check= (saexprp '(x + y + z)) nil)

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

(definecd lookup (v :var a :assignment) :rational
  (match a
    (() 1)
    (((x . val) . &) (if (== x v) val
                       (lookup v (cdr a))))
    (& (lookup v (cdr a)))))

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

(definec saeval (e :saexpr a :assignment) :rat-err
  (match e
    (:rational e)
    (:var (lookup e a))
    (:usaexpr
     (let ((v (saeval (second e) a)))
       (if (== v *er*)
           *er*
         (match e
           (('- &) (- 0 v))
           (('/ &) (if (== v 0)
                       *er*
                     (/ 1 v)))))))
    (:bsaexpr
     (let ((v1 (saeval (first e) a)))
       (if (== v1 *er*)
           *er*
         (let ((v2 (saeval (third e) a)))
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







; Define, m-ack, a measure function for ack.
; Q2a. We are using the definition on page 129

; Note: fill in the ...'s above, as you can use the generalized
; measure functions, as mentioned in Section 5.5.

; Q2b. Fill in the definition
(definec m-ack (n :nat m :nat) :lex
  `(1 ,n ,(+ n m)))



; Q2c
(property (n :nat m :nat)
	  (=> (and (! (zp n)) (zp m))
	      (d< (m-ack (1- n) 1) (m-ack n m))))

(property (n :nat m :nat)
	  (=> (and (! (zp n)) (! (zp m)))
	      (d< (m-ack n (1- m)) (m-ack n m))))
	      
(property (n :nat m :nat)
	  (=> (and (! (zp n)) (! (zp m)))
	      (d< (m-ack (1- n) (ack n (1- m))) (m-ack n m))))
  



