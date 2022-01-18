#|

CS 2800 Homework 1 - Spring 2020

This homework is done in groups. 

 * Groups should consist of 2-3 people.

 * All students should register for the class in the handin server at
     http://handins.ccs.neu.edu/courses

 * If you have never used the handin server, you can read about it here:
     http://www.ccs.neu.edu/home/blerner/handin-server/handin-server-guide.html

 * Students should set up their team with the handin server.

 * Submit the homework file (this file) on the homework sever.

 * You must list the names of ALL group members below, using the given
   format. This way we can confirm team membership with the handin
   server teams. If you fail to follow these instructions, it costs us
   time and it will cost you points, so please read carefully.

The format should be: FirstName1 LastName1, FirstName2 LastName2, ...
For example:
Names of ALL group members: Frank Sinatra, Billy Holiday

There will be a 10 pt penalty if your names do not follow this format.

Replace "..." below with the names as explained above.

Names of ALL group members: ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 For this homework you will need to use ACL2s.

 Technical instructions:

 - Open this file in ACL2s as hwk01.lisp

 - Make sure you are in ACL2s mode. This is essential! Note that you can
   only change the mode when the session is not running, so set the correct
   mode before starting the session.

 - Insert your solutions into this file where indicated (usually as "...")

 - Only add to the file. Do not remove or comment out anything pre-existing.

 - Make sure the entire file is accepted by ACL2s. In particular, there must
   be no "..." left in the code. If you don't finish all problems, comment
   the unfinished ones out. Comments should also be used for any English
   text that you may add. This file already contains many comments, so you
   can see what the syntax is.

 - When done, save your file and submit it as hwk01.lisp

 - Do not submit the session file (which shows your interaction with the theorem
   prover). This is not part of your solution. Only submit the lisp file.

 Instructions for programming problems:

 For each function definition, you must provide both contracts and a body.

 You must also ALWAYS supply your own tests. This is in addition to the
 tests sometimes provided. Make sure you produce sufficiently many new test
 cases. This means: cover at least the possible scenarios according to the
 data definitions of the involved types. For example, a function taking two
 lists should have at least 4 tests: all combinations of each list being
 empty and non-empty.

 Beyond that, the number of tests should reflect the difficulty of the
 function. For very simple ones, the above coverage of the data definition
 cases may be sufficient. For complex functions with numerical output, you
 want to test whether it produces the correct output on a reasonable
 number of inputs.

 Use good judgment. For unreasonably few test cases we will deduct points.

 We will use ACL2s' check= function for tests. This is a two-argument
 function that rejects two inputs that do not evaluate equal. You can think
 of check= roughly as defined like this:

 (definec check= (x :all y :all) :bool
   :input-contract (equal x y)
   :output-contract (== (check= x y) t)
   t)

 That is, check= only accepts two inputs with equal value. For such inputs, t
 (or "pass") is returned. For other inputs, you get an error. If any
 check= test in your file does not pass, your file will be rejected.

 You can use any types, functions and macros listed on the ACL2s
 Language Reference (from class Webpage, click on "Lectures and Notes"
 and then on "ACL2s Language Reference").

 Since this is our first programming exercise, we will simplify the
 interaction with ACL2s somewhat. Instead of requiring ACL2s to prove
 termination and contracts, we allow ACL2s to proceed even if a proof
 fails.  However, if a counterexample is found, ACL2s will report it.
 See the lecture notes for more information.  This is achieved using
 the following directives (do not remove them):

|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

#| 

 Part I:
 Function definitions.

 In this part, you will be given a set of programming exercises. Since you
 already took a fantastic course on programming (CS2500), the problems are not
 entirely trivial. Make sure you give yourselves enough time to develop
 solutions and feel free to define helper functions as needed.

|# 

#|
Define the function rr.
rr: Nat x TL -> TL

(rr n l) rotates the true list l to the right n times.
|#

(definec instr-rr (n :nat l :tl) :tl
  (if (endp l)
      '()
    (b* ((n (mod n (len l)))
         (m (- (len l) n)))
      (app (nthcdr m l)
           (take m l)))))

(check= (instr-rr 1 '(1 2 3)) '(3 1 2))
(check= (instr-rr 2 '(1 2 3)) '(2 3 1))
(check= (instr-rr 3 '(1 2 3)) '(1 2 3))

#|
Define the function err, an efficient version of rr.
err: Nat x TL -> TL

(err n l) returns the list obtained by rotating l to the right n times
but it does this efficiently because it actually never rotates more than
(len l) times.

|#

(definec instr-err (n :nat l :tl) :tl
  (if (endp l)
      '()
    (b* ((n (mod n (len l)))
         (m (- (len l) n)))
      (app (nthcdr m l)
           (take m l)))))

(check= (instr-err 1 '(1 2 3)) '(3 1 2))
(check= (instr-err 2 '(1 2 3)) '(2 3 1))
(check= (instr-err 3 '(1 2 3)) '(1 2 3))

#|
Make sure that err is efficient by timing it with a large n
and comparing the time with rr.

Replace the ...'s in the string below with the times you 
observed.

These aren't handled by autograder, signature mismatch
(time$ (instr-rr  10000000 '(a b c d e f g)))
(time$ (instr-err 10000000 '(a b c d e f g)))

"rr took 0.00 seconds but err took 0.00 seconds"             
|#
;; Here is a data definition for a bitvector.

(defdata bit (oneof 0 1))
(defdata bitvector (listof bit))

;; For example

(check= (bitvectorp '(0 1 0 0 1 0 0)) t)
(check= (bitvectorp '(1 a 1 0)) nil)

#|
We can use bitvectors to represent natural numbers as follows.
The list

(0 0 1)

corresponds to the number 4 because the first 0 is the "low-order" bit
of the number which means it corresponds to 2^0=1 if the bit is 1 and
0 otherwise. The next Boolean corresponds to 2^1=2 if 1 and 0
otherwise and so on. So the above number is:

0 + 0 + 2^2 = 4.

As another example, 31 is

(1 1 1 1 1)

or

(1 1 1 1 1 0 0)

or ...

Define the function n-to-b that given a natural number, returns a
bitvector list of minimal length, corresponding to that number.
|#

(definec instr-n-to-b (n :nat) :bitvector
  (if (zp n)
      '()
    (cons (mod n 2) (instr-n-to-b (floor n 2)))))

(check= (instr-n-to-b 0)  '())
(check= (instr-n-to-b 7)  '(1 1 1)) 
(check= (instr-n-to-b 10) '(0 1 0 1)) 

#|
Define the function b-to-n that given a bitvector, returns
the  corresponding natural number.
|#

(definec instr-b-to-n (b :bitvector) :nat
  (if (endp b) 0
    (+ (car b) (* 2 (instr-b-to-n (cdr b))))))

(check= (instr-b-to-n '(0 1 0 1)) 10)  
(check= (instr-b-to-n '(1 1 1)) 7) 
(check= (instr-b-to-n '(0 0 1)) 4) 
(check= (instr-b-to-n '(1 1 1 1 1)) 31) 
(check= (instr-b-to-n ()) 0)

#|

The permutations of a (true) list are all the lists you can obtain by
swapping any two of its elements (repeatedly). For example, starting
with the list

(1 2 3)

I can obtain

(3 2 1)

by swapping 1 and 3.

So the permutations of (1 2 3) are

(1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)

Notice that if the list is of length n and all of its elements are distinct, it
has n! (n factorial) permutations.

Given a list, say (a b c d e), we can define any of its permutations using a
list of *distinct* natural numbers from 0 to the length of the list - 1, which
tell us how to reorder the elements of the list.  Let us call this list of
distinct natural numbers an arrangement.  For example applying the
arrangement (4 0 2 1 3) to the list (a b c d e) yields (e a c b d).

Define the function find-arrangement that given two lists, either returns nil if
they are not permutations of one another or returns an arrangement such that
applying the arrangement to the first list yields the second list. Note that if
the lists have repeated elements, then more than one arrangement will work. In
such cases, it does not matter which of these arrangements you return.



(definec list->numberedlist (ls :tl n :nat) :tl
  (if (endp ls)
      '()
    (cons `(,(car ls) . ,n)
          (list->numberedlist (cdr ls) (1+ n)))))

(defdata pn (alistof all nat))
(defdata lon (listof nat))
            

(definec find-arrangement (ls :tl) :tl
  (b* ((init (list->numberedlist ls 0))

(check= (find-arrangement '(a b c) '(a b b)) nil)
(check= (find-arrangement '(a b c) '(a c b)) '(0 2 1))
(check= (find-arrangement '(a a) '(a a)) '(0 1))
;; in the above check= you can use '(1 0) instead of '(0 1) if you wish
;; since both arrangements work
|#

#|

Recall the following definitions from the lecture notes.

(definec listp (l :all) :bool 
  (or (consp l)
      (equal l () )))

(definec endp (l :list) :bool
  (atom l))

(definec true-listp (l :all) :bool
  (if (consp l)
      (true-listp (cdr l))
    (equal l ())))

(definec binary-append (x :tl y :all) :all
  (if (endp x)
      y
    (cons (first x) (binary-append (rest x) y))))

This exercise will require you to use what you learned solving
recurrence relations from discrete.

We will explore the difference between static and dynamic
type checking. 

A. What is the computational complexity of listp?

B. What is the computational complexity of endp?

C. What is the computational complexity of true-listp?

D. What is the computational complexity of binary-append?

To answer the above questions, we will assume (just for this exercise)
that the following operations have a cost of 1:

 cons, first, rest, consp, atom, or, equal, not, if

We will also assume for this first set of questions that static
contract checking is used.  With static contract checking, ACL2s
checks that the arguments to the function satisfy their contract only
once, for the top-level call. For example, if you type:

(binary-append '(1 2 3 4) '(5 6))

ACL2s checks that '(1 2 3 4) is a true-list and no other contracts.

Remember also that we want the worst-case complexity.  So if the
function has an if-then-else, you must compute separately the
complexity of the then branch, the else branch, and then take the
worst case (i.e., the maximum), plus the complexity of the if
condition itself.

To get you going, we will give the complexity of listp as an example.
Checking the contract statically takes no time, since the type of

x is :all. 

Independently of the size of x, there are 3 operations: (consp x),
(equal x nil), and the or. So the complexity is O(3)=O(1), that is,
constant time. 

Notice that we want the complexity of the functions assuming that the
top-level checking has been already been done.

ANSWER B: ...

ANSWER C: ...

ANSWER D: ...

One way of implementing dynamic checking is to have every function
dynamically check its input contracts. Think about how you might do
that before reading further. So, the above definitions get transformed
into the following. In essence, we are forcing contract checking to
happen dynamically, during runtime.

(definec listp (x :all) :bool
  (or (consp x)
      (equal x nil)))

(definec endp (x :all) :bool
  (if (listp x)
      (atom x)
    (error)))

(definec true-listp (x :all) :bool
  (if (consp x)
      (true-listp (rest x))
    (equal x nil)))

(definec binary-append (x :tl y :all) :all
  (if (true-listp x)
      (if (endp x)
          y
        (cons (first x) (binary-append (rest x) y)))
    (error)))


What is the computational complexity of the above modified functions?
You can assume that error has cost 1.

ANSWER A: ... 

ANSWER B: ...

ANSWER C: ...

ANSWER D: ...

|#

(defconst *instr-complexities* '((A . (O 1)) (B . (O 1)) (C . (O (len x))) (D . (O (len x)))))
