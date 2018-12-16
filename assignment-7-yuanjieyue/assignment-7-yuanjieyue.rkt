;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment-7-yuanjieyue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;; Assignment 7

;;;; S-Expressions

;; 1. Provide the deconstructor template for SExp

;;;; Data Definition
;; An S-Expression (SExp) is one of 
;; - Atom 
;; - List<SExp>
;; INTERP: represents an s-expression

;; Deconstructor Template
;; sexp-fn: SExp -> ???
#; (define (sexp-fn sexp)
     (cond
       [(is-atom? sexp) ... (atom-fn atom) ...]
       [(list? sexp) ... (losexp-fn sexp) ...]))

;; Deconstuctor Template
;; losexp-fn: LOSExp -> ???
#; (define (losexp-fn losexp)
     (cond
       [(empty? losexp) ...]
       [(cons? losexp) ... (sexp-fn (first losexp)) ...
                       ... (losexp-fn (rest losexp)) ...]))
                                    
(define ATOM-1 0)
(define ATOM-2 "aa")
(define ATOM-3 'red)
(define ATOM-4 #true)
(define ATOM-5 3)
(define ATOM-6 4)
(define LOSEXP-MT empty)
(define LOSEXP-1 (list ATOM-1))
(define LOSEXP-2 (list ATOM-1 ATOM-2 ATOM-3 ATOM-4))
(define LOSEXP-3 (list LOSEXP-1 ATOM-1 ATOM-2 LOSEXP-2 'green #false))
(define LOSEXP-4 (list LOSEXP-1 ATOM-3 ATOM-3 'yellow LOSEXP-2 'green #false))
(define LOSEXP-5 (list LOSEXP-1 ATOM-5 ATOM-6 'yellow LOSEXP-2 11 #false))
(define LOSEXP-6 (list LOSEXP-1 ATOM-2 "bb" ATOM-2 LOSEXP-2 "bb"))

(define SEXP-1 ATOM-1)
(define SEXP-2 ATOM-2)
(define SEXP-3 ATOM-3)
(define SEXP-4 ATOM-4)
(define SEXP-5 LOSEXP-MT)
(define SEXP-6 LOSEXP-1)
(define SEXP-7 LOSEXP-2)
(define SEXP-8 LOSEXP-3)
(define SEXP-9 LOSEXP-4)
(define SEXP-10 LOSEXP-5)
(define SEXP-11 LOSEXP-6)

;;;; Signature
;; is-atom? : SExp -> Boolean
;;;; Purpose
;; GIVEN: an s-expression
;; RETURNS: true if the s-expression is an atom
;;          false otherwise
;;;; Function Definition
(define (is-atom? sexp)
  (or (number? sexp)
      (string? sexp)
      (symbol? sexp)
      (boolean? sexp)))
;;;; Tests
(check-expect (is-atom? SEXP-1)
              #true)
(check-expect (is-atom? SEXP-2)
              #true)
(check-expect (is-atom? SEXP-3)
              #true)
(check-expect (is-atom? SEXP-4)
              #true)
(check-expect (is-atom? SEXP-5)
              #false)
(check-expect (is-atom? SEXP-6)
              #false)
(check-expect (is-atom? SEXP-7)
              #false)
(check-expect (is-atom? SEXP-8)
              #false)

;; An Atom is one of 
;; - Number
;; - String 
;; - Symbol 
;; - Boolean 
;; INTERP: represents atomic values

;; Deconstructor Template
;; atom-fn : Atom -> ???
#; (define (atom-fn atom)
     (cond
       [(number? atom) ...]
       [(string? atom) ...]
       [(symbol? atom) ...]
       [(boolean? atom) ...]))

;; 2. Design a function that given an SExp returns the total number of Atoms
;;    in the SExp.

;;;; Signature
;; num-of-atoms: SExp -> NonNegInteger
;;;; Purpose
;; GIVEN: an s-expression
;; RETURNS: the total number of atoms in the expression
;;;; Function Definition
(define (num-of-atoms sexp)
  (cond
    [(is-atom? sexp) 1]
    [(list? sexp) (local (;;;; Signature
                          ;; num-of-atoms-losexp: LOSExp -> NonNegInteger
                          ;;;; Purpose
                          ;; GIVEN: a list of s-expressions
                          ;; RETURNS: the total number of atoms in this list of s-expressions

                          ;;;; Function Definition
                          (define (num-of-atoms-losexp losexp)
                            (cond
                              [(empty? losexp) 0]
                              [(cons? losexp) (+ (num-of-atoms (first losexp))
                                                 (num-of-atoms-losexp (rest losexp)))])))
                    (num-of-atoms-losexp sexp))]))
        
;;;; Tests
(check-expect (num-of-atoms SEXP-1)
              1)
(check-expect (num-of-atoms SEXP-2)
              1)
(check-expect (num-of-atoms SEXP-3)
              1)
(check-expect (num-of-atoms SEXP-4)
              1)
(check-expect (num-of-atoms SEXP-5)
              0)
(check-expect (num-of-atoms SEXP-6)
              1)
(check-expect (num-of-atoms SEXP-7)
              4)
(check-expect (num-of-atoms SEXP-8)
              9)

;; 3. Design a function that given an SExp returns a list of all Symbols in
;;    the SExp.

;;;; Date Definition
;; A List Of Symbols (LoS) is one of
;; - empty
;; - (cons Symbol Los)
;; INTERP: represents a list of symbols
(define LOS-MT empty)

;;;; Signature
;; symbols-in-sexp: SExp -> LoS
;;;; Purpose
;; GIVEN: an s-expression
;; RETURNS: a list of all symbols in this s-expression

;;;; Function Definition
(define (symbols-in-sexp sexp)
  (cond
    [(is-atom? sexp) (if (symbol? sexp)
                         (cons sexp empty)
                         empty)]
    [(list? sexp) (local (;;;; Signature
                          ;; symbols-in-losexp: LOSExp -> LoS
                          ;;;; Purpose
                          ;; GIVEN: a list of s-expressions
                          ; RETURNS: a list of all symbols in this list of s-expressions

                          ;;;; Function Definiton
                          (define (symbols-in-losexp losexp)
                            (cond
                              [(empty? losexp) empty]
                              [(cons? losexp) (append (symbols-in-sexp (first losexp))
                                                      (symbols-in-losexp (rest losexp)))])))
                    (symbols-in-losexp sexp))]))


;;;; Tests
(check-expect (symbols-in-sexp SEXP-1)
              LOS-MT)
(check-expect (symbols-in-sexp SEXP-2)
              LOS-MT)
(check-expect (symbols-in-sexp SEXP-3)
              (list 'red))
(check-expect (symbols-in-sexp SEXP-4)
              LOS-MT)
(check-expect (symbols-in-sexp SEXP-5)
              LOS-MT)
(check-expect (symbols-in-sexp SEXP-6)
              LOS-MT)
(check-expect (symbols-in-sexp SEXP-7)
              (list 'red))
(check-expect (symbols-in-sexp SEXP-8)
              (list 'red 'green))
(check-expect (symbols-in-sexp SEXP-9)
              (list 'red 'red 'yellow 'red 'green))
                    
;; 4. Design a function that given an SExp returns the sum of all Numbers in
;;    the SExp.
;;;; Signature
;; sum-of-nums-sexp: SExp -> NonNegInteger
;;;; Purpose
;; GIVEN: an s-expression
;; RETURNS: the sum of all numbers in this s-expression

;;;; Function Definition
(define (sum-of-nums-sexp sexp)
  (cond
    [(is-atom? sexp) (if (number? sexp)
                         sexp
                         0)]
    [(list? sexp) (local (;;;; Signature
                          ;; sum-of-nums-losexp: LOSExp -> NonNegInteger
                          ;;;; Purpose
                          ;; GIVEN: a list of s-expressions
                          ;; RETURNS: the sum of all numbers in this list of s-expressions

                          ;;;; Function Definition
                          (define (sum-of-nums-losexp losexp)
                            (cond
                              [(empty? losexp) 0]
                              [(cons? losexp) (+ (sum-of-nums-sexp (first losexp))
                                                 (sum-of-nums-losexp (rest losexp)))])))
                    (sum-of-nums-losexp sexp))]))


;;;; Tests
(check-expect (sum-of-nums-sexp SEXP-1)
              0)
(check-expect (sum-of-nums-sexp SEXP-2)
              0)
(check-expect (sum-of-nums-sexp SEXP-3)
              0)
(check-expect (sum-of-nums-sexp SEXP-4)
              0)
(check-expect (sum-of-nums-sexp SEXP-5)
              0)
(check-expect (sum-of-nums-sexp SEXP-6)
              0)
(check-expect (sum-of-nums-sexp SEXP-7)
              0)
(check-expect (sum-of-nums-sexp SEXP-10)
              18) 

;; 5. Design a function that given an SExp and two strings  s1 and s2 returns a new
;;    SExp where all instance of  s1 have been replaced with s2.

;;;; Signature
;; replace-string-in-sexp: SExp String String -> SExp
;;;; Purpose
;; GIVEN: an s-expression and two strings
;; RETURNS: the s-expression with all the first given strings in the s-expression
;;          replaced by the second given string

;;;; Function Definition
(define (replace-string-in-sexp sexp str-1 str-2)
  (cond
    [(is-atom? sexp) (local (;;;; Signature
                                 ;; is-str? : Any -> Boolean
                                 ;;;; Purpose
                                 ;; GIVEN: any value
                                 ;; RETURNS: true if the value equals to str-1
                                 ;;          false otherwise
                                 
                                 ;;;; Function Definition
                                 (define (is-str? any)
                                   (and (string? any)
                                        (string=? any str-1))))
                           (if (is-str? sexp)
                               str-2
                               sexp))]
    [(list? sexp) (local (;;;; Signature
                          ;; replace-string-in-losexp: LOSExp String String -> LOSExp
                          ;;;; Purpose
                          ;; GIVEN: a list of s-expressions and two strings
                          ;; RETURNS: the list of s-expressions with all the instances of the first given
                          ;;          strings replaced by the second given string

                          ;;;; Functio Definition
                          (define (replace-string-in-losexp losexp str-1 str-2)
                            (cond
                              [(empty? losexp) empty]
                              [(cons? losexp) (cons (replace-string-in-sexp (first losexp) str-1 str-2)
                                                    (replace-string-in-losexp (rest losexp) str-1 str-2))])))
     (replace-string-in-losexp sexp str-1 str-2))]))



;;;; Tests
(check-expect (replace-string-in-sexp SEXP-1 "aa" "bb")
              SEXP-1)
(check-expect (replace-string-in-sexp SEXP-2 "aa" "bb")
              "bb")
(check-expect (replace-string-in-sexp SEXP-3 "aa" "bb")
              SEXP-3)
(check-expect (replace-string-in-sexp SEXP-4 "aa" "bb")
              SEXP-4)
(check-expect (replace-string-in-sexp SEXP-5 "aa" "bb")
              SEXP-5)
(check-expect (replace-string-in-sexp SEXP-6 "aa" "bb")
              SEXP-6)
(check-expect (replace-string-in-sexp SEXP-7 "aa" "bb")
              (list ATOM-1 "bb" ATOM-3 ATOM-4))
(check-expect (replace-string-in-sexp SEXP-11 "aa" "bb")
              (list (list 0) "bb" "bb" "bb" (list ATOM-1 "bb" ATOM-3 ATOM-4) "bb"))
(check-expect (replace-string-in-sexp SEXP-11 "bb" "aa")
              (list (list 0) "aa" "aa" "aa" (list ATOM-1 "aa" ATOM-3 ATOM-4) "aa"))                          
       

;;;;  Higher Order Functions

;;;; Data Definition

(define-struct grade (letter num))
;; A Grade is: (make-grade LetterGrade Number)
;; INTERP: represents a homework grade as 
;;         a letter grade and number grade

;; Deconstructor Template
;; grade-fn : Grade -> ???
#; (define (grade-fn grade)
     ... (letter-fn (grade-letter grade)) ...
     ... (grade-num grade) ...)

;; A LetterGrade is one of:
;; - 'A  >= 90
;; - 'B  >= 80
;; - 'C  >= 70
;; - 'D  >= 60 
;; - 'F  < 60
;; Deconstructor Template
;; letter-grade-fn: LetterGrade -> ???
#; (define (letter-grade-fn letter-grade)
     (cond
       [(symbol=? letter-grade 'A) ...]
       [(symbol=? letter-grade 'B) ...]
       [(symbol=? letter-grade 'C) ...]
       [(symbol=? letter-grade 'D) ...]
       [(symbol=? letter-grade 'F) ...]))

;; A List<Grades> is one of
;; - empty
;; - (cons Grade List<Grades>)
;; INTERP: represents a list of grades

;; Deconstructor Template
;; log-fn : List<Grades> -> ???
#; (define (log-fn log)
     ... (grade-fn (first log)) ...
     ... (log-fn (rest log)) ...)

(define grades (list (make-grade 'D 62) (make-grade 'C 79) 
                     (make-grade 'A 93) (make-grade 'B 84) 
                     (make-grade 'F 57) (make-grade 'F 38) 
                     (make-grade 'A 90) (make-grade 'A 95)
                     (make-grade 'C 76) (make-grade 'A 90) 
                     (make-grade 'F 55) (make-grade 'C 74)
                     (make-grade 'A 92) (make-grade 'B 86) 
                     (make-grade 'F 43) (make-grade 'C 73)))

(define GRADE-1 (list (make-grade 'A 92)
                      (make-grade 'A 94)
                      (make-grade 'B 86)
                      (make-grade 'C 73)
                      (make-grade 'D 68)))

;; 1 .Design the function log->lon that converts a List<Grade> into
;;    a List<Number> that contains just the numerical grade, using
;;    the Scheme function map.

;;;; Signature
;; log->lon: List<Grades> -> List<Number>
;;;; Purpose
;; GIVEN: a list of grades
;; RETURNS: a list of numbers that contains only the numerical part
;;          of the grades in the list of grades

;;;; Function Definition
(define (log->lon log)
  (map (lambda (grade) (grade-num grade))
       log))

;;;; Tests
(check-expect (log->lon grades)
              (list 62 79 93 84 57 38 90 95 76 90 55 74 92 86 43 73))
(check-expect (log->lon GRADE-1)
              (list 92 94 86 73 68))

;; 2 .Using foldr, design the function best-grade that finds the
;;    highest Grade in a List<Grade>.
;;;; Signature
;; best-grade: List<Grades> -> Grade
;;;; Purpose
;; GIVEN: a list of grades
;; RETURNS: the highest grade in the list of grades

(define BASE (make-grade 'F 0))
;;;; Function Definition
(define (best-grades log)
  (foldr (lambda (x y) (if (> (grade-num x)
                              (grade-num y))
                           x
                           y))
         BASE
         log))

;;;; Tests
(check-expect (best-grades grades)
              (make-grade 'A 95))
(check-expect (best-grades GRADE-1)
              (make-grade 'A 94))

;; 3. Design a function just-As that returns a list of only the
;;    'A grades, using filter.
;;;; Signature
;; just-as : List<Grades> -> List<Grades>
;;;; Purpose
;; GIVEN: a list of grades
;; RETURNS: a list of only the 'A grades in the list of grades

;;;; Function Definition
(define (just-as log)
  (filter (lambda (x) (symbol=? (grade-letter x) 'A))
          log))

;;;; Tests
(check-expect (just-as grades)
              (list (make-grade 'A 93)
                    (make-grade 'A 90)
                    (make-grade 'A 95)
                    (make-grade 'A 90) 
                    (make-grade 'A 92))) 
(check-expect (just-as GRADE-1)
              (list (make-grade 'A 92)
                    (make-grade 'A 94)))
                          
;; 4. Use andmap  to design the function all-pass? that checks to
;;    see if all the Grades in a given list are not 'F.

;;;; Signature
;; all-pass? : List<Grades> -> Boolean
;;;; Purpose
;; GIVEN: a list of grades
;; RETURNS: ture if the all the grades in the given list are not 'F
;;          false otherwise

;;;; Function Definition
(define (all-pass? log)
  (andmap (lambda (x) (not (symbol=? (grade-letter x)
                                     'F)))
          log))
;;;; Tests
(check-expect (all-pass? grades)
              #false)
(check-expect (all-pass? GRADE-1)
              #true)

;; 5. Finally design the function bonus that adds 5 to all of the 
;;    Grades in a given list, and updates the letter portion of the
;;    Grade if it changes. Use map to design your function...
;     it must return a List<Grade>!

;;;; Signature
;; bonus : List<Grades> -> List<Grades>
;;;; Purpose
;; GIVEN: a list of grades
;; RETURNS: the list of grades with all the grade added 5 to its
;;          numerical part, and update its letter portion as well

;;;; Function Definition
(define (bonus log)
  (map (lambda (grade)
         (local ((define num (grade-num grade))
                 (define new-num (+ (grade-num grade) 5))
                 ;;;; Signature
                 ;; letter-update: Grade -> Letter
                 ;;;; Purpose
                 ;; GIVEN: a grade
                 ;; RETURNS: the letter grade of the grade after
                 ;;          it is added by 5
                 
                 ;;;; Function Definition
                 (define (letter-update grade)
                   ;;(local (
                     (cond
                       [(>= new-num 90) 'A]
                       [(>= new-num 80) 'B]
                       [(>= new-num 70) 'C]
                       [(>= new-num 60) 'D]
                       [(< new-num 60) 'F]))

                 ;;;; Signature
                 ;; num-update: Grade -> NonNegInteger
                 ;;;; Purpose
                 ;; GIVEN: a grade
                 ;; RETURNS: the numerical grade of the grade after
                 ;;          it is added by 5

                 ;;;; Function Definition
                 (define (num-update grade)
                   (if (>= num 95)
                       100
                       new-num)))               
           (make-grade (letter-update grade)
                       (num-update grade))))
         log))

;;;; Tests
(check-expect (bonus grades)
              (list (make-grade 'D 67) (make-grade 'B 84) 
                     (make-grade 'A 98) (make-grade 'B 89) 
                     (make-grade 'D 62) (make-grade 'F 43) 
                     (make-grade 'A 95) (make-grade 'A 100)
                     (make-grade 'B 81) (make-grade 'A 95) 
                     (make-grade 'D 60) (make-grade 'C 79)
                     (make-grade 'A 97) (make-grade 'A 91) 
                     (make-grade 'F 48) (make-grade 'C 78)))

;;;; Trees

;; 1. Provide a data definition for a Binary Search Tree that contains non-negative
;;    Integers (BSTI) and answer the following questions

;;;; Data Definition
(define-struct leaf ())
(define-struct node (value left right))
;; A Binary Search Tree of Non Negative Integers (BSTI) is one of
;; - (make-leaf)
;;   INTERP: represents an empty leaf node
;; - (make-node NonNegInteger BSTI BSTI)
;;   WHERE: the value of the nodes in the node's left subtree are all smaller than the value
;;          of the node, and the value of the nodes in the node's right subtree are all
;;          greater than the value of the node
;;   INTERP: represents a node with its value and 2 children
;; INTERP: represents a binary search tree that contains non-negative integers

;; Deconstructor Template
;; bsti-fn : BSTI -> ???
#; (define (bsti-fn bsti)
     (cond
       [(leaf? bsti) ...]
       [(node? bsti) ... (node-value bsti) ...
                     ... (bsti-fn (node-left bsti)) ...
                     ... (bsti-fn (node-value bsti)) ...]))


(define BSTI-MT (make-leaf))
(define BSTI-2 (make-node 2 BSTI-MT BSTI-MT))
(define BSTI-5 (make-node 5 BSTI-2 BSTI-MT))
(define BSTI-15 (make-node 15 BSTI-MT BSTI-MT))
(define BSTI-10 (make-node 10 BSTI-5 BSTI-15))
(define BSTI-28 (make-node 28 BSTI-MT BSTI-MT))
(define BSTI-25 (make-node 25 BSTI-MT BSTI-28))
(define BSTI-35 (make-node 35 BSTI-MT BSTI-MT))
(define BSTI-30 (make-node 30 BSTI-25 BSTI-35))
(define BSTI-20 (make-node 20 BSTI-10 BSTI-30))

;; 1.a Design a function that given a BSTI returns the sum of all the integers found
;; inside the BSTI.
;;;; Signature
;; sum-of-int-bsti: BSTI -> NonNegInteger
;;;; Purpose
;; GIVEN: a binary search tree of non negative integers
;; RETURNS: the sum of all integers in the binary tree

;;;; Function Definition
(define (sum-of-int-bsti bsti)
  (cond
    [(leaf? bsti) 0]
    [(node? bsti) (+ (node-value bsti)
                     (sum-of-int-bsti (node-left bsti))
                     (sum-of-int-bsti (node-right bsti)))]))


;;;; Tests
(check-expect (sum-of-int-bsti BSTI-MT)
              0)
(check-expect (sum-of-int-bsti BSTI-2)
              2)
(check-expect (sum-of-int-bsti BSTI-5)
              7)
(check-expect (sum-of-int-bsti BSTI-10)
              32)
(check-expect (sum-of-int-bsti BSTI-25)
              53)
(check-expect (sum-of-int-bsti BSTI-30)
              118)
(check-expect (sum-of-int-bsti BSTI-20)
              170)
;; 1.b Design a function that given a BSTI returns the product of all the integers found
;; inside the BSTI.
;;;; Signature
;; product-of-int-bsti: BSTI -> NonNegInteger
;;;; Purpose
;; GIVEN: a binary search tree of non negative integers
;; RETURNS: a the product of all integers found inside the binary tree

;;;; Function Definition
(define (product-of-int-bsti bsti)
  (cond
    [(leaf? bsti) 1]
    [(node? bsti) (* (node-value bsti)
                     (product-of-int-bsti (node-left bsti))
                     (product-of-int-bsti (node-right bsti)))]))
;;;; Tests
(check-expect (product-of-int-bsti BSTI-MT)
              1)
(check-expect (product-of-int-bsti BSTI-2)
              2)
(check-expect (product-of-int-bsti BSTI-5)
              10)
(check-expect (product-of-int-bsti BSTI-10)
              1500)
(check-expect (product-of-int-bsti BSTI-30)
              735000)

;; 1.c Design a funciton that given a BSTI and a non negative integer dx multiplies each
;; integer in the BSTI by dx.
;;;; Signature
;; multiply-each-int-bsti: BSTI NonNegInteger -> BSTI
;;;; Purpose
;; GIVEN: a binary search tree of non negative integers and a non negative integer
;; RETURNS: the binary search tree with all the integers in the tree been multiplies to
;;          the given non negative integer

;;;; Function Definition
(define (multiply-each-int-bsti bsti dx)
  (cond
    [(leaf? bsti) BSTI-MT]
    [(node? bsti) (make-node (* (node-value bsti) dx)
                             (multiply-each-int-bsti (node-left bsti) dx)
                             (multiply-each-int-bsti (node-right bsti) dx))]))

;;;; Tests
(check-expect (multiply-each-int-bsti BSTI-MT 2)
              BSTI-MT)
(check-expect (multiply-each-int-bsti BSTI-2 2)
              (make-node 4 BSTI-MT BSTI-MT))
(check-expect (multiply-each-int-bsti BSTI-5 2)
              (make-node 10
                         (make-node 4 BSTI-MT BSTI-MT)
                         BSTI-MT))
(check-expect (multiply-each-int-bsti BSTI-10 2)
              (make-node 20
                         (make-node 10
                                    (make-node 4 BSTI-MT BSTI-MT)
                                    BSTI-MT)
                         (make-node 30 BSTI-MT BSTI-MT)))



;; 2. Provide a data definition for a Binary Search Tree that contains Strings (BSTS).
;;    Your BSTS should use alphabetical ordering to decide which string should be stored
;;    to the left or right subtree. Answer the following questions for a BSTS

;; A Binary Search Tree of Strings (BSTS) is one of
;; - (make-leaf)
;;   INTERP: represents an empty leaf node
;; - (make-node String BSTS BSTS)
;;   WHERE: the value of the nodes in the node's left subtree are of lower alphabetical order
;;          than the value of the node, and the value of the nodes in the node's right subtree
;;          are all higher alphabetical order than the value of the node
;;   INTERP: represents a node with its value and 2 children
;; INTERP: represents a binary search tree that contains strings

;; Deconstructor Template
;; bsts-fn : BSTS -> ???
#; (define (bsts-fn bsts)
     (cond
       [(leaf? bsts) ...]
       [(node? bsts) ... (node-value bsts) ...
                     ... (bsts-fn (node-left bsts)) ...
                     ... (bsts-fn (node-value bsts)) ...]))


(define BSTS-MT (make-leaf))
(define BSTS-A (make-node "apple" BSTS-MT BSTS-MT))
(define BSTS-D (make-node "duck" BSTS-A BSTS-MT))
(define BSTS-J (make-node "jack" BSTS-MT BSTS-MT))
(define BSTS-G (make-node "goose" BSTS-D BSTS-J))
(define BSTS-R (make-node "rooster" BSTS-MT BSTS-MT))
(define BSTS-P (make-node "pine" BSTS-MT BSTS-R))
(define BSTS-W (make-node "wine" BSTS-MT BSTS-MT))
(define BSTS-T (make-node "turkey" BSTS-P BSTS-W))
(define BSTS-M (make-node "milk" BSTS-G BSTS-T))

;; 2.a Design a function that given a BSTS appends all the strings in the BSTS 
;;   using an in-order traversal.

;;;; Signature
;; append-string-bsts: BSTS -> String
;;;; Purpose
;; GIVEN: a binary search tree of strings
;; RETURNS: a string that appends all the strings in the given binary tree

;;;; Function Definition
(define (append-string-bsts bsts)
  (cond
    [(leaf? bsts) ""]
    [(node? bsts) (string-append (append-string-bsts (node-left bsts))
                                 (node-value bsts)                                 
                                 (append-string-bsts (node-right bsts)))]))

;;;; Tests
(check-expect (append-string-bsts BSTS-MT)
              "")
(check-expect (append-string-bsts BSTS-A)
              "apple")
(check-expect (append-string-bsts BSTS-D)
              "appleduck")
(check-expect (append-string-bsts BSTS-G)
              "appleduckgoosejack")
(check-expect (append-string-bsts BSTS-T)
              "pineroosterturkeywine")
(check-expect (append-string-bsts BSTS-M)
              "appleduckgoosejackmilkpineroosterturkeywine")


;; 2.b Design a function that given a BSTS and a String ds appends  ds to each
;;     string in the BSTS. extend-string-bsts: BSTS String -> BSTS
;;;; Purpose
;; GIVEN: a binary search tree of strings and a string
;; RETURNS: a binary search tree with each of the string in the tree appends
;;          the given string
;;;; Function Definition
(define (extend-string-bsts bsts ds)
  (cond
    [(leaf? bsts) BSTS-MT]
    [(node? bsts) (make-node (string-append (node-value bsts) ds)                                 
                             (extend-string-bsts (node-left bsts) ds)
                             (extend-string-bsts (node-right bsts) ds))]))

;;;; Tests
(check-expect (extend-string-bsts BSTS-MT "aa")
              BSTS-MT)
(check-expect (extend-string-bsts BSTS-A "aa")
              (make-node "appleaa" BSTS-MT BSTS-MT))
(check-expect (extend-string-bsts BSTS-D "aa")
              (make-node "duckaa"
                         (make-node "appleaa" BSTS-MT BSTS-MT)
                         BSTS-MT))
(check-expect (extend-string-bsts BSTS-G "aa")
              (make-node "gooseaa"
                         (make-node "duckaa"
                                              (make-node "appleaa"
                                                         BSTS-MT
                                                         BSTS-MT)
                                              BSTS-MT)
                         (make-node "jackaa" BSTS-MT BSTS-MT)))
              
;; 3. Provide a data defintion for a tuple. A tuple always contains 2 values, 
;;    the first value is a string and the second value is a non-negative 
;;    integer. Provide a data definition of a Binary Search Tree that can 
;;    contain Tuples (BSTT). Your BSTT should use the alphabetical ordering on
;;    the String inside the Tuple's in order to decide which tuple should be 
;;    stored to the left or right subtree of a node.

(define-struct tuple (str num))
;;;; Data Definition
;; A Tuple is (make-tuple String NonNegInteger)
;; INTERP: represents a tuple with a string and an non negative integer

;; Deconstructor Template
;; tuple-fn : Tuple -> ???
#; (define (tuple-fn tuple)
     ... (tuple-str tuple) ...
     ... (tuple-num tuple) ...)
(define TUPLE-A (make-tuple "apple" 2))
(define TUPLE-D (make-tuple "duck" 5))
(define TUPLE-J (make-tuple "jack" 15))
(define TUPLE-G (make-tuple "goose" 10))
(define TUPLE-R (make-tuple "rooster" 28))
(define TUPLE-P (make-tuple "pine" 25))
(define TUPLE-W (make-tuple "wine" 35))
(define TUPLE-T (make-tuple "turkey" 30))
(define TUPLE-M (make-tuple "milk" 20))

;; A Binary Search Tree of Tuple (BSTT) is one of
;; - (make-leaf)
;;   INTERP: represents an empty leaf node
;; - (make-node Tuple BSTT BSTT)
;;   WHERE: the strings of the tuples in the node's left subtree are of lower
;;          alphabetical order than the node's, and the strings of the tuples 
;;          in the node's right subtree are all higher alphabetical order
;;           than the node's
;;   INTERP: represents a node with its value and 2 children
;; INTERP: represents a binary search tree of tuples

(define BSTT-MT (make-leaf))
(define BSTT-A (make-node TUPLE-A BSTT-MT BSTT-MT))
(define BSTT-J (make-node TUPLE-J BSTT-MT BSTT-MT))
(define BSTT-D (make-node TUPLE-D BSTT-A BSTT-MT))
(define BSTT-G (make-node TUPLE-G BSTT-D BSTT-J))
(define BSTT-R (make-node TUPLE-R BSTT-MT BSTT-MT))
(define BSTT-P (make-node TUPLE-P BSTT-MT BSTT-R))
(define BSTT-W (make-node TUPLE-W BSTT-MT BSTT-MT))
(define BSTT-T (make-node TUPLE-T BSTT-P BSTT-W))
(define BSTT-M (make-node TUPLE-M BSTT-G BSTT-T))

;; Deconstructor Template
;; bstt-fn: BSTT -> ???
#; (define (bstt-fn bstt)
     (cond
       [(leaf? bstt) ...]
       [(node? bstt) ... (tuple-fn (node-value bstt)) ...
                     ... (bstt-fn (node-left bstt)) ...
                     ... (bstt-fn (node-right bstt)) ...]))

;; 3.a Design a function that given a BSTT returns the sum of all Integer 
;;     values in the Tuples found in the BSTT.

;;;; Signature
;; sum-of-int-bstt: BSTT -> NonNegInteger
;;;; Purpose
;; GIVEN: a binary search tree of tuples
;; RETURNS: the sum of all integers in the tuples found in the binary search 
;;          tree of tuples
;;;; Function Definition
(define (sum-of-int-bstt bstt)
  (cond
    [(leaf? bstt) 0]
    [(node? bstt) (+ (tuple-num (node-value bstt))
                     (sum-of-int-bstt (node-left bstt))
                     (sum-of-int-bstt (node-right bstt)))]))

;;;; Tests
(check-expect (sum-of-int-bstt BSTT-MT)
              0)
(check-expect (sum-of-int-bstt BSTT-A)
              2)
(check-expect (sum-of-int-bstt BSTT-D)
              7)
(check-expect (sum-of-int-bstt BSTT-G)
              32)
(check-expect (sum-of-int-bstt BSTT-T)
              118)
(check-expect (sum-of-int-bstt BSTT-M)
              170)

;; 3.b Design a function that given a BSTT appends all the String values
;;     in the Tuples found int he BSTT using an in-order traversal.


;;;; Signature
;; append-string-bstt: BSTT -> String
;;;; Purpose
;; GIVEN: a binary search tree of tuples
;; RETURNS: a string that appends all the strings in the tuples found in the
;           binary tree
;;;; Function Definition
(define (append-string-bstt bstt)
  (cond
    [(leaf? bstt) ""]
    [(node? bstt) (string-append (append-string-bstt (node-left bstt))
                                 (tuple-str (node-value bstt))
                                 (append-string-bstt (node-right bstt)))]))

;;;; Tests
(check-expect (append-string-bstt BSTT-MT)
              "")
(check-expect (append-string-bstt BSTT-A)
              "apple")
(check-expect (append-string-bstt BSTT-D)
              "appleduck")
(check-expect (append-string-bstt BSTT-G)
              "appleduckgoosejack")
(check-expect (append-string-bstt BSTT-T)
              "pineroosterturkeywine")
(check-expect (append-string-bstt BSTT-M)
              "appleduckgoosejackmilkpineroosterturkeywine")

;; 3.c Design a function that given a BSTT returns a BSTS that holds only the
;;     String values found in each Tuple in the BSTT.


;;;; Signature
;; filter-string-bstt: BSTT -> BSTS
;;;; Purpose
;; GIVEN: a binary search tree of tuples
;; RETURNS: a binary search tree of strings that with all the strings in the 
;;          tuples found in the binary tree

;;;; Function Definition
(define (filter-string-bstt bstt)
  (cond
    [(leaf? bstt) BSTS-MT]
    [(node? bstt) (make-node (tuple-str (node-value bstt))
                             (filter-string-bstt (node-left bstt))
                             (filter-string-bstt (node-right bstt)))]))

;;;; Tests
(check-expect (filter-string-bstt BSTT-MT)
              BSTS-MT)
(check-expect (filter-string-bstt BSTT-A)
              BSTS-A)
(check-expect (filter-string-bstt BSTT-D)
              BSTS-D)
(check-expect (filter-string-bstt BSTT-G)
              BSTS-G)
(check-expect (filter-string-bstt BSTT-T)
              BSTS-T)
(check-expect (filter-string-bstt BSTT-M)
              BSTS-M)

;; 1. provide a generic data definition for a BST (GBST) that can take any kind
;;    of data as an element.

;;;; Data Definition

;; A GBST<X> is one of
;; - (make-leaf)
;;   INTERP: represents an empty leaf node
;; - (make-node X GBST<X> GBST<X>)
;;   INTERP: represents a node with a value and two children
;; INTERP: represents a binary search tree of any kind of elements

;; 2. The solutions to 1.c., 2.b., and 3.c. are similar. Use your existing 
;;    solutions to design a generic function on GBST. Use your generic function
;;    to provide a second solution to 1.c., 2.b., 3.c.
(define GBST-MT (make-leaf))
;;;; Function Template
#; (define (mymap-1 gbst ...)
     (cond
       [(leaf? gbst) GBST-MT]
       [(node? gbst) (make-node (... (node-value gbst) ...)                                 
                                (mymap-1 (node-left gbst) ...)
                                (mymap-1 (node-right gbst) ...))]))
;;;; Signature:
;; mymap-1: [X,Y] : GBST<X> [X -> Y] -> GBST<Y>
(define (mymap-1 gbst op)
  (cond
       [(leaf? gbst) GBST-MT]
       [(node? gbst) (make-node (op (node-value gbst))                                 
                                (mymap-1 (node-left gbst) op)
                                (mymap-1 (node-right gbst) op))]))
;;;; Function Definition
(define (multiply-each-int-bsti.v2 bsti dx)
  (mymap-1 bsti
           (lambda (x) (* x
                          dx))))

;;;; Tests
(check-expect (multiply-each-int-bsti.v2 BSTI-MT 2)
              BSTI-MT)
(check-expect (multiply-each-int-bsti.v2 BSTI-2 2)
              (make-node 4 BSTI-MT BSTI-MT))
(check-expect (multiply-each-int-bsti.v2 BSTI-5 2)
              (make-node 10
                         (make-node 4 BSTI-MT BSTI-MT)
                         BSTI-MT))
(check-expect (multiply-each-int-bsti.v2 BSTI-10 2)
              (make-node 20
                         (make-node 10
                                    (make-node 4 BSTI-MT BSTI-MT)
                                    BSTI-MT)
                         (make-node 30 BSTI-MT BSTI-MT)))


;;;; Function Definition
(define (extend-string-bsts.v2 bsts ds)
  (mymap-1 bsts
           (lambda (x) (string-append x
                                      ds))))

;;;; Tests
(check-expect (extend-string-bsts.v2 BSTS-MT "aa")
              BSTS-MT)
(check-expect (extend-string-bsts.v2 BSTS-A "aa")
              (make-node "appleaa" BSTS-MT BSTS-MT))
(check-expect (extend-string-bsts.v2 BSTS-D "aa")
              (make-node "duckaa"
                         (make-node "appleaa" BSTS-MT BSTS-MT)
                         BSTS-MT))
(check-expect (extend-string-bsts.v2 BSTS-G "aa")
              (make-node "gooseaa"
                         (make-node "duckaa"
                                              (make-node "appleaa"
                                                         BSTS-MT
                                                         BSTS-MT)
                                              BSTS-MT)
                         (make-node "jackaa" BSTS-MT BSTS-MT)))

;;;; Function Definition  
(define (filter-string-bstt.v2 bstt)
  (mymap-1 bstt
           (lambda (x) (tuple-str x))))

;;;; Tests
(check-expect (filter-string-bstt.v2 BSTT-MT)
              BSTS-MT)
(check-expect (filter-string-bstt.v2 BSTT-A)
              BSTS-A)
(check-expect (filter-string-bstt.v2 BSTT-D)
              BSTS-D)
(check-expect (filter-string-bstt.v2 BSTT-G)
              BSTS-G)
(check-expect (filter-string-bstt.v2 BSTT-T)
              BSTS-T)
(check-expect (filter-string-bstt.v2 BSTT-M)
              BSTS-M)

;; 3. The solutions to 1.a., 1.b., 2.a., 3.a. and 3.b are similar. Use your
;;    existing solutions to design a generic function on GBST. Use your 
;;    generic function to provide a second solution to 1.a., 1.b., 2.a., 3.a.
;;    and 3.b.
;;;; Function Template
#; (define (mymap-2 gbst ...)
     (cond
       [(leaf? gbst) ...]
       [(node? gbst) (... (node-value bsti) ...
                          (mymap-2 (node-left gbst) ...)
                          (mymap-2 (node-right gbst) ...))]))
;;;; Signature:
;; mymap-1: [X,Y] : GBST<X> [X -> Y] -> GBST<Y>
(define (mymap-2 gbst base op)
  (cond
       [(leaf? gbst) base]
       [(node? gbst) (op (node-value gbst)
                         (mymap-2 (node-left gbst) base op)
                         (mymap-2 (node-right gbst) base op))]))



;;;; Function Definition

(define (sum-of-int-bsti.v2 bsti)
  (mymap-2 bsti
           0
           (lambda (x y z) (+ x y z))))


;;;; Tests
(check-expect (sum-of-int-bsti.v2 BSTI-MT)
              0)
(check-expect (sum-of-int-bsti.v2 BSTI-2)
              2)
(check-expect (sum-of-int-bsti.v2 BSTI-5)
              7)
(check-expect (sum-of-int-bsti.v2 BSTI-10)
              32)
(check-expect (sum-of-int-bsti.v2 BSTI-25)
              53)
(check-expect (sum-of-int-bsti.v2 BSTI-30)
              118)
(check-expect (sum-of-int-bsti.v2 BSTI-20)
              170)

;;;; Function Definition
(define (product-of-int-bsti.v2 bsti)
  (mymap-2 bsti
           1
           (lambda (x y z) (* x y z))))

;;;; Tests
(check-expect (product-of-int-bsti.v2 BSTI-MT)
              1)
(check-expect (product-of-int-bsti.v2 BSTI-2)
              2)
(check-expect (product-of-int-bsti.v2 BSTI-5)
              10)
(check-expect (product-of-int-bsti.v2 BSTI-10)
              1500)
(check-expect (product-of-int-bsti.v2 BSTI-30)
              735000)


;;;; Function Definition
(define (append-string-bsts.v2 bsts)
  (mymap-2 bsts
           ""
           (lambda (x y z) (string-append y x z))))

;;;; Tests
(check-expect (append-string-bsts.v2 BSTS-MT)
              "")
(check-expect (append-string-bsts.v2 BSTS-A)
              "apple")
(check-expect (append-string-bsts.v2 BSTS-D)
              "appleduck")
(check-expect (append-string-bsts.v2 BSTS-G)
              "appleduckgoosejack")
(check-expect (append-string-bsts.v2 BSTS-T)
              "pineroosterturkeywine")
(check-expect (append-string-bsts.v2 BSTS-M)
              "appleduckgoosejackmilkpineroosterturkeywine")


;;;; Function Definition
(define (sum-of-int-bstt.v2 bstt)
  (mymap-2 bstt
           0
           (lambda (x y z) (+ (tuple-num x)
                              y
                              z))))

;;;; Tests
(check-expect (sum-of-int-bstt.v2 BSTT-MT)
              0)
(check-expect (sum-of-int-bstt.v2 BSTT-A)
              2)
(check-expect (sum-of-int-bstt.v2 BSTT-D)
              7)
(check-expect (sum-of-int-bstt.v2 BSTT-G)
              32)
(check-expect (sum-of-int-bstt.v2 BSTT-T)
              118)
(check-expect (sum-of-int-bstt.v2 BSTT-M)
              170)


;;;; Function Definition
(define (append-string-bstt.v2 bstt)
  (mymap-2 bstt
           ""
           (lambda (x y z) (string-append y
                                          (tuple-str x)
                                          z))))

;;;; Tests
(check-expect (append-string-bstt.v2 BSTT-MT)
              "")
(check-expect (append-string-bstt.v2 BSTT-A)
              "apple")
(check-expect (append-string-bstt.v2 BSTT-D)
              "appleduck")
(check-expect (append-string-bstt.v2 BSTT-G)
              "appleduckgoosejack")
(check-expect (append-string-bstt.v2 BSTT-T)
              "pineroosterturkeywine")
(check-expect (append-string-bstt.v2 BSTT-M)
              "appleduckgoosejackmilkpineroosterturkeywine")