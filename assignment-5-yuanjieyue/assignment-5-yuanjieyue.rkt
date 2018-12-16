;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment-5-yuanjieyue) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;> TOTAL: 149/161
;;> -0.5 style (see comment below)

;;;; Problem 1

;;;; Data Definition
;; A List of Symbols (LoS) is one of
;; - empty
;; - (cons Symbol LoS)
;; INTERP: represent s a list of symbols

;; Examples
(define MTLOS empty)
(define LOS-1 (list 'red))
(define LOS-2 (list 'red 'green))
(define LOS-3 (list 'a 'b 'green 'blue 'red))

;; Deconstructor Template
;; los-fn : LoS -> ???
#; (define (los-fn los)
     (cond
       [(empty? los) ...]
       [(cons? los) ... (first los) ...
                    ... (los-fn (rest los) ...)]))


;; A List of List of Symbols (LLoS) is one of
;; - empty
;; - (cons LoS LLoS)
;; INTERP: represents a list of list of symbols

;; Exmaples
(define MTLLOS empty)
(define LLOS-1 (list LOS-1))
(define LLOS-2 (list LOS-1 MTLOS))
(define LLOS-3 (list LOS-1 LOS-2))
(define LLOS-4 (list LOS-1 LOS-2 LOS-3))
(define LLOS-5 (list LOS-1 MTLOS LOS-3 LOS-2))

;; Deconstructor Template
;; llos-fn : LLOS -> ???
#; (define (llos-fn llos)
     (cond
       [(empty? llos) ...]
       [(cons? llos) ... (los-fn (first llos))...
                     ... (llos-fn (rest llos))...]))


;;;; Signature
;; los-size : LoS -> NonNegInteger

;;;; Purpose
;; GIVEN: a list of symbols
;; RETURN: the total number of symbols that are in the list of symbols

;;;; Examples
;; (los-size MTLOS) => 0
;; (los-size LOS-1) => 1
;; (los-size LOS-2) => 2
;; (los-size LOS-3) => 5

;;;; Function Definition
(define (los-size los)
  (cond
    [(empty? los) 0]
    [(cons? los) (+  1
                     (los-size (rest los)))]))

;;;; Tests
(check-expect (los-size MTLOS)
              0)
(check-expect (los-size LOS-1)
              1)
(check-expect (los-size LOS-2)
              2)
(check-expect (los-size LOS-3)
              5)

;;;; Signature
;; llos-size : LLoS -> NonNegInteger

;;;; Purpose
;; GIVEN: a list of list of symbols
;; RETURN: the total number of symbols that are stored
;;         in the list of list of symbols
         
;;;; Exmaples
;; (llos-size MTLLOS) => 0
;; (llos-size LLOS-1) => 1
;; (llos-size LLOS-2) => 1
;; (llos-size LLOS-3) => 3
;; (llos-size LLOS-4) => 8
;; (llos-size LLOS-5) => 8

;;;; Function Definition
(define (llos-size llos)
  (cond
    [(empty? llos) 0]
    [(cons? llos) (+ (los-size (first llos))
                     (llos-size (rest llos)))]))

;;;; Tests
(check-expect (llos-size MTLLOS)
              0)
(check-expect (llos-size LLOS-1)
              1)
(check-expect (llos-size LLOS-2)
              1)
(check-expect (llos-size LLOS-3)
              3)
(check-expect (llos-size LLOS-4)
              8)
(check-expect (llos-size LLOS-5)
              8)


;;;; Signature
;; time-appears-in-los : LoS Symbol -> NonNegInteger
;;> a better fn name would be "num-occurrences" or "num-appearances"

;;;; Purpose
;; GIVEN: a list of symbols and a symbol
;; RETURN: the times that the symbol is found in the list of symbols

;;;; Examples
;; (time-appears-in-los MTLOS 'red) => 0
;; (time-appears-in-los LOS-1 'red) => 1
;; (time-appears-in-los LOS-2 'red) => 1
;; (time-appears-in-los LOS-3 'red) => 1
;; (time-appears-in-los LOS-3 'yellow) => 0

;;;; Function Definition
(define (time-appears-in-los los sym)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (symbol=? (first los) sym)
                     (+ 1
                        (time-appears-in-los (rest los) sym))
                     (time-appears-in-los (rest los) sym))]))

;;;; Tests
(check-expect (time-appears-in-los MTLOS 'red)
              0)
(check-expect (time-appears-in-los LOS-1 'red)
              1)
(check-expect (time-appears-in-los LOS-2 'red)
              1)
(check-expect (time-appears-in-los LOS-3 'red)
              1)
(check-expect (time-appears-in-los LOS-3 'yellow)
              0)


;;;; Signature
;; time-appears : LLoS Symbol -> NonNegInteger
;;> style: bad fn name. time-appears says nothing about your input or what your
;;> fn is doing. A fn name should be descriptive and meaningful

;;;; Purpose
;; GIVEN: a list of list of symbols and a symbol
;; RETURN: the times of the symbol that is found in the list
;;         of list of symbols

;;;; Examples
;; (time-appears MTLLOS 'red) => 0
;; (time-appears LLOS-1 'red) => 1
;; (time-appears LLOS-1 'green) => 0
;; (time-appears LLOS-2 'red) => 1
;; (time-appears LLOS-3 'red) => 2
;; (time-appears LLOS-4 'red) => 3

;;;; Function Definition
(define (time-appears llos sym)
  (cond
    [(empty? llos) 0]
    [(cons? llos) (+ (time-appears-in-los (first llos) sym)
                     (time-appears (rest llos) sym))]))

;;;; Tests
(check-expect (time-appears MTLLOS 'red)
              0)
(check-expect (time-appears LLOS-1 'red)
              1)
(check-expect (time-appears LLOS-1 'green)
              0)
(check-expect (time-appears LLOS-2 'red)
              1)
(check-expect (time-appears LLOS-3 'red)
              2)
(check-expect (time-appears LLOS-4 'red)
              3)


;;;; Signature
;; los-symbol-replace : LoS Symbol Symbol -> LoS

;;;; Purpose
;; GIVEN: a list of symbols and two symbols
;; RETURN: the list of symbols in which the two symbols replace one another

;;;; Examples
;; (los-symbol-replace MTLOS 'red 'green) => MTLOS
;; (los-symbol-replace LOS-1 'red 'green) => (list 'green)
;; (los-symbol-replace LOS-2 'red 'green) => (list 'green 'red)
;; (los-symbol-replace LOS-3 'red 'green)
;;  => (list 'a 'b 'red 'blue 'green)

;;;; Function Definition
(define (los-symbol-replace los sym-1 sym-2)
  (cond
    [(empty? los) empty]
    [(cons? los) (cons (if (symbol=? (first los) sym-1)
                           sym-2
                           (if (symbol=? (first los) sym-2)
                               sym-1
                               (first los)))
                       (los-symbol-replace (rest los) sym-1 sym-2))]))

;;> -1 Should have used a helper function here to replace the symbol if it
;;> needed to be replaced. It would have made this fn would cleaner and easier
;> to read. It also would be a better separation of tasks (1 task 1 function!).

;;;; Tests
(check-expect (los-symbol-replace MTLOS 'red 'green)
              MTLOS)
(check-expect (los-symbol-replace LOS-1 'red 'green)
              (list 'green))
(check-expect (los-symbol-replace LOS-2 'red 'green)
              (list 'green 'red))
(check-expect (los-symbol-replace LOS-3 'red 'green)
              (list 'a 'b 'red 'blue 'green))


;;;; Signature
;; symbol-replace : LLoS Symbol Symbol -> LLoS
;;> a more specific fn name would be llos-symbol-replace

;;;; Purpose
;; GIVEN: a list of list of symbols and two symbols
;; RETURN: the list of list of symbols in which
;;         the two symbols replace one another

;;;; Examples
;; (symbol-replace MTLLOS 'red 'green) => MTLOS
;; (symbol-replace LLOS-1 'red 'red) => LOS-1
;; (symbol-replace LLOS-2 'red 'green) => (list (list 'green) MTLLOS)
;; (symbol-replace LLOS-3 'red 'green)
;;  => (list (list 'green) (list 'green 'red))
;; (symbol-replace LLOS-4 'red 'green)
;;  => (list (list 'green) (list 'green 'red) (list 'a 'b 'red 'blue 'green))
;; (symbol-replace LLOS-5 'red 'green)
;;  => (list (list 'green) MTLLOS (list 'a 'b 'red 'blue 'green)
;;           (list 'green 'red))

;;;; Function Definition
(define (symbol-replace llos sym-1 sym-2)
  (cond
    [(empty? llos) empty]
    [(cons? llos) (cons (los-symbol-replace (first llos) sym-1 sym-2)
                        (symbol-replace (rest llos) sym-1 sym-2))]))


;;;; Tests
(check-expect (symbol-replace MTLLOS 'red 'green)
              MTLLOS)
(check-expect (symbol-replace LLOS-1 'red 'red)
              LLOS-1)
(check-expect (symbol-replace LLOS-2 'red 'green)
              (list (list 'green) MTLLOS))
(check-expect (symbol-replace LLOS-3 'red 'green)
              (list (list 'green) (list 'green 'red)))
(check-expect (symbol-replace LLOS-4 'red 'green)
              (list (list 'green) (list 'green 'red)
                    (list 'a 'b 'red 'blue 'green)))
(check-expect (symbol-replace LLOS-5 'red 'green)
              (list (list 'green) MTLLOS (list 'a 'b 'red 'blue 'green)
                    (list 'green 'red)))




;;;; Problem 2

;;;; Data Definition

(define-struct leaf ())    
(define-struct node (value left right))

;; An IntegerBinarySearchTree (IBST) is one of
;; - (make-leaf)
;;   INTERP: represents an empty leaf node

;; - (make-node Integer IBST IBST)
;;   WHERE: value is less than all values in the left subtree and
;;          value is greater than all values in the right subtree
;;   INTERP: represents a node with a value and 2 children
;; INTERP: represents an integer binary search tree

;; Examples
(define LEAF (make-leaf))
(define IBST-0 (make-node 0 LEAF LEAF))
(define IBST-2 (make-node 2 LEAF LEAF))
(define IBST-1 (make-node 1 IBST-0 IBST-2))
(define IBST-4 (make-node 4 LEAF LEAF))
(define IBST-6 (make-node 6 LEAF LEAF))
(define IBST-5 (make-node 5 IBST-4 IBST-6))
(define IBST-3 (make-node 3 IBST-1 IBST-5))
(define IBST-10 (make-node 10 LEAF LEAF))
(define IBST-12 (make-node 12 LEAF LEAF))
(define IBST-14 (make-node 14 LEAF LEAF))
(define IBST-16 (make-node 16 LEAF LEAF))
(define IBST-11 (make-node 11 IBST-10 IBST-12))
(define IBST-15 (make-node 15 IBST-14 IBST-16))
(define IBST-13 (make-node 13 IBST-11 IBST-15))
(define IBST-9 (make-node 9 IBST-3 IBST-13))

;; Deconstructor Template
;; ibst-fn : IBST -> ???
#; (define (ibst-fn ibst)
     (cond
       [(leaf? ibst) ...]
       [(node? ibst) ...(node-value ibst)...
                     ...(ibst-fn (node-left ibst))...
                     ...(ibst-fn (node-right ibst))...]))

;;;; Signature
;; ibst-size : IBST -> PosInt

;;;; Purpose
;; GIVEN: an integer binary search tree
;; RETURN: the total number of nodes that are in the tree
;;> to be specific, you could have mentioned that you also count the leaves.

;; Examples
;; (ibst-size LEAF) => 1
;; (ibst-size IBST-0) => 3
;; (ibst-size IBST-1) => 7
;; (ibst-size IBST-2) => 3
;; (ibst-size IBST-3) => 15  
;; (ibst-size IBST-4) => 3    
;; (ibst-size IBST-5) => 7    
;; (ibst-size IBST-6) => 3     
;; (ibst-size IBST-9) => 31    
;; (ibst-size IBST-10) => 3
;; (ibst-size IBST-11) => 7
;; (ibst-size IBST-12) => 3
;; (ibst-size IBST-13) => 15
;; (ibst-size IBST-14) => 3
;; (ibst-size IBST-15) => 7
;; (ibst-size IBST-16) => 3
    
;;;; Function Definition
(define (ibst-size ibst)
  (cond
    [(leaf? ibst) 1]
    [(node? ibst) (+ 1
                     (ibst-size (node-left ibst))
                     (ibst-size (node-right ibst)))]))

;; Tests
(check-expect (ibst-size LEAF)
              1)
(check-expect (ibst-size IBST-0)
              3)
(check-expect (ibst-size IBST-1)
              7)
(check-expect (ibst-size IBST-2)
              3)
(check-expect (ibst-size IBST-3)
              15)
(check-expect (ibst-size IBST-4)
              3)
(check-expect (ibst-size IBST-5)
              7)
(check-expect (ibst-size IBST-6)
              3)
(check-expect (ibst-size IBST-9)
              31)
(check-expect (ibst-size IBST-10)
              3)
(check-expect (ibst-size IBST-11)
              7)
(check-expect (ibst-size IBST-12)
              3)
(check-expect (ibst-size IBST-13)
              15)
(check-expect (ibst-size IBST-14)
              3)
(check-expect (ibst-size IBST-15)
              7)
(check-expect (ibst-size IBST-16)
              3)
    
;;;; Signature
;; ibst-contains? : IBST Integer -> Boolean

;;;; Purpose
;; GIVEN: an integer binary search tree and an integer
;; RETURN: true if the integer binary search tree contains the integer
;;         false otherwise

;;;; Examples
;; (ibst-contains? LEAF 0) => #false
;; (ibst-contains? LEAF 1) => #false
;; (ibst-contains? IBST-1 2) => #true
;; (ibst-contains? IBST-1 3) => #false 

;;;; Function Definition
(define (ibst-contains? ibst num)
  (cond
    [(leaf? ibst) #false]
    [(node? ibst) (or (= (node-value ibst) num)
                      (ibst-contains? (node-left ibst) num)
                      (ibst-contains? (node-right ibst) num))]))
;;> You could have used the properties of IBST to recurse on either the left or
;;> right subtree only based on the given num.

;;;; Tests
(check-expect (ibst-contains? LEAF 0)
              #false)
(check-expect (ibst-contains? LEAF 1)
              #false)
(check-expect (ibst-contains? IBST-1 2)
              #true)
(check-expect (ibst-contains? IBST-1 3)
              #false)


;;;; Signature
;; ibst-add : IBST Integer -> IBST

;;;; Purpose
;; GIVEN: an integer binary search tree and a number
;; RETURN: the integer binary search tree with the number added in
;;         if the number is an integer

(define IBST-20 (make-node 20 LEAF LEAF))
(define IBST-32 (make-node 32 LEAF LEAF))
(define IBST-24 (make-node 24 IBST-20 LEAF))
(define IBST-38 (make-node 38 LEAF LEAF))
(define IBST-35 (make-node 35 IBST-32 IBST-38))
(define IBST-30 (make-node 30 IBST-24 IBST-35))
(define IBST-60 (make-node 60 LEAF LEAF))
(define IBST-55 (make-node 55 LEAF IBST-60))
(define IBST-45 (make-node 45 LEAF LEAF))
(define IBST-50 (make-node 50 IBST-45 IBST-55))
(define IBST-40 (make-node 40 IBST-30 IBST-50))

;;;; Examples
;; (ibst-add LEAF 12.5) => "Num should be Integer!"
;; (ibst-add LEAF 18) => (make-node 18 LEAF LEAF)
;; (ibst-add IBST-20 18) => (make-node 20 (make-node 18 LEAF LEAF) LEAF)
;; (ibst-add IBST-20 22) => (make-node 20 LEAF (make-node 22 LEAF LEAF))
;; (ibst-add IBST-24 22) => (make-node 24 (make-node 22 IBST-20 LEAF) LEAF)
;; (ibst-add IBST-24 26) => (make-node 24 IBST-20 (make-node 26 LEAF LEAF))
;; (ibst-add IBST-35 39) => (make-node 35 IBST-32
;;                                     (make-node 38 LEAF
;;                                                (make-node 39 LEAF LEAF)))
;; (ibst-add IBST-30 31)
;;  =>(make-node 30
;;               IBST-24
;;               (make-node 35
;;                          (make-node 32
;;                                     (make-node 31 LEAF LEAF)
;;                                     LEAF)
;;                          IBST-38))


;;;; Function Definition
(define (ibst-add ibst num)
  (if (integer? num)
      (cond
        [(leaf? ibst) (make-node num LEAF LEAF)]
        [(node? ibst) (if (> (node-value ibst) num)
                          (make-node (node-value ibst)
                                     (ibst-add (node-left ibst) num)
                                     (node-right ibst))
                          (if (< (node-value ibst) num)
                              (make-node (node-value ibst)
                                         (node-left ibst)
                                         (ibst-add (node-right ibst) num))
                              ibst))])
      (error "Num should be Integer!")))

;;> -1 Violation of signature. Your signature says you will return an IBST.
;;> Returning an error is not an IBST. I'm guessing you checked for integer?
;;> because you wanted to ensure that you would return an IBST? You could have
;;> done it 2 ways:
;;> 1. Given Any as signature input value and when you check for integer?,
;;>    if the value is not an integer, simply return the IBST unchanged.
;;> 2. You gave Integer in your signature as the data type you REQUIRE for your
;;>    first input. You can depend on this. If someone else puts in a non-int
;;>    value, it is their fault if the fn breaks because they broke the
;;>    contract.

;;;; Tests
(check-error (ibst-add LEAF 12.5)
              "Num should be Integer!")
(check-expect (ibst-add LEAF 18)
              (make-node 18 LEAF LEAF))
(check-expect (ibst-add IBST-20 18)
              (make-node 20 (make-node 18 LEAF LEAF) LEAF))
(check-expect (ibst-add IBST-20 22)
              (make-node 20 LEAF (make-node 22 LEAF LEAF)))
(check-expect (ibst-add IBST-20 20)
              IBST-20)
(check-expect (ibst-add IBST-24 22)
              (make-node 24
                         (make-node 20 LEAF (make-node 22 LEAF LEAF))
                         LEAF))
(check-expect (ibst-add IBST-24 26)
              (make-node 24 IBST-20 (make-node 26 LEAF LEAF)))
(check-expect (ibst-add IBST-35 39)
              (make-node 35 IBST-32
                         (make-node 38 LEAF
                                    (make-node 39 LEAF LEAF))))
(check-expect (ibst-add IBST-30 31)
              (make-node 30
                         IBST-24
                         (make-node 35
                                    (make-node 32
                                               (make-node 31 LEAF LEAF)
                                               LEAF)
                                    IBST-38)))

;;;; A ListofIntegers (LoI) is one of
;; - empty
;; - (cons Integer LoI)
;; INTERP: represents the a list of integers

;; Deconstructor Template
;; loi-fn : LoI -> ???
#; (define (loi-fn loi)
     (cond
       [(empty? loi) ...]
       [(cons? loi) ...(first loi)...
                    ...(loi-fn (rest loi))...]))

;;;; Signature
;; childless : IBST -> Boolean

;;;; Purpose
;; GIVEN: an integer binary search tree
;; WHERE: IBST is not (make-leaf)
;; RETURN: true if the tree has no siblings
;;         false otherwise
;;> WHERE clauses belong with data definitions. You could have used 'Node'
;;> instead (after defining it of course). 

;;;; Examples
;; (childless IBST-0) => #true
;; (childless IBST-24) => #false

;;;; Function Definition
(define (childless? ibst)
  (and (leaf? (node-left ibst))
       (leaf? (node-right ibst))))

;;;; Tests
(check-expect (childless? IBST-0)
              #true)
(check-expect (childless? IBST-24)
              #false)

;;;; Signature
;; ibst-inorder : IBST -> LoI

;;;; Purpose
;; GIVEN: an integer binary search tree
;; RETURN: a list of Integers that are stored in the binary tree
;;> -1 Your purpose statement says nothing about an inorder traversal
;;> How can your purposes be the same for all 3 different traversals?

;;;; Examples
;; (ibst-inorder LEAF) => empty
;; (ibst-inorder IBST-0) => (list 0)
;; (ibst-inorder IBST-1) => (list 0 1 2)
;; (ibst-inorder IBST-3) => (list 0 1 2 3 4 5 6)
;; (ibst-inorder IBST-13) => (list 10 11 12 13 14 15 16)
;; (ibst-inorder IBST-30) => (list 20 24 30 32 35 38)

;;;; Function Definition
(define (ibst-inorder ibst)
  (cond
    [(leaf? ibst) empty]
    [(node? ibst) (if (childless? ibst)
                       (cons (node-value ibst) empty)
                       (append (ibst-inorder (node-left ibst))
                               (cons (node-value ibst) 
                                     (ibst-inorder (node-right ibst)))))]))
;;> The if statement is unnecessary code. If you wrote your recursion correctly,
;;> then leaves will return emptys and append will know how to append empty
;;> lists. You can remove the if statement entirely and maintain same behavior.
;;> Try it. Trust the recursion.

;;;; Tests
(check-expect (ibst-inorder LEAF)
              empty)
(check-expect (ibst-inorder IBST-0)
              (list 0))
(check-expect (ibst-inorder IBST-1)
              (list 0 1 2))
(check-expect (ibst-inorder IBST-3)
              (list 0 1 2 3 4 5 6))
(check-expect (ibst-inorder IBST-13)
              (list 10 11 12 13 14 15 16))
(check-expect (ibst-inorder IBST-30)
              (list 20 24 30 32 35 38))


;;;; Signature
;; ibst-postorder : IBST -> LoI

;;;; Purpose
;; GIVEN: an integer binary search tree
;; RETURN: a list of integers that are stored in teh binary tree
;;> spelling: the*

;;;; Examples
;; (ibst-postorder LEAF) => empty
;; (ibst-postorder IBST-0) => (list 0)
;; (ibst-postorder IBST-1) => (list 0 2 1)
;; (ibst-postorder IBST-3) => (list 0 2 1 4 6 5 3)
;; (ibst-postorder IBST-13) => (list 10 12 11 14 16 15 13)
;; (ibst-postorder IBST-30) => (list 20 24 32 38 35 30)

;;;; Function Definition
(define (ibst-postorder ibst)
  (cond
    [(leaf? ibst) empty]
    [(node? ibst) (if (childless? ibst)
                      (cons (node-value ibst) empty)
                      (append (ibst-postorder (node-left ibst))
                              (append (ibst-postorder (node-right ibst))
                                      (cons (node-value ibst) empty))))]))

;;;; Test
(check-expect (ibst-postorder LEAF)
              empty)
(check-expect (ibst-postorder IBST-0)
              (list 0))
(check-expect (ibst-postorder IBST-1)
              (list 0 2 1))
(check-expect (ibst-postorder IBST-3)
              (list 0 2 1 4 6 5 3))
(check-expect (ibst-postorder IBST-13)
              (list 10 12 11 14 16 15 13))
(check-expect (ibst-postorder IBST-30)
              (list 20 24 32 38 35 30))


;;;; Signature
;; ibst-preorder : IBST -> LoI

;;;; Purpose
;; GIVEN: an integer binary search tree
;; RETURN: a list of Integers that are stored in the binary tree

;;;; Examples
;; (ibst-preorder LEAF) => empty
;; (ibst-preorder IBST-0) => (list 0)
;; (ibst-preorder IBST-1) => (list 1 0 2)
;; (ibst-preorder IBST-3) => (list 3 1 0 2 5 4 6)
;; (ibst-preorder IBST-13) => (list 13 11 10 12 15 14 16)
;; (ibst-preorder IBST-30) => (list 30 24 20 35 32 38)

;;;; Function Definition
(define (ibst-preorder ibst)
  (cond
    [(leaf? ibst) empty]
    [(node? ibst) (if (childless?  ibst)
                      (cons (node-value ibst) empty)
                      (append (cons (node-value ibst) empty)
                              (append (ibst-preorder (node-left ibst))
                                      (ibst-preorder (node-right ibst)))))]))
     
;;;; Tests
(check-expect (ibst-preorder LEAF)
              empty)
(check-expect (ibst-preorder IBST-0)
              (list 0))
(check-expect (ibst-preorder IBST-1)
              (list 1 0 2))
(check-expect (ibst-preorder IBST-3)
              (list 3 1 0 2 5 4 6))
(check-expect (ibst-preorder IBST-13)
              (list 13 11 10 12 15 14 16))
(check-expect (ibst-preorder IBST-30)
              (list 30 24 20 35 32 38))


;;;; Signature
;; is-ibst : Any -> Boolean

;;;; Purpose
;; GIVEN: any racket value
;; RETURN: true if and only if the input is a valid IBST
;;         false otherwise

;;;; Examples
;; (is-ibst 2) => #false
;; (is-ibst "hello") => #false
;; (is-ibst #true) => #false
;; (is-ibst 'red) => #false
;; (is-ibst (make-posn 2 3)) => #false
;; (is-ibst LEAF) => #true
;; (is-ibst IBST-1) => #true
;; (is-ibst IBST-3) => #true

;;;; Function Definition
(define (is-ibst any)
  (cond
    [(leaf? any) #true]
    [(node? any) #true]
    [else #false]))

;;> -7 You did not implement this correctly. Even if a value is a node, you need
;;> to check further if it is a VALID IBST. This means comparing the values of
;;> the root node to the left and right and recursively checking that the left
;;> and right subtrees and valid IBSTs too. In addition, I could have passed you
;;> a StringBinaryTree (SBT) and you would have returned true if SBT used the
;;> node struct as part of its tree.
    
;;;;Tests
(check-expect (is-ibst 2)
              #false)
(check-expect (is-ibst "hello")
              #false)
(check-expect (is-ibst #true)
              #false)
(check-expect (is-ibst 'red)
              #false)
(check-expect (is-ibst (make-posn 2 3))
              #false)
(check-expect (is-ibst LEAF)
              #true)
(check-expect (is-ibst IBST-1)
              #true)
(check-expect (is-ibst IBST-3)
              #true)


;;;; Signature
;; ibst-remove : IBST Integer-> IBST

;;;; Purpose
;; GIVEN: an integer binary search tree and an integer
;; Purpose: the integer binary search tree with integer removed

;;;; Examples
;; (ibst-remove LEAF 0) => LEAF
;; (ibst-remove IBST-0 0) => LEAF
;; (ibst-remove IBST-1 0) => (make-node 1 LEAF (make-node 2 LEAF LEAF))
;; (ibst-remove IBST-1 2) => (make-node 1 (make-node 0 LEAF LEAF) LEAF)
;; (ibst-remove IBST-1 3) => IBST-1
;; (ibst-remove IBST-24 20) => (make-node 24 LEAF LEAF)
;; (ibst-remove IBST-3 1)
;;  =>(make-node 3 (make-node 2 (make-node 0 LEAF LEAF) LEAF) IBST-13)
;; (ibst-remove IBST-3 5)
;;  =>(make-node 3 IBST-1 (make-node 6 (make-node 4 LEAF LEAF) LEAF))
;; (ibst-remove IBST-3 3)
;;  =>(make-node 5 (make-node 4 IBST-1 LEAF) IBST-6)
;; (ibst-remove IBST-30 24)
;;  =>(make-node 30 IBST-20 IBST-35)
;; (ibst-remove IBST-50 55)
;;  =>(make-node 50 IBST-45 IBST-60)

;;;; Function Definition
(define (ibst-remove ibst int)
  (cond
    [(leaf? ibst) LEAF]
    [(node? ibst) (if (ibst-contains? ibst int)
                       (if (= (node-value ibst) int)
                           (cond
                             [(childless? ibst) LEAF]
                             [(leaf? (node-right ibst)) (node-left ibst)]
                             [(leaf? (node-left ibst)) (node-right ibst)]
                             [else
                              (make-node
                               (ibst-min (node-right ibst))
                               (node-left ibst)
                               (ibst-remove (node-right ibst)
                                            (ibst-min (node-right ibst))))])
                           (if (ibst-contains? (node-left ibst) int)
                               (make-node (node-value ibst)
                                          (ibst-remove (node-left ibst) int)
                                          (node-right ibst))                                                    
                               (make-node (node-value ibst)
                                          (node-left ibst)
                                          (ibst-remove (node-right ibst)
                                                       int))))
                       ;;> Instead of checking if the left or right contains
                       ;;> the value, you should use the properties of an IBST
                       ;;> to guide you down the left or right subtree.
                       ibst)]))

;;> -1 Need a helper function here. This function is too messy and it is
;;> difficult to quickly figure out what is going on. 

;;;; Tests
(check-expect (ibst-remove LEAF 0)
              LEAF)
(check-expect (ibst-remove IBST-0 0)
              LEAF)
(check-expect (ibst-remove IBST-1 0)
              (make-node 1 LEAF (make-node 2 LEAF LEAF)))
(check-expect (ibst-remove IBST-24 20)
              (make-node 24 LEAF LEAF))
(check-expect (ibst-remove IBST-55 60)
              (make-node 55 LEAF LEAF))
(check-expect (ibst-remove IBST-24 24)
              IBST-20)
(check-expect (ibst-remove IBST-55 55)
              IBST-60)
(check-expect (ibst-remove IBST-30 24)
              (make-node 30 IBST-20 IBST-35))
(check-expect (ibst-remove IBST-3 1)
              (make-node 3 (make-node 2 IBST-0 LEAF) IBST-5))
(check-expect (ibst-remove IBST-3 5)
              (make-node 3 IBST-1 (make-node 6 IBST-4 LEAF)))
(check-expect (ibst-remove IBST-3 3)
              (make-node 4 IBST-1 (make-node 5 LEAF IBST-6)))

;;;; Signature
;; ibst-min : IBST -> Integer

;;;; Purpose
;; GIVEN: an integer binary search tree
;; RETURN: the minimum integer that is stored in the binary tree

;;;; Examples
;; (ibst-min LEAF) => "LEAF does not have minimum"
;; (ibst-min IBST-0) => 0
;; (ibst-min IBST-1) => 0
;; (ibst-min IBST-3) => 0
;; (ibst-min IBST-13) => 10
;; (ibst-min IBST-30) => 20
;; (ibst-min IBST-50) => 45
;; (ibst-min IBST-55) => 55

;;;; Function Definition
(define (ibst-min ibst)
  (cond
    [(leaf? ibst) (error "LEAF does not have minimum")]
    [(node? ibst) (if (leaf? (node-left ibst))
                      (node-value ibst)
                      (ibst-min (node-left ibst)))]))
;;> -0.5 Violation of signature. If you wanted to specify that leaves are not
;;> valid input, you should have changed the signature

;;;; Tests
(check-error (ibst-min LEAF)
              "LEAF does not have minimum")
(check-expect (ibst-min IBST-0)
              0)
(check-expect (ibst-min IBST-1)
              0)
(check-expect (ibst-min IBST-3)
              0)
(check-expect (ibst-min IBST-13)
              10)
(check-expect (ibst-min IBST-30)
              20)
(check-expect (ibst-min IBST-50)
              45)
(check-expect (ibst-min IBST-55)
              55)



;;;; Problem 3


;;;; Data Definition

;; a direction is one of
;;  - 'up
;;  - 'down
;;  - 'left
;;  - 'right
;; INTERP: a direction

;; Deconstructor Template
;; direction-fn : Direction -> ???
#; (define (direction-fn direction)
     (cond
       [(symbol=? direction 'up)...]
       [(symbol=? direction 'down)...]
       [(symbol=? direction 'left)...]
       [(symbol=? direction 'right)...]))
       

(define-struct bullet [location radius direction speed])

;; A Bullet is (make-bullet Posn PosInt Direction PosInt)
;; INTERP: represents a bullet with its current location,
;;         the bullet's radius, the bullet's direction and
;;         the bullet's speed

;; Examples
(define BULLET-1 (make-bullet (make-posn 30 40) 10 'down 2))
(define BULLET-2 (make-bullet (make-posn 50 50) 12 'right 4))
(define BULLET-3 (make-bullet (make-posn 150 120) 30 'left 3))
(define BULLET-4 (make-bullet (make-posn 200 180) 25 'up 1))

;; Deconstructor Template
;; bullet-fn : Bullet-> ???
#; (define (bullet-fn bullet)
     ...(bullet-location bullet)...
     ...(bullet-radius bullet)...
     ...(bullet-direction bullet)...
     ...(bullet-speed bullet)...)

;; A ListOfBullets (LoB) is one of
;; - empty
;; - (cons Bullet LoB)
;; INTERP: represents a list of bullets

;; Examples
(define MTLOB empty)
(define LOB-1 (list BULLET-1))
(define LOB-2 (list BULLET-1 BULLET-2))
(define LOB-3 (list BULLET-2 BULLET-3 BULLET-4))

;; Deconstructor Template
;; lob-fn : LoB -> ???
#; (define (lob-fn lob)
     (cond
       [(empty? lob) ...]
       [(cons? lob) ...(bullet-fn (first lob))...
                    ...(lob-fn (rest lob))...]))
     
;;;; Signature
;; bullets-draw : LoB -> Images

;;;; Purpose
;; GIVEN: a list of bullets
;; RETURN: the bullets drawn on a canvas

(define CANVAS (empty-scene 800 500))
;;;; Examples
;; (bullets-draw MTLOB) => (empty-scene 800 500)
;; (bullets-draw LOB-1)
;;  => (place-image (circle 10 "solid" "blue") 30 40 CANVAS)
;; (bullets-draw LOB-2)
;;  =>  (place-image (circle 12 "solid" "blue") 50 50
;;              (place-image (circle 10 "solid" "blue") 30 40 CANVAS)) 
;; (bullets-draw LOB-3)
;;  => (place-image (circle 25 "solid" "blue") 200 180
;;            (place-image (circle 30 "solid" "blue") 150 120
;;                    (place-image (circle 12 "solid" "blue") 50 50 CANVAS)))

;;;; Function Definition
(define (bullets-draw lob)
  (cond
    [(empty? lob) CANVAS]
    [(cons? lob) (place-image
                  (circle (bullet-radius (first lob)) "solid" "blue")
                  (posn-x (bullet-location (first lob)))
                  (posn-y (bullet-location (first lob)))
                  (bullets-draw (rest lob)))]))
                  
;;;; Tests
(check-expect (bullets-draw MTLOB)
              (empty-scene 800 500))
(check-expect (bullets-draw LOB-1)
              (place-image (circle 10 "solid" "blue") 30 40 CANVAS))
(check-expect (bullets-draw LOB-2)
              (place-image (circle 12 "solid" "blue") 50 50
                           (place-image (circle 10 "solid" "blue") 30 40
                                        CANVAS))) 
(check-expect (bullets-draw LOB-3)
              (place-image (circle 25 "solid" "blue") 200 180
                           (place-image (circle 30 "solid" "blue") 150 120
                                        (place-image
                                         (circle 12 "solid" "blue")
                                         50
                                         50
                                         CANVAS))))

;;;; Signature
;; bullets-move : LoB -> LoB

;;;; Purpose
;; GIVEN: a list of bullets
;; RETURN: the list of bullets with each bullte moved a speed unit toward its
;;         direction


;;;; Examples
;; (bullets-move MTLOB) =>  MTLOB
;; (bullets-move LOB-1)
;; => (list (make-bullet (make-posn 30 42) 10 'down 2))
;; (bullets-move LOB-2)
;; => (list (make-bullet (make-posn 30 42) 10 'down 2)
;;          (make-bullet (make-posn 54 50) 12 'right 4))
;; (bullets-move LOB-3)
;; => (list (make-bullet (make-posn 54 50) 12 'right 4)
;;          (make-bullet (make-posn 147 120) 30 'left 3)
;;          (make-bullet (make-posn 200 179) 25 'up 1))

;;;; Function Definition
(define (bullets-move lob)
  (cond
    [(empty? lob) empty]
    [(cons? lob) (cons
                  (make-bullet (location-move (first lob))
                               (bullet-radius (first lob))
                               (bullet-direction (first lob))
                               (bullet-speed (first lob)))
                  (bullets-move (rest lob)))]))
     

;; Tests
(check-expect (bullets-move MTLOB)
              empty)
(check-expect (bullets-move LOB-1)
              (list (make-bullet (make-posn 30 42) 10 'down 2)))
(check-expect (bullets-move LOB-2)
              (list (make-bullet (make-posn 30 42) 10 'down 2)
                    (make-bullet (make-posn 54 50) 12 'right 4)))
(check-expect (bullets-move LOB-3)
              (list (make-bullet (make-posn 54 50) 12 'right 4)
                    (make-bullet (make-posn 147 120) 30 'left 3)
                    (make-bullet (make-posn 200 179) 25 'up 1)))

;;;; Signature
;; location-move : Bullet -> Location

;;;; Purpose
;; GIVEN: a bullet 
;; RETURN: the bullet's new location which is
;          moved towards its direction for a unit of its speed

;;;; Examples
;; (location-move BULLET-1) => (make-posn 30 42)
;; (location-move BULLET-2) => (make-posn 54 50)
;; (location-move BULLET-3) => (make-posn 147 120)
;; (location-move BULLET-4) => (make-posn 200 179)

;;;; Function Definition
(define (location-move bullet)
  (cond
    [(symbol=? (bullet-direction bullet) 'up)
     (make-posn (posn-x (bullet-location bullet))
                (- (posn-y (bullet-location bullet))
                   (bullet-speed bullet)))]
    [(symbol=? (bullet-direction bullet) 'down)
     (make-posn (posn-x (bullet-location bullet))
                (+ (posn-y (bullet-location bullet))
                   (bullet-speed bullet)))]
    [(symbol=? (bullet-direction bullet) 'left)
     (make-posn (- (posn-x (bullet-location bullet))
                   (bullet-speed bullet))
                (posn-y (bullet-location bullet)))]
    [(symbol=? (bullet-direction bullet) 'right)
     (make-posn (+ (posn-x (bullet-location bullet))
                   (bullet-speed bullet))
                (posn-y (bullet-location bullet)))]))

;;;; Tests
(check-expect (location-move BULLET-1)
              (make-posn 30 42))
(check-expect (location-move BULLET-2)
              (make-posn 54 50))
(check-expect (location-move BULLET-3)
              (make-posn 147 120))
(check-expect (location-move BULLET-4)
              (make-posn 200 179))

