;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment-8-yuanjieyue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;; Assignment 8

;;;; Problem 1

;;;; Data Definition
;; A List<X> is one of
;; - empty
;; - (cons X List<X>)
;; INTERP: represents a list of elements

;;;; Signature
;; list->chunks: List<X> PosInt -> List<List<X>>
;;;; Purpose
;; GIVEN: a list of elements and a number 
;; RETURNS: a list of list of elements that each inner list has the given
;;          number of elements

;; H.M.: (length List<X>)
;; T.A.: For n = 0, we do not recur.
;;       For n > 0 we recur on on n-1 and subtracting will eventually drcrease
;;       n to 0.

;;;; Function Definition
(define (list->chunks lox n)
  (cond
    [(empty? lox) empty]
    [(cons? lox)
     (local (;;;; Signature
             ;; first-n-elements-of-list: List<X> PosInt -> List<X>
             ;;;; Purpose
             ;; GIVEN: a list of elements and a number.
             ;; RETURNS: a list of the first given number of elements
             ;;          in the list.
             ;; H.M.: n
             ;; T.A.: For n = 0 we do not recur.
             ;;       For n > 1 we recur on on n-1, and subtracting will 
             ;;       eventually decreasen to 0.

             ;;;; Function Definition
             (define (first-n-elements-of-list lox n)
               (cond
                 [(empty? lox) empty]
                 [(cons? lox) (if (= n 0)
                                  empty
                                  (cons
                                   (first lox)
                                   (first-n-elements-of-list (rest lox)
                                                             (sub1 n))))]))
             ;;;; Signature
             ;; remove-first-n-elements-from-list: List<X> PosInt -> List<X>
             ;;;; Purpose
             ;; GIVEN: a list of elements and a number.
             ;; RETURNS: the list with the first given number of elements
             ;;          removed.

             ;; H.M.: n
             ;; T.A.: For n = 0, we do not recur.
             ;;       For n > 0, we recur one (n - 1), and subtracting will
             ;;       eventually decrease n to 0.

             ;;;; Function Definition
             (define (remove-first-n-elements-from-list lox n)
               (cond
                 [(empty? lox) empty]
                 [(cons? lox)
                  (if (= n 0)
                      lox
                      (remove-first-n-elements-from-list (rest lox)
                                                         (- n 1)))])))
       (cons (first-n-elements-of-list lox n)
             (list->chunks (remove-first-n-elements-from-list lox n)
                           n)))]))

;;;; Tests
(check-expect (list->chunks L1 3)
              (list (list "a" "b" "c") (list "d" "e" "f")))




;;;; Problem 2

;;;; Data Definition
;; A Letter is one of
;; - "A"
;; - "C"
;; - "G"
;; - "T"
;; INTERP: represents the 4 kinds of letters in DNA

;;;; Deconstructor Template
;; char-fn: Char -> ???
#; (define (char-fn a-char)
     (cond
       [(string=? a-char "A") ...]
       [(string=? a-char "C") ...]
       [(string=? a-char "G") ...]
       [(string=? a-char "T") ...]))

;; A DNA is a string
;; WHERE: all characters in the string are capital letters
;; INTERP: represents a consecutive piece of dna
(define DNA-MT "")
(define DNA-1 "AAGCCCTTAAAAAAAAAA")

;; A AbbrDNA is a string
;; WHERE: the string consists of letter and number alternately
;; INTERP: represents a piece of dna with the continuous letter duplicates 
;;         parts replaced by the number of duplicates.
(define ABBR-DNA-MT "")
(define ABBR-DNA-1 "A2G1C3T2A10")

;;;; Signature
;; dna-encode: DNA -> AbbrevDNA
;;;; Purpose
;; GIVEN: a piece of DNApurely letters.
;; RETURNS: the piece of DNA with each duplicate part of letters replaced
;;          by the number of its duplicate times.

;;;; Function Definition
(define (dna-encode dna)  
  (local (;;;;Signature
          ;; replace-duplicates: List<String> -> List<String>
          ;;;; Purpose
          ;; GIVEN: a list of strings.
          ;; RETURNS: the list of strings with continuous duplicates character
          ;;          elements replaced by the string of the number of its
          ;;          duplicate times.
          ;; H.M.: (length los)
          ;; T.A.:For an empty list, H.M. is 0 and the implementation returns
          ;;      with no recursive call. 
          ;;      For a non-empty list, H.M. is n, the implementation recurs
          ;;      on the (rest lon). H.M. for (rest lon) is n - 1 so each
          ;;      recursive call decreases H.M. by 1 and eventually hits 0.
          
          ;;;; Function Definition
          (define (replace-duplicates los)
            (cond
              [(empty? los) empty]
              [(cons? los)
               (local
                 (;;;; Signature
                  ;; count-duplicates: String List<String> -> PosInt
                  ;;;; Purpose
                  ;; GIVEN: a string and a list of string
                  ;; RETURNS: the list of strings of the number of 
                  ;;          continuous duplicate strings that in the 
                  ;;          front of the list as the given string
                  
                  ;; H.M.: (length los)
                  ;; T.A.: For empty list, H.M. is 0 and the implementation
                  ;;       returns with no recursive call. 
                  ;;       For a non-empty list, H.M. is n, the implementation
                  ;;       recurs on the (rest lon). H.M. for (rest lon) is
                  ;;       n - 1, so each recursive call decreases H.M. by 1
                  ;;       and eventually hits 0.
                  
                  ;;;; Function Definition
                  (define (count-duplicates str los)
                    (cond
                      [(empty? los) 1]
                      [(cons? los) (if (string=? str (first los))
                                       (+ 1
                                          (count-duplicates str
                                                            (rest los)))
                                       1)]))
                  ;;;; Signature
                  ;; remove-dup: List<String> PosInt -> List<String>
                  ;;;; Purpose
                  ;; GIVEN: a list of strings and an integer
                  ;; RETURNS: the list of strings with the first
                  ;;          number of given integer of strings
                  ;;          removed.
                  
                  ;; H.M.: n
                  ;; T.A.: For n = 0, and the implementation returns with no
                  ;;       recursive calls.
                  ;;       For n > 0, the implementation recurs on (n - 1),
                  ;;       so each recursive call decreases H.M. by 1 and
                  ;;       eventually hits 0.

                  ;;;; Function Definition
                  (define (remove-dup los n)
                    (cond
                      [(empty? los) empty]
                      [(cons? los) (if (= n 0)
                                       los
                                       (remove-dup (rest los)
                                                   (- n 1)))])))                                             
                 (cons (string-append (first los)
                                      (number->string (count-duplicates
                                                       (first los)
                                                       (rest los))))     
                       (replace-duplicates (remove-dup los
                                                       (count-duplicates
                                                        (first los)
                                                        (rest los))))))])))
    (foldr string-append "" (replace-duplicates (explode dna)))))

;;;; Tests
(check-expect (dna-encode DNA-MT)
              ABBR-DNA-MT)
(check-expect (dna-encode "A")
              "A1")
(check-expect (dna-encode DNA-1)
              ABBR-DNA-1)



;;;; Signature
;; dna-decode: AbbrDNA -> DNA
;;;; Purpose
;; GIVEN: an abbrevated piece of DNA
;; RETURNS: the original piece of DNA that consists of purely captial letters.

;;;; Function Definition
(define (dna-decode abbr-dna)
  (local
    (;;;; Signature
     ;; append-continous-numeric-strings: List<String> -> List<String>
     ;;;; Purpose
     ;; GIVEN: a list of strings that contains alphabetic string and numeric
     ;;        strings.
     ;; RETURNS: the list of strings with the continuous numeric strings
     ;;          appended to one string.
     
     ;; H.M.: (length los)
     ;; T.A.: Given an empty list, the implementation returns no recursive
     ;;       calls.
     ;;       Given an non-empty list, the implementation will recurs on
     ;;       (rest los), H.M on (rest los) is n-1, so each recursive call
     ;;       decrease H.M. by 1, and eventually his 0.

     ;;;; Function Definition
     (define (append-continuous-numeric-strings los)
       (cond
         [(empty? los) empty]
         [(cons? los)
          (local
            (;;;; Signature
             ;; new-head: List<String> -> String
             ;;;; Purpose
             ;; GIVEN: a list of strings that contains  alphabetic string and
             ;;        numeric strings, and the string is headed with numeric
             ;;        strings.
             ;; RETURNS: the list with continuous numeric strings at the head
             ;;          of the list appended to one string.
             
             ;; H.M.: (length los)
             ;; T.A.: Given an empty list, the implementation returns no 
             ;;       recursive calls.
             ;;       Given an non-empty list, the implementation will recurs
             ;;       on(rest los), H.M on (rest los) is n-1, so each recursive
             ;;       call decrease H.M. by 1, and eventually his 0.
             
             ;;;; Function Definition
             (define (new-head los)
               (cond
                 [(empty? los) ""]
                 [(cons? los) (if (string-numeric? (first los))
                                  (string-append (first los)
                                                 (new-head (rest los)))
                                  "")]))
             ;;;; Signature
             ;; remove-number-at-head: List<String> -> List<String>
             ;;;; Purpose
             ;; GIVEN:a list of strings that contains  alphabetic string and
             ;;        numeric strings, and the string is headed with numeric
             ;;        strings.
             ;; RETURNS: the list of strings with the continuous numeric stirng
             ;;          at the head of the list removed.
             
             ;; H.M.: (length los)
             ;; T.A.: Given an empty list, the implementation returns no 
             ;;       recursive calls.
             ;;       Given an non-empty list, the implementation will recurs 
             ;;       on (rest los), H.M on (rest los) is n-1, so each 
             ;;       recursive call decrease H.M. by 1, and eventually his 0.
             
             ;;;; Function Definition
             (define (remove-number-at-head los)
               (cond
                 [(empty? los) empty]
                 [(cons? los) (if (string-numeric? (first los))
                                  (remove-number-at-head (rest los))
                                  los)])))                          
            (if (string-numeric? (first los))
                (cons (new-head los)
                      (append-continuous-numeric-strings
                       (remove-number-at-head los)))                                                        
                (cons (first los)
                      (append-continuous-numeric-strings (rest los)))))]))
     ;;;; Signature
     ;; recover-duplicate: List<String> -> List<String>
     ;;;; Purpose
     ;; GIVEN: a list of strings of letters and postive integers
     ;; RETURNS: the list of strings with the strings of number in the list
     ;;          replaced by a string of letters that consists of the number of
     ;;          the letter in front of the number.
     
     ;; H.M.: n
     ;; T.A.: For H.M.= 1, the implementation returns no recursive
     ;;       calls.
     ;;       For H.M. > 1, the implementation will recurs on
     ;;       (n - 1), so each recursive call decrease H.M.
     ;;       by 1, and eventually his 0.
     
     ;;;; Function Definition
     (define (recover-duplicate los)
       (cond
         [(empty? los) empty]
         [(cons? los)
          (local (;;;; Signature
                  ;; replace-num-with-letters: String PosInt -> String
                  ;;;; Purpose
                  ;; GIVEN: a string of letter and a string of number
                  ;; RETURNS: the string of letters that consists of
                  ;;          the number value of the second give string of
                  ;;          the first given letter..
                  ;; H.M.: n
                  ;; T.A.: For H.M.= 1, the implementation returns no recursive
                  ;;       calls.
                  ;;       For H.M. > 1, the implementation will recurs on
                  ;;       (n - 1), so each recursive call decrease H.M.
                  ;;       by 1, and eventually his 0.

                  ;;;; Function Definition
                  (define (generate-duplicates str num)
                    (cond
                      [(= 1 num) ""]
                      [else (string-append str
                                           (generate-duplicates str
                                                                (- num 1)))])))
            (if (string-numeric? (first (rest los)))
                (cons (string-append
                       (first los)
                       (generate-duplicates
                        (first los)
                        (string->number (first (rest los)))))              
                      (recover-duplicate (rest (rest los))))
                (recover-duplicate (rest los))))])))   
    (foldr string-append
           ""
           (recover-duplicate (append-continuous-numeric-strings 
                               (explode abbr-dna))))))

;;;; Tests
(check-expect (dna-decode ABBR-DNA-MT)
              DNA-MT)
(check-expect (dna-decode ABBR-DNA-1)
              DNA-1)

;;;; Problem 3

;;;; Signature
;; bubble-sort: List<Number> -> List<Number>
;;;; Purpose
;; GIVEN: a list of numbers
;; RETURNS: the list of numbers in which all numbers are in sorted order
;; H.M.: (length lon)
;; T.A.: Given an empty list, the implementation returns no recursive
;;       calls.
;;       Given an non-empty list, the implementation will recurs on
;;       (rest los), H.M on (rest los) is n-1, so each recursive call
;;       decrease H.M. by 1, and eventually his 0.

;;;; Function Definition
(define (bubble-sort lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) lon]
    [else
     (local ((define times (length lon))
             ;;;; Signature
             ;; swap-elements: List<Number> Number -> List<Number>
             ;;;; Purpose
             ;; GIVEN: a list of numbers and a number 
             ;; RETURNS: a list of numbers with adjacent inverted elements
             ;;          swapped with each other.
             ;; H.M.: (length lon)
             ;; T.A.: Given an empty list, the implementation returns no
             ;;       recursive calls.
             ;;       Given an non-empty list, the implementation will recurs
             ;;       on (rest los), H.M on (rest los) is n-1, so each
             ;;       recursive call decrease H.M. by 1, and eventually his 0.
             ;;;; Function Definition
             (define (swap-elements lon)
               (cond
                 [(empty? lon) empty]
                 [(empty? (rest lon)) lon]
                 [else  (if (> (first lon)
                               (first (rest lon)))
                            (cons (first (rest lon))     
                                  (swap-elements (cons (first lon)
                                                       (rest (rest lon)))))                                                          
                            (cons (first lon)
                                  (swap-elements (rest lon))))]))            
             ;;;; Signature
             ;; keep-calling-swap: List<Number> Number -> List<Number>
             ;;;; Purpose
             ;; GIVEN: a list of numbers and a number
             ;; RETURNS: swap the adjacent inverted elements in the list for
             ;;          the given number of times
             ;; H.M.: n
             ;; T.A.: For H.M.= 1, the implementation returns no recursive
             ;;       calls.
             ;;       For H.M. > 1, the implementation will recurs on
             ;;       (n - 1), so each recursive call decrease H.M.
             ;;       by 1, and eventually his 0. 
             ;;;; Function Definition
             (define (keep-calling-swap lon num)
               (cond
                 [(= num 1) (swap-elements lon)]
                 [(> num 1) (swap-elements (keep-calling-swap lon
                                                              (- num 1)))])))
       (keep-calling-swap lon times))]))
                                     
;;;; Tests
(define LON-MT empty)
(define LON-1 (list 1 2))
(define LON-2 (list 2 4 3 8 5 4 9 10))
(define LON-3 (list 10 9 8 7 6 5 4 3 2 1))
(check-expect (bubble-sort LON-MT)
              LON-MT)
(check-expect (bubble-sort LON-1)
              LON-1)
(check-expect (bubble-sort LON-2)
              (list 2 3 4 4 5 8 9 10))
(check-expect (bubble-sort LON-3)
              (list 1 2 3 4 5 6 7 8 9 10))