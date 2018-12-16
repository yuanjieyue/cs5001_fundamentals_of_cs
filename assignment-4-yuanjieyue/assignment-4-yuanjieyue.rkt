;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment-4-yuanjieyue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;> TOTAL: 137.5/151
;;> -1 for grammar


;;;; Assignment 4

;;;; Problem 1
;; 1. Provide the data definition for a list of strings (los)

;;;; Data Definition
;; A ListOfString (LoS) is one of
;; - empty
;; - (cons String LoS)
;; INTERP: represents a list of strings



;; Deconstructor Template
;; los-fn : LoS -> ???
#;(define (los-fn los)
    (cond
      [(empty? los) ...]
      [(cons? los) ... (first los) ...
                   ... (los-fn (rest los)) ...]))


;; 2.Design the function los-total-length that consumes a list of strings and
;;    returns the sum of the length of each element in the list.

;;;; Signature
;; los-total-length : LoS -> NonNegInteger

;;;; Purpose
;; GIVEN: a list of strings
;; RETURN: a non negative integer
;;> -0.5 Your purpose statement should tell me what your function is doing. This
;;> tells me nothing. Are you going to just return a random NonNegInt?

;;;; Examples
;; (los-total-length empty) => error "Empty list do not have length"
;; (los-total-length (cons "" empty) => 0
;; (los-total-length (cons "a" empty)) => 1
;; (los-total-length (cons "aa" (cons "a" empty))) => 3

;;;; Function Definition
(define (los-total-length los)
    (cond
     [(empty? los) (error "Empty list do not have length")]
     [(cons? los) (if (empty? (rest los))
                      (string-length (first los))
                      (+ (string-length (first los))
                         (los-total-length (rest los))))]))
;;> -0.5 Violated the signature. Throwing an error is not a NonNegInt and your
;;> signature says you only return NonNegInts.
;;> -0.5 Empty lists do have length... their length is 0. 

;;;; Tests
(check-error (los-total-length empty)
             "Empty list do not have length")
(check-expect (los-total-length (cons "" empty))
              0)
(check-expect (los-total-length (cons "a" empty))
              1)
(check-expect (los-total-length (cons "aa" (cons "a" empty)))
              3)

;; 3.Design the function los-contains that consumes a list of strings
;;   string-list and a string s and returns true if s is in string-list,
;;   otherwise returns false.

;;;; Signature
;; los-contains? : LoS String -> Boolean

;;;; Purpose
;; GIVEN: a list of string and a string ;;> Grammar: list of strings*
;; RETURN: true if the string appears in the list
;;         false otherwise

(define MTLOS empty)
(define 1LOS (cons "" MTLOS))
(define 2LOS (cons "a" MTLOS))
(define 3LOS (cons "a" (cons "" MTLOS)))
(define 4LOS (cons "hello" (cons "a" (cons " " MTLOS))))
(define NLOS (cons "hello" (cons " " (cons "aa" (cons "" (cons "wo" MTLOS))))))

;;;; Examples
;; (los-contains MTLOS "") => #false
;; (los-contains 1LOS "") => #true
;; (los-contains 2TLOS "") => #false
;; (los-contains 2TLOS "a") => #true
;; (los-contains 3TLOS "a") => #true
;; (los-contains 4TLOS " ") => #true
;; (los-contains NLOS "wo") => #true

;;;; Function Definition
(define (los-contains? string-list s)
  (cond
    [(empty? string-list) #false]
    [(cons? string-list) (if (string=? (first string-list) s)
                             #true
                             (los-contains? (rest string-list) s))]))

;;;; Tests
(check-expect (los-contains? MTLOS "")
              #false)
(check-expect (los-contains? 1LOS "")
              #true)
(check-expect (los-contains? 2LOS "")
              #false)
(check-expect (los-contains? 2LOS "a")
              #true)
(check-expect (los-contains? 3LOS "a")
              #true)
(check-expect (los-contains? 4LOS " ")
              #true)
(check-expect (los-contains? NLOS "wo")
              #true)


;; 4. Design the function los-replace-first that consumes a list of strings
;;    string-list and two strings  old and new and returns the string-list
;;    but with the first  old replaced by new. For example

;;;; Signature
;; los-replace-first : LoS String String -> LoS

;;;; Purpose
;; GIVEN: a list of strings and two strings
;; RETURN: a list of strings
;;> -0.5 What is your function doing? What is the PURPOSE of your function?


(define ALOS (cons "a" (cons "b" (cons "a" empty))))
(define NALOS (cons "x" (cons "b" (cons "a" empty))))
;;;; Examples
;; (los-replace-first ALOS "a" "x") => NALOS
;; (los-replace-first ALOS "c" "x") => ALOS
;; (los-replace-first MTLOS "a" "x") => MTLOS
;; (los-replace-first 1LOS "a" "x") => 1LOS
;; (los-replace-first 1LOS "" "x") => (cons "x" empty)
;; (los-replace-first 2LOS "a" "x") => (cons "x" empty)
;; (los-replace-first NLOS "hello" "world")
;; => (cons "world" (cons " " (cons "aa" (cons "" (cons "wo" MTLOS))))))

;;;; Function Definition
(define (los-replace-first string-list old new)
  (cond
    [(empty? string-list) empty]
    [(cons? string-list) (if (los-contains? string-list old)
                             (replace-string string-list old new)
                             string-list)]))
;;;; Signature
;; replace-string : LoS String String -> LoS

;;;; Purpose
;; GIVEN: a list of strings and two strings
;; RETURN: a list of strings
;;> -0.5 What is your function doing? What is the PURPOSE of your function?

;; Examples 
;;(check-expect (replace-string ALOS "a" "x") => NALOS
;;(check-expect (replace-string 1LOS "" "x") => (cons "x" empty)
;;(check-expect (replace-string 2LOS "a" "x") =>  (cons "x" empty)            
;;(check-expect (replace-string NLOS "hello" "world")
;; => (cons "world" (cons " " (cons "aa" (cons "" (cons "wo" MTLOS)))))

;;;; Function Definition
(define (replace-string string-list old new)
  (if (string=? (first string-list)
                old)
      (cons new (rest string-list))
      (cons (first string-list)
            (replace-string (rest string-list) old new))))
;;> -0.5 Violated the signature. An empty list is a LoS. If I pass you an empty
;;> list, your function breaks.  

;; Tests
(check-expect (replace-string ALOS "a" "x") NALOS)
              
(check-expect (replace-string 1LOS "" "x") (cons "x" empty))
              
(check-expect (replace-string 2LOS "a" "x") (cons "x" empty))
             
(check-expect (replace-string NLOS " " "world")
              (cons "hello"
                    (cons "world"
                          (cons "aa"
                                (cons ""
                                      (cons "wo" MTLOS))))))

;;;; Tests
(check-expect (los-replace-first ALOS "a" "x")
              NALOS)
(check-expect (los-replace-first ALOS "c" "x")
              ALOS)
(check-expect (los-replace-first MTLOS "a" "x")
              MTLOS)
(check-expect (los-replace-first 1LOS "a" "x")
              1LOS)
(check-expect (los-replace-first 1LOS "" "x")
              (cons "x" empty))
(check-expect (los-replace-first 2LOS "a" "x")
              (cons "x" empty))
(check-expect (los-replace-first NLOS "hello" "world")
              (cons "world" (cons " " (cons "aa" (cons "" (cons "wo" MTLOS))))))


;; 5. Design the function los-replace-all that consumes a list of strings
;;    string-list and two strings  old and  new and returns the string-list
;;    but with all  old replaced by new. For example


;;;; Signature
;; los-replace-all : LoS String String -> LoS

;;;; Purpose
;; GIVEN: a list of strings and two strings
;; RETURN: a list of strings
;;> -0.5 What is your function doing? What is the PURPOSE of your function?

(define NNALOS (cons "x" (cons "b" (cons "x" empty))))
;;;; Examples
;; (los-replace-all ALOS "a" "x") => NNALOS
;; (los-replace-all ALOS "c" "x") => ALOS
;; (los-replace-all MTLOS "a" "x") => MTLOS
;; (los-replace-all 1LOS "a" "x") => 1LOS


;;;; Function Definition
(define (los-replace-all string-list old new)
  (cond
    [(empty? string-list) empty]
    [(cons? string-list)
     (if (los-contains? string-list old)
         (los-replace-all (replace-string string-list old new) old new)
         string-list)]))

;;> What is replace-string doing here? You already defined it. You should remove
;;> this; it adds clutter. 
;;;; Signature
;; replace-string : LoS String String -> LoS

;;;; Purpose
;; GIVEN: a list of strings and two strings
;; RETURN: a list of strings

;; Examples 
;; (replace-string ALOS "a" "x") => NALOS
              
;; (replace-string 1LOS "" "x") => (cons "x" empty)
              
;; (replace-string 2LOS "a" "x") =>  (cons "x" empty)         
;; (replace-string NLOS "hello" "world")
;;  => (cons "world" (cons " " (cons "aa" (cons "" (cons "wo" MTLOS)))))


;; Tests
(check-expect (replace-string ALOS "a" "x") NALOS)
              
(check-expect (replace-string 1LOS "" "x") (cons "x" empty))
              
(check-expect (replace-string 2LOS "a" "x") (cons "x" empty))
             
(check-expect (replace-string NLOS " " "world")
              (cons "hello"
                    (cons "world"
                          (cons "aa"
                                (cons ""
                                      (cons "wo" MTLOS))))))

;;;; Tests
(check-expect (los-replace-all ALOS "a" "x")
              NNALOS)
(check-expect (los-replace-all ALOS "c" "x")
              ALOS)
(check-expect (los-replace-all MTLOS "a" "x")
              MTLOS)
(check-expect (los-replace-all 1LOS "a" "x")
              1LOS)


;;;; Problem 2


;; 1. Provide a data definition for a list of lineitems

;; An Id is a Symbol
;; INTERP: represents a product's id 

;; A Name is a String 
;; INTERP: represents the product's name 

;; Dollars is a NonNegInteger
;; INTERP: represents the amount of dollars for a price

;; Cents is a NonNegInteger 
;; WHERE: cents is greater or equal to 0 
;;        and less or equal to 99
;; INTERP: represents the amount of cents for a price

(define-struct price [dollars cents])
;; A Price is (make-price Dollars Cents)
;; INTERP: represents total amount in dollars and cents.

;; Deconstructor Template
;; price-fn : Price -> ???
#; (define (price-fn price)
      ... (price-dollars price) ...
      ... (price-cents price) ...)

;; Examples
(define 0PRICE (make-price 0 0))
(define 1PRICE (make-price 0 49))
(define 2PRICE (make-price 0 99))
(define 3PRICE (make-price 2 50))
(define 4PRICE (make-price 3 99))
(define 5PRICE (make-price 10 30))

;; A Quantity is a PosInteger
;; INTERP: represent the number of items

(define-struct line-item [id name price quantity])
;; A LineItem is a (make-line-item Id Name Price Quantity)
;; INTERP: represents a line item with the products id, name price and 
;;         quantity

;; Examples
(define 0LINEITEM (make-line-item 'coke "Cola" 0PRICE 50))
(define 1LINEITEM (make-line-item 'sugar "Soda" 1PRICE 12))
(define 2LINEITEM (make-line-item 'wholefat "Milk" 2PRICE 40))
(define 3LINEITEM (make-line-item 'organic "Oragne Juice" 3PRICE 15))
(define 4LINEITEM (make-line-item 'organic "Apple Juice" 4PRICE 10))
(define 5LINEITEM (make-line-item 'Franch "Red Wine" 5PRICE 22))


;; Decontructor Template
;; line-item-fn : LineItem -> ???
#;(define (line-item-fn line-item)
    ...(line-item-id line-item) ...
    ...(line-item-name line-item) ...
    ...(price-fn (line-item-price line-item)) ...)
;;> -0.5 line-item-quantity?




;; A ListOfLineItems (LoLI) is one of
;; - empty
;; - (cons LineItem LoLI)
;; INTERP: represents a list of line items

;; Examples
(define MTLOLI empty)
(define 0LOLI (cons 0LINEITEM MTLOLI))
(define 1LOLI (cons 1LINEITEM 0LOLI))
(define 2LOLI (cons 2LINEITEM 1LOLI))
(define 3LOLI (cons 3LINEITEM 2LOLI))
(define NLOLI (cons 5LINEITEM
                    (cons 2LINEITEM
                          (cons 4LINEITEM
                                (cons 3LINEITEM
                                      (cons 0LINEITEM
                                            (cons 1LINEITEM MTLOLI)))))))


;; Deconstructor Template
;; loli-fn : LoLI -> ???
#; (define (loli-fn loli)
     (cond
       [(empty? loli) ...]
       [(cons? loli) ... (line-item-fn (first loli)) ...
                     ... (loli-fn (rest loli)) ...]))


;; 2. Design the function bill-total that consumes a list of lineitems
;;    and returns the total amount for the list of items.

;; Signature
;; bill-total : LoLi -> Price

;;;; Purpose
;; GIVEN: a list of line-items
;; RETURN: the total amount for the list of items

;; Examples
;; (bill-total MTLOLI) => error "Empty list of line-item does not have amount"
;; (bill-total 0LOLI) => (make-price 0 0)
;; (bill-total 1LOLI) => (make-price 5 88)
;; (bill-total 2LOLI) => (make-price 45 48)
;; (bill-total 3LOLI) => (make-price 83 98)
;; (bill-total NLOLI) => (make-price 350 48)

;;;; Function Definition
(define (bill-total loli)
  (cond
    [(empty? loli) (error "Empty list of line-item does not have amount")]
    [(cons? loli) (if (empty? (rest loli))
                      (one-amount (first loli))
                      ;;> if you properly implemented your base case, you
                      ;;> wouldn't have to check for empty here. 
                      (total-amount (one-amount (first loli))
                                    (bill-total (rest loli))))]))
;;> -0.5 Violated the signature. An Error is not a Price
;;> -0.5 Empty lists do have amounts .. it is (make-price 0 0)

;; Tests
(check-error (bill-total MTLOLI)
             "Empty list of line-item does not have amount")
(check-expect (bill-total 0LOLI)
              (make-price 0 0))
(check-expect (bill-total 1LOLI)
              (make-price 5 88))
(check-expect (bill-total 2LOLI)
              (make-price 45 48))
(check-expect (bill-total 3LOLI)
              (make-price 82 98))
(check-expect (bill-total NLOLI)
              (make-price 349 48))

;;;; Signature
;; one-amount: LineItem -> Price

;;;; Purpose
;; GIVEN: a line-item
;; RETURN: the total amount of this line-item

;;;; Examples
;; (one-amount 0LINEITEM) => (make-price 0 0)
;; (one-amount 1LINEITEM) => (make-price 5 88)
;; (one-amount 2LINEITEM) => (make-price 39 60)
;; (one-amount 3LINEITEM) => (make-price 37 50)
;; (one-amount 4LINEITEM) => (make-price 39 90)
;; (one-amount 5LINEITEM) => (make-price 226 60)

;;;; Function Definition
(define (one-amount line-item)
  (make-price
   (quotient (* (sum-of-cents (line-item-price line-item))
                (line-item-quantity line-item))
             100)
   (remainder (* (sum-of-cents (line-item-price line-item))
                 (line-item-quantity line-item))
              100)))
;;;; Tests
(check-expect (one-amount 0LINEITEM)
              (make-price 0 0))
(check-expect (one-amount 1LINEITEM)
              (make-price 5 88))
(check-expect (one-amount 2LINEITEM)
              (make-price 39 60))
(check-expect (one-amount 3LINEITEM)
              (make-price 37 50))
(check-expect (one-amount 4LINEITEM)
              (make-price 39 90))
(check-expect (one-amount 5LINEITEM)
              (make-price 226 60))

;;;; Signature
;; sum-of-cents : Price -> NonNegInteger

;;;; Purpose
;; GIVEN: a price
;; RETURN: the sum of cents of its total amount

;; Examples
;; (sum-of-cents 0PRICE) => 0
;; (sum-of-cents 1PRICE) => 49
;; (sum-of-cents 2PRICE) => 99
;; (sum-of-cents 3PRICE) => 250
;; (sum-of-cents 4PRICE) => 399
;; (sum-of-cents 5PRICE) => 1030

;;;; Function Definition
(define (sum-of-cents price)
  (+ (* 100
        (price-dollars price))
     (price-cents price)))

;; Tests
(check-expect (sum-of-cents 0PRICE)
              0)
(check-expect (sum-of-cents 1PRICE)
              49)
(check-expect (sum-of-cents 2PRICE)
              99)
(check-expect (sum-of-cents 3PRICE)
              250)
(check-expect (sum-of-cents 4PRICE)
              399)
(check-expect (sum-of-cents 5PRICE)
              1030)

;;;; Signature
;; total-amount : Price Price -> Price

;;;; Purpose
;; GIVEN: two prices
;; RETURN: the total amount of these two prices

;;;; Examples
;; (total-amount 0PRICE 1PRICE) => (make 0 49)
;; (total-amount 1PRICE 2PRICE) => (make 1 48)
;; (total-amount 2PRICE 3PRICE) => (make 3 49)

;;;; Function Definition
(define (total-amount p1 p2)
  (make-price
   (quotient (+ (sum-of-cents p1) (sum-of-cents p2))
             100)
   (remainder (+ (sum-of-cents p1) (sum-of-cents p2))
              100)))

;;;; Tests
(check-expect (total-amount 0PRICE 1PRICE)
              (make-price 0 49))
(check-expect (total-amount 1PRICE 2PRICE)
              (make-price 1 48))
(check-expect (total-amount 2PRICE 3PRICE)
              (make-price 3 49))




;;;; Problem 4
;; 1. Create data definitions to capture the information for your friend's
;;    daily planning.

;;;; Data Definition
;; A Id is a symbol ;;> -0.5 *Symbol (capitalize your data defs!)
;; INTERP: represents the symbol to identify a task

;; A Description is a string ;;> *String (capitalize your data defs!)
;; INTERP: represents the description for a task
;;> Are these data definitions necessary? how is an Id different from a Symbol
;;> or a Description different from a String?

;; An Estimate is one of the following PosInt
;; - 1
;; - 2
;; - 3
;; - 5
;; INTERP: represents the estimated numbers of hours that is needed to
;;         complete a task

;; Deconstructor Template
;; estimate-fn : Estimate -> ???
#; (define (estimate-fn estimate)
     (cond
       [(= estimate 1) ...]
       [(= estimate 2) ...]
       [(= estimate 3) ...]
       [(= estimate 5) ...]))

(define-struct task (id description estimate))
;; A Task is (make-task id description estimate)
;;> -1 *Id *Description *Estimate (capitalize!)
;; INTERP: represents a task with its id, description and estimate hours
;;> Grammar: *estimated number of hours
;;         that is needed to complete the task

;; Examples
(define TASK-1 (make-task 'PPT "Make a PPT for tomorrow's conference" 3))
(define TASK-2 (make-task 'Word "Write a document for team discussion" 2))
(define TASK-3 (make-task 'Drawing "Draw a blackboard to show to customers" 5))
(define TASK-4 (make-task 'Email "Reply the business emails" 1))

;; Deconstructor Template
;; task-fn : Task -> ???
#; (define (task-fn task)
     ... (task-id task) ...
     ... (task-description task) ...
     ... (estimate-fn (task-estimate task)) ...)

;; A ListOfTasks (LoT) is one
;; - empty
;; - (cons Task LoT)
;; INTERP: represents a list of tasks

;; Examples
(define MTLOT empty)
(define 1LOT (cons TASK-1 MTLOT))
(define 2LOT (cons TASK-2 1LOT))
(define 3LOT (cons TASK-3 2LOT))
(define 4LOT (cons TASK-4 3LOT))

;; Deconstructor Template
;; lot-fn : LoT -> ???
#; (define (lot-fn lot)
     (cond
       [(empty? lot) ...]
       [(cons? lot) ... (task-fn (first lot)) ...
                    ... (lot-fn (rest lot)) ...]))

;; A AvailableWorkHours (AWHours) is NonNegInteger ;;> Grammar: *is a
;; WHERE: awhours is greater or equal to 0
;;        and less or equal to 24 
;; INTERP: represents the number of hours that one has to work for a day

(define-struct work-day (lot awhours))
;; A WorkDay is (make-work-day lot awhours) ;;> -0.5 *LoT AWHours
;; INTERP: represents a work day
;;> -0.5 The INTERP statement for a struct should describe what the struct
;;> represents and what fields it has

;; Examples
(define WORKDAY-1 (make-work-day MTLOT 0))
(define WORKDAY-2 (make-work-day 1LOT 0))
(define WORKDAY-3 (make-work-day 1LOT 5))
(define WORKDAY-4 (make-work-day 2LOT 8))
(define WORKDAY-5 (make-work-day 3LOT 12))
(define WORKDAY-6 (make-work-day 4LOT 16))
(define WORKDAY-7 (make-work-day 4LOT 24))

;; Deconstructor Template
;; work-day-fn : WorkDay -> ???
#; (define (work-day-fn work-day)
     ... (lot-fn (work-day-lot work-day)) ...
     ... (work-day-awhours work-day) ...)

;; 2. Design a function called free-hours that consumes a workday and returns
;;    the difference between the workday's available hours and the sum of all
;;    estimated hours for that day's tasks.

;;;; Signature
;; free-hours : WorkDay -> Integer

;;;; Purpose
;; GIVEN: a work-day
;; RETURN: an integer
;;> -1 This is not a purpose statement. This is a list of inputs and outputs.
;;> Your purpose statement should tell me the PURPOSE of your function. 

;;;; Examples
;; (free-hours WORKDAY-1) => 0
;; (free-hours WORKDAY-2) => -3
;; (free-hours WORKDAY-3) => 2
;; (free-hours WORKDAY-4) => 3
;; (free-hours WORKDAY-5) => 2
;; (free-hours WORKDAY-6) => 5
;; (free-hours WORKDAY-7) => 13

;;;; Function Definition
(define (free-hours work-day)
  (- (work-day-awhours work-day)
     (sum-of-estimate (work-day-lot work-day))))

;;;; Tests
(check-expect (free-hours WORKDAY-1)
              0)
(check-expect (free-hours WORKDAY-2)
              -3)
(check-expect (free-hours WORKDAY-3)
              2)
(check-expect (free-hours WORKDAY-4)
              3)
(check-expect (free-hours WORKDAY-5)
              2)
(check-expect (free-hours WORKDAY-6)
              5)
(check-expect (free-hours WORKDAY-7)
              13)

;;;; Signature
;; sum-of-estimate : LoT -> PosInt
;;> Choose meaningful fn names. This could be 'total-estimated-hours'

;;;; Purpose
;; GIVEN: a list of tasks
;; RETURN: the sum of estimate number of hours that is needed to complete
;;         all the tasks in the list

;;;; Examples
;; (sum-of-estimate MTLOT) => 0
;; (sum-of-estimate 1LOT) => 3
;; (sum-of-estimate 2LOT) => 5
;; (sum-of-estimate 3LOT) => 10
;; (sum-of-estimate 4LOT) => 11

;;;; Function Definition
(define (sum-of-estimate lot)
  (cond
    [(empty? lot) 0]
    [(cons? lot) (+ (task-estimate (first lot))
                    (sum-of-estimate (rest lot)))]))

;;;; Tests
(check-expect (sum-of-estimate MTLOT)
              0)
(check-expect (sum-of-estimate 1LOT)
              3)
(check-expect (sum-of-estimate 2LOT)
              5)
(check-expect (sum-of-estimate 3LOT)
              10)
(check-expect (sum-of-estimate 4LOT)
              11)



;; 3. Design a function called overbooked? that consumes a workday and returns
;;    false if the total number of estimated hours for all tasks in the workday
;;    is less than or equal the available hours for that workday,
;;    and true otherwise.


;;;; Signature
;; overbooked? : WorkDay -> Boolean

;;;; Purpose
;; GIVEN: a work-day
;; RETURN: false if the total number of estimated hours for all tasks in the
;;         workday is less than or equal the available hours for that workday
;;         true otherwise

;;;; Examples
;; (overbooked? WORKDAY-1) => #false
;; (overbooked? WORKDAY-2) => #true
;; (overbooked? WORKDAY-3) => #false
;; (overbooked? WORKDAY-4) => #false
;; (overbooked? WORKDAY-5) => #false
;; (overbooked? WORKDAY-6) => #false
;; (overbooked? WORKDAY-2) => #false

;;;; Function Definition
(define (overbooked? work-day)
  (if (>= (free-hours work-day) 0)
      #false
      #true))
;;> -0.5 Unnecessary code. This is what you are doing:
#; (if (statement = true) return false
       else return true)
;;> An if statement will check if the statement evaluates to true and then
;;> return the answer. If the statement itself is returning a boolean, can you
;;> use that to your advantage?

;;;; Tests
(check-expect (overbooked? WORKDAY-1)
              #false)
(check-expect (overbooked? WORKDAY-2)
              #true)
(check-expect (overbooked? WORKDAY-3)
              #false)
(check-expect (overbooked? WORKDAY-4)
              #false)
(check-expect (overbooked? WORKDAY-5)
              #false)
(check-expect (overbooked? WORKDAY-6)
              #false)
(check-expect (overbooked? WORKDAY-7)
              #false)

;; 4. Desgin a function called add-task that consumes a workday and a
;;    a new task and adds the task to that workday.

;;;; Signature
;; add-task : WorkDay Task -> WorkDay

;;;; Purpose
;; GIVEN: a work day and a task
;; RETURN: the work day with the task added

(define TASK-5 (make-task 'Conversation
                         "Have a conversation with chief manager"
                         1))
(define TASK-6 (make-task 'GroupDiscuss "Attend a group discussion" 2))

;;;; Examples
;; (add-taks WORKDAY-1 TASK-5)
;; => (make-work-day (cons TASK-5 MTLOT) 0)
;; (add-task WORKDAY-2 TASK-5)
;; => (make-work-day (cons TASK-5 1LOT) 0)
;; (add-task WORKDAY-5 TASK-6)
;; => (make-work-day (cons TASK-6 3LOT) 12)

;;;; Function Definition
(define (add-task work-day task)
  (make-work-day (cons task
                       (work-day-lot work-day))
                 (work-day-awhours work-day)))


;;;; Tests
(check-expect (add-task WORKDAY-1 TASK-5)
              (make-work-day (cons TASK-5 MTLOT) 0))
(check-expect (add-task WORKDAY-2 TASK-5)
              (make-work-day (cons TASK-5 1LOT) 0))
(check-expect (add-task WORKDAY-5 TASK-6)
              (make-work-day (cons TASK-6 3LOT) 12))

;; 5. Design a function called completed-task that consumes a workday and
;;    a task id and returns an updated workday that does not include
;;    the taks with the provided task id.

;;;; Signature
;; completed-task : WorkDay Id -> WordDay

;;;; Purpose
;; GIVEN: a work day and a task id
;; RETURN: an updated work day which does not include the task with the id

;;;; Examples
;; (completed-task WORKDAY-1 'PPT)
;; => WORKDAY-1
;; (completed-task WORKDAY-2 'PPT)
;; => (make-work-day MTLOT 0)
;; (completed-task WORKDAY-4 'Word)
;; => (make-work-day (cons TASK-1 MTLOT) 8)
;; (completed-task WORKDAY-6 'Conversation)
;; => WORKDAY-6
;; (completed-task WORKDAY-7 'Drawing)
;; => (make-work-day (cons TASK-4 2LOT) 24)

;;;; Function Definition
(define (completed-task work-day id)
    (make-work-day (new-lot (work-day-lot work-day) id)
                   (work-day-awhours work-day)))

;;;; Tests
(check-expect (completed-task WORKDAY-1 'PPT)
              WORKDAY-1)
(check-expect (completed-task WORKDAY-2 'PPT)
              (make-work-day MTLOT 0))
(check-expect (completed-task WORKDAY-4 'Word)
              (make-work-day (cons TASK-1 MTLOT) 8))
(check-expect (completed-task WORKDAY-6 'Conversation)
              WORKDAY-6)
(check-expect (completed-task WORKDAY-7 'Drawing)
              (make-work-day (cons TASK-4 2LOT) 24))


;;;; Signature
;; new-lot : LoT Id -> LoT
;;> -0.5 bad fn name. Function names should be meaningful. If you are removing
;;> something from a list and returning a new list with the task removed, you
;;> could call it 'remove-task-from-list'. 'new-lot' gives me no clue that you
;;> are removing anything. 

;;;; Purpose
;; GIVEN: a list of tasks and a task id
;; RETURN: a list of tasks
;;> -0.5 bad purpose statement. See comments for other bad purpose statements.

;;;; Examples
;; (new-lot MTLOT 'PPT) => MTLOT
;; (new-lot 1LOT 'PPT) => MTLOT
;; (new-lot 1LOT 'Word) => 1LOT
;; (new-lot 2LOT 'PPT) => (cons TASK-2 MTLOT)
;; (new-lot 2LOT 'Word) => 1LOT
;; (new-lot 3LOT 'Word) => (cons TASK-3 1LOT)


;;;; Function Definition
(define (new-lot lot id)
  (cond
    [(empty? lot) empty]
    [(cons? lot) (if (lot-contains? lot id)
                     (remove-task lot id)
                     lot)]))

;;;; Tests
(check-expect (new-lot MTLOT 'PPT)
              MTLOT)
(check-expect (new-lot 1LOT 'PPT)
              MTLOT)
(check-expect (new-lot 1LOT 'Word)
              1LOT)
(check-expect (new-lot 2LOT 'PPT)
              (cons TASK-2 MTLOT))
(check-expect (new-lot 2LOT 'Word)
              1LOT)
(check-expect (new-lot 3LOT 'Word)
              (cons TASK-3 1LOT))

;;;; Signature
;; lot-contains : LoT Id -> Boolean

;;;; Purpose
;; GIVEN: a list of tasks and an id of a task
;; RETURN: true if the list of tasks contains the task with the id
;;         false otherwise

;;;; Examples
;; (lot-contains? MTLOT TASK-1) => #false
;; (lot-contains? 1LOT TASK-1) => #true
;; (lot-contains? 2LOT TASK-1) => #true
;; (lot-contains? 2LOT TASK-2) => #true
;; (lot-contains? 3LOT TASK-4) => #false

;;;; Function Definition
(define (lot-contains? lot id)
   (cond
     [(empty? lot) #false]
     [(cons? lot) (if (symbol=? (task-id (first lot))
                                id)
                      #true
                      (lot-contains? (rest lot) id))]))

;;;; Tests
(check-expect (lot-contains? MTLOT 'PPT)
              #false)
(check-expect (lot-contains? 1LOT 'PPT)
              #true)
(check-expect (lot-contains? 2LOT 'PPT)
              #true)
(check-expect (lot-contains? 2LOT 'Word)
              #true)
(check-expect (lot-contains? 3LOT 'Email)
              #false)

;;;; Signature
;; remove-task : LoT task -> LoT ;;> *Task 

;;;; Purpose
;; GIVEN: a list of tasks and a task
;; RETURN: a list of tasks
;;> -0.5 bad purpose statement


;;;; Examples
;; (remove-task 1LOT 'PPT) => MTLOT
;; (remove-task 2LOT 'PPT) => (cons TASK-2 MTLOT)
;; (remove-task 2LOT 'Word) => 1LOT
;; (remove-task 3LOT 'Word) => (cons TASK-3 1LOT)

;;;; Function Definition
(define (remove-task lot id)
  (cond
    [(empty? lot) empty] ;;> -0.5 black line
    [(cons? lot) (if (symbol=? (task-id (first lot))
                                id)
                     (rest lot)
                     (cons (first lot) (remove-task (rest lot) id)))]))
;;> Why didn't you just call this function in 'completed-task'? It would have
;;> accomplished the same exact thing. lot-contains and new-lot are unnecessary.

;;;; Tests
;; (remove-task MTLOT 'PPT) => MTLOT
;; (remove-task 1LOT 'PPT) => MTLOT
;; (remove-task 2LOT 'PPT) => (cons TASK-2 MTLOT)
;; (remove-task 2LOT 'Word) => 1LOT
;; (remove-task 3LOT 'Word) => (cons TASK-3 1LOT)



;; 6. Your friend would now like to plan his work week and he would like to you
;;    to design a program that will allow them to keep a list of workdays.
;;    They would also like you to design a program that given a list of
;;    workdays returns back the list of free hours, one for each workday.
;;    HINT: you might want to use your solution to part 2 of this problem.

;;;; Data Definition
;; A ListOfWorkDays (LoWD) is one of
;; - empty
;; - (cons WorkDay LoWD)
;; INTERP: represents a list of  work days

;; Examples
(define MTLOWD empty)
(define 1LOWD (cons WORKDAY-1 MTLOWD))
(define 2LOWD (cons WORKDAY-2 1LOWD))
(define 3LOWD (cons WORKDAY-3 2LOWD))
(define 4LOWD (cons WORKDAY-4 3LOWD))
(define 5LOWD (cons WORKDAY-5 4LOWD))
(define 6LOWD (cons WORKDAY-6 5LOWD))
(define 7LOWD (cons WORKDAY-7 6LOWD))

;; Deconstructor Template
;; lowd-fn : LoWD -> ???
#; (define (lowd-fn lowd)
     (cond
       [(empty? lowd) ...]
       [(cons? lowd) ... (work-day-fn (first lowd)) ...
                     ... (lowd-fn (rest lowd)) ...]))

;; A ListOfFreeHours (LoFH) is one of
;; - empty
;; - (cons free-hours LoFH) ;;> -0.5 free-hours is not a defined data type
;; INTERP: represents a list of free hours

;;;; Signature
;; work-week-free-hours : LoWD -> LoFH

;;;; Purpose
;; GIVEN: a list of work days
;; RETURN: a list of free hours for each work days of a work week
;;> Grammar: each work *day
;;;; Examples
;; (work-week-free-hours MTLOWD) => empty
;; (work-week-free-hours 1LOWD) => (cons 0 empty)
;; (work-week-free-hours 2LOWD) => (cons -3 (cons 0 empty)) 
;; (work-week-free-hours 3LOWD) => (cons 2 (cons -3 (cons 0 empty)
;; (work-week-free-hours 4LOWD) => (cons 3 (cons 2 (cons -3 (cons 0 empty))))
;; (work-week-free-hours 5LOWD)
;; => (cons 2 (cons 3 (cons 2 (cons -3 (cons 0 empty)))))
;; (work-week-free-hours 6LOWD)
;; => (cons 5 (cons 2 (cons 3 (cons 2 (cons -3 (cons 0 empty))))))
;; (work-week-free-hours 7LOWD)
;; => (cons 13 (cons 5 (cons 2 (cons 3 (cons 2 (cons -3 (cons 0 empty)))))))

;;;; Function Definition
(define (work-week-free-hours lowd)
  (cond
    [(empty? lowd) empty]
    [(cons? lowd) (cons (free-hours (first lowd))
                        (work-week-free-hours (rest lowd)))]))

;;;; Tests
(check-expect (work-week-free-hours MTLOWD)
              empty)
(check-expect (work-week-free-hours 1LOWD)
              (cons 0 empty))
(check-expect (work-week-free-hours 2LOWD)
              (cons -3 (cons 0 empty)) )
(check-expect (work-week-free-hours 3LOWD)
              (cons 2 (cons -3 (cons 0 empty))))
(check-expect (work-week-free-hours 4LOWD)
              (cons 3 (cons 2 (cons -3 (cons 0 empty)))))
(check-expect (work-week-free-hours 5LOWD)
              (cons 2 (cons 3 (cons 2 (cons -3 (cons 0 empty))))))
(check-expect (work-week-free-hours 6LOWD)
              (cons 5 (cons 2 (cons 3 (cons 2 (cons -3 (cons 0 empty)))))))
(check-expect (work-week-free-hours 7LOWD)
              (cons 13
                    (cons 5
                          (cons 2
                                (cons 3
                                      (cons 2
                                            (cons -3
                                                  (cons 0 empty))))))))