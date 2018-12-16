;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname assignment-3-yuanjieyue) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")) #f)))
;;> TOTAL: 88.5/92
;;> Good job!
;;> Don't create unnecessary aliases and then not use them. 


;;;; Problem 1

(define-struct student [name id grade])
;; A Student is (make-student String PosInt PosInt)
;;> -1 Since you specify that grade can be 0, it should be NonNegInt, not PosInt
;; WHERE: 0 <= grade <= 100
;; INTER: represents a student with the student's name 
;;        the student's id and the student's grade.

;; Selectors
;; (student-name a-student)
;;;; Signature
;; student-name : Student -> String

;; (student-id a-student)
;;;; Signature
;; student-id : Student -> PosInt

;; (student-grade a-student)
;;;; Signature
;; student-grade : Student -> PosInt

;; Constructor Template
;; (make-student String PosInt PosInt)
;;;; Signature
;; make-student : String PosInt PosInt -> Student

;; Predicate
;; (student? any)
;;;; Signature
;; student? : Any -> Boolean

;; Deconstructor Template
;; student-fn : Student -> ???
#; (define (student-fn a-student)
     ...(student-name a-student)...
     ...(student-id a-student)...
     ...(student-grade a-student)...)



(define-struct container [width height depth capacity label])
;; A Container is (make-container PosInt PosInt PosInt PosInt Symbol)
;; WHERE: capacity = width * height * depth
;; INTERP: represents a container with its width, height, depth 
;;         in centimeters, it's capacity in cubic centimeters and 
;;         it's label

;; Selectors
;; (container-width a-container)
;;;; Signature
;; container-width : Container -> PosInt

;; (container-height a-container)
;;;; Signature
;; container-height : Container -> PosInt

;; (container-depth a-container)
;;;; Signature
;; container-depth : Container -> PosInt

;; (container-capacity a-container)
;;;; Signature
;; container-capacity : Container -> PosInt

;; (container-label a-container)
;;;; Signature
;; container-label : Container -> Symbol

;; Constructor
;; (make-container PosInt PosInt PosInt PosInt Symbol)
;;;; Signature
;; make-container : PosInt PosInt PosInt PosInt -> Symbol

;; Predicate
;; (container? any)
;; container? : Any -> Boolean

;; Deconstructor Template
;; container-fn : Container -> ???
#; (define (container-fn a-container)
     ...(container-width a-container)...
     ...(container-height a-container)...
     ...(container-depth a-container)...
     ...(container-capacity a-container)..
     ...(container-label a-container)...)

                        
(define-struct sweater [material size producer])
;; A Sweater is (make-sweater Symbol PosInt String)
;; INTERP: represents a sweater with the sweater's material 
;;         it's size and the name of the manufacturer

;; Selectors
;; (sweater-material a-sweater)
;;;; Signature
;; sweater-material : Sweater -> Symbol

;; (sweater-size a-sweater)
;;;; Signature
;; sweater-size : Sweater -> PosInt

;; (sweater-producer a-sweater)
;;;;Signature
;; sweater-producer : Sweater-> String

;; Constructor Template
;; (make-sweater Symbol PosInt String)
;;;; Signture
;; make-sweater : Symbol PosInt String -> Sweater

;; Predicate
;; (sweater? any)
;;;; Signature
;; sweater? : Any -> Boolean

;; Decontructor Template
;; sweater-fn : Sweater -> ???
#; (define (sweater-fn a-sweater)
     ...(sweater-material a-sweater)...
     ...(sweater-size a-sweater)...
     ...(sweater-producer a-sweater)...)


(define-struct game [name min-ram min-graphics-ram online?])
;; A Game is (make-game String PosInt PosInt Boolean ;;> *)
;; INTERP: represents a game with it's name, the minimum ram 
;;         capacity needed , the minimum graphics 
;;         card memory needed and whether it is an online game or not


;; Selectors
;; (game-name a-game)
;;;; Signature
;; game-name : Game -> String

;; (game-min-ram a-game)
;;;; Signature
;; game-min-ram : Game -> PosInt

;; (game-min-graphics-ram a-game)
;;;; Signature
;; game-min-graphics : Game -> PosInt
;;> -0.5 fn name should be game-min-graphics-ram

;; (game-online? a-game)
;;;; Signature
;; game-online? : Game -> Boolean

;; Constructor Template
;; (make-game String PosInt PosInt Boolean)
;;;; Signature
;; make-game : String PosInt PosInt Boolean -> Game

;; Predicate
;; (game? any)
;;;; Signature
;; game? : Any -> Boolean

;;;; Deconstructor Template
;; game-fn : Game -> ???
#; (define (game-fn a-game)
     ...(game-name a-game)...
     ...(game-min-ram a-game)...
     ...(game-min-graphics-ram a-game)...
     ...(game-online? a-game)...)
     

;;;; Problem 2

;; Design a program that consumes two Cartesian coordinates as Posn s
;; and returns the distance between the two coordinates.

;;;; Data Analysis and Definition
;; A CartesianCoordinates is a Posn
;;> Multiple coordinates are represented by one Posn?
;; INTERP: represents a Cartesian Coordinates 
;;> -1 Unnecessary alias. How is a CartesianCoordinate different from a Posn? 
;;> Also, you don't even use it so why did you define it?

;; A Distance is NonNegReal
;; INTERP: represents the distance between two Cartesian Coordinatres
;;> Spelling *Coordinates
;;> -1 Unnecessary alias & you don't even use it. 

;;;; Signature
;; distance-between-coordinates : Posn Posn -> NonNegReal

;;;; Purpose
;; GIVEN: two Cartesian Coordinates
;; RETURN: the distance between these two coordinates

;;;; Examples
;; (distance-between-coordinates (make-posn 12.5 30) (make-posn -25 2.5))

;;;; Function Definition
(define (distance-between-coordinates posn-a posn-b)
   (sqrt (+ (sqr (- (posn-x posn-a)
                    (posn-x posn-b)))
            (sqr (- (posn-y posn-a)
                    (posn-y posn-b))))))

;;;; Tests
(check-within (distance-between-coordinates (make-posn 12.5 30)
                                            (make-posn -25 2.5))
              46.5 0.1)
;;> Even though there are no black lines, you should write a couple tests to
;;> cover all cases such as giving 2 identical coordinates



;;;; Problem 3

;;;; Data Anylysis and Definition

;; Dollars is a PosInt
;;> Since dollars can be 0, this should be NonNegInt instead of PosInt
;; Cents is a NonNegInteger 
;; WHERE: Cents is greater or equal to 0 
;;        and less or equal to 99 ;;> Grammar: *less than or equal

(define-struct amount [dollars cents])
;; An Amount is (make-amount Dollars Cents)
;; INTERP: represents total amount in dollars and cents. 

;; Constructor Template
;; (make-amount Dollars Cents)
;; Examples
;; (make-amount 25 70)
;; (make-amount 40 0)

;; Deconstructor Template
;; amount-fn : Amount -> ???
#; (define (amount-fn an-amt)
     ...(amount-dollars an-amt)...
     ...(amount-cents an-amt)...)

;;;; Signature
;; add-to-amount : Amount Dollars Cents -> Amount

;;;; Purpose
;; GIVEN: an amount and a number of dollars to add and a number of cents to add
;; RETURN: the total amount after adding dollars and cents to the amount

;;;; Examples
;;(add-to-amount (make-amount 25 70) 10 20) => (make-amount 35 90)
;;(add-to-amount (make-amount 40 0) 5 24) => (make-amount 45 24)
;;(add-to-amount (make-amount 12 55) 3 80) => (make-amount 16 35)
     
;;;; Function Definition
(define (add-to-amount amt dollars cents)
  (make-amount
   (new-dollars amt dollars cents)
   (new-cents amt cents)))
;;> good use of helper functions. 

;;;; Signature
;; new-dollars : Amount Dollars Cents -> Dollars

;;;; Purpose
;; GIVEN: an amount, a number of dollars to add and a number of cents to add
;; RETURN: the dollars of the new total amount

;;;; Examples
;; (new-dollars (make-amount 25 70) 10 20) => 35
;; (new-dollars (make-amount 40 0) 5 24) => 45
;; (new-dollars (make-amount 12 55) 3 80) => 16

;;;; Function Definition
(define (new-dollars amt dollars cents)
  (cond
    [(>= (+ (amount-cents amt) cents) 100)
     (+ (amount-dollars amt) dollars 1)]
    [(< (+ (amount-cents amt) cents) 100)
     (+ (amount-dollars amt) dollars)]))

;;;; Tests
(check-expect (new-dollars (make-amount 25 70) 10 20)
              35)
(check-expect (new-dollars (make-amount 40 0) 5 24)
              45)
(check-expect (new-dollars (make-amount 12 55) 3 80)
              16)

;;;; Signature
;; new-cents : Amount Cents -> Cents

;;;; Purpose
;;;; GIVEN: an amount and a number of cents
;;;; RETURN: the number of cents of the new amount

;;;; Examples
;; (new-cents (make-amount 25 70) 20) => 90
;; (new-cents (make-amount 40 0)24) => 24
;; (new-cents (make-amount 12 55)80) => 35

;;;; Function Definition
(define (new-cents amt cents)
  (cond
    [(>= (+ (amount-cents amt) cents) 100)
     (- (+ (amount-cents amt) cents) 100)]
    [(< (+ (amount-cents amt) cents) 100)
     (+ (amount-cents amt) cents)]))

;;;; Tests
(check-expect (new-cents (make-amount 25 70) 20)
              90)
(check-expect (new-cents (make-amount 40 0) 24)
              24)
(check-expect (new-cents (make-amount 12 55)80)
              35)


;;> For future assignments, put tests for a function underneath the function
;;> definition. Otherwise, it can be confusing. 
;;;; Tests
(check-expect (add-to-amount (make-amount 25 70) 10 20)
              (make-amount 35 90))
(check-expect (add-to-amount (make-amount 40 0) 5 24)
              (make-amount 45 24))
(check-expect (add-to-amount (make-amount 12 55) 3 80)
              (make-amount 16 35))


;;;; Problem 4

;; A Style is one of 
;; - 'outline
;; - 'solid

(define-struct disk [radius style location])
;; A Disk is (make-disk PosInt Style Posn)
;; WHERE (and (< (+ radius (posn-x location)) 500)
;;            (>= (- (posn-x location) radius) 0)
;;            (< (+ radius (posn-y location)) 500)
;;            (>= (- (posn-y location) radius) 0))
;; INTERP: represents a disk with its radius, its style 
;;         and its location as an (x,y) coordinate

;; Deconstructor Template
;; disk-fn : Disk -> ???
#; (define (disk-fn a-disk)
     ...(disk-radius a-disk)...
     ...(disk-radius a-disk)...
     ...(disk-radius a-disk)...)


;;;; Signature
;; connect-disks : Disk Disk -> Images

;;;; Purpose
;; GIVEN: two disks
;; RETURN: a image with the two disks and a red line from the center of 
;;         one disk to the center of the second disk

;;;; Examples
;;(connect-disks (make-disk 50 'outline (make-posn 100 100))
;;               (make-disk 50 'outline (make-posn 300 400)))
;;;; Function Definition
(define (connect-disks disk1 disk2)
    (and
     (draw-disk disk1)
     (draw-disk disk2)
     (draw-solid-line (disk-location disk1) (disk-location disk2) 'red)))

;;;; Signature
;; draw-disk : Disk -> Image

;;;; Purpose
;; GIVEN: a disk
;; RETURN: the disk with the right style

(define (draw-disk a-disk)
  (cond
    [(symbol=? (disk-style a-disk) 'outline)
     (draw-circle (disk-location a-disk) (disk-radius a-disk) 'black)]
    [(symbol=? (disk-style a-disk) 'solid)
     (draw-solid-disk (disk-location a-disk) (disk-radius a-disk) 'black)]))


(start 500 500)

(connect-disks (make-disk 50 'outline (make-posn 100 100))
               (make-disk 50 'outline (make-posn 300 400)))

(connect-disks (make-disk 80 'solid (make-posn 300 100))
               (make-disk 80 'solid (make-posn 100 350)))






