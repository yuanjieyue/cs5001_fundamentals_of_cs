;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assignment-6-yuanjieyue) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;> TOTAL: 100/111

;;> GAME BEHAVIOR:
;;> I like your losing message :)

;;> CODE STYLE/DESIGN (See comments for details)
;;> -4 Design recipe: Incomplete signatures, data defs not capitalized, using an
;;>    undefined data type
;;> -3 Code style: magic numbers, bad fn names, unnecessary code
;;> -3 Purpose: confusing or vague purpose statements


(require 2htdp/image)
(require 2htdp/universe)

;;;; Assignment 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;- Data Definition -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define LEFT 'left)
(define RIGHT 'right)

(define WIDTH 870)
(define HEIGHT 700)
(define BG (empty-scene WIDTH HEIGHT))

(define SPACESHIP-HEIGHT 15)
(define SPACESHIP-WIDTH 40)
(define SPACESHIP-IMAGE (rectangle SPACESHIP-WIDTH
                                   SPACESHIP-HEIGHT
                                   "solid"
                                   "black"))
(define SPACESHIP-SPEED 10)


(define MAX-REMAINING-LIVES 2)
(define SHIP-LIVES-POSN-X 60)
(define SHIP-LIVES-POSN-Y 120)

(define SPACESHIP-BULLET-RADIUS 3)
(define SPACESHIP-BULLET-IMAGE (circle SPACESHIP-BULLET-RADIUS
                                       "solid"
                                       "black"))
(define SPACESHIP-BULLET-SPEED 10)
(define MAX-SPACESHIP-BULLETS 3)



(define INVADER-WIDTH 30)
(define INVADER-HEIGHT 30)
(define INVADER-IMAGE (rectangle INVADER-WIDTH
                                 INVADER-HEIGHT
                                 "solid"
                                 "red"))
(define INVADERS-ROW 4)
(define INVADERS-COLUMN 10)
(define INVADERS-ROW-GAP (* (/ 1 2) INVADER-HEIGHT))
(define INVADERS-COLUMN-GAP INVADER-WIDTH)
(define INVADERS-MOVE-TICKS 60)
(define INVADERS-MOVE-UNIT 15)

(define LEFT-MARGIN (/ (- WIDTH
                          (- (* INVADERS-COLUMN
                                (+ INVADER-WIDTH INVADERS-COLUMN-GAP))
                             INVADERS-COLUMN-GAP))
                          2))                         
(define RIGHT-MARGIN (- WIDTH LEFT-MARGIN))
(define UP-MARGIN 30)
(define DOWN-MARGIN 30)

(define INVADER-BULLET-RADIUS 3)
(define INVADER-BULLET-IMAGE (circle INVADER-BULLET-RADIUS
                                     "solid"
                                     "red"))
(define INVADER-BULLET-SPEED 10)
(define MAX-INVADER-BULLETS 10)

(define INVADER-INIT (make-posn (+ LEFT-MARGIN (* (/ 1 2) INVADER-WIDTH))
                                (+ UP-MARGIN (* (/ 1 2) INVADER-HEIGHT))))


(define SCORE-UNIT 3)
(define SCORE-POSN-X 60)
(define SCORE-POSN-Y 200)

;; A Direction is one of
;; - LEFT
;; - RIGHT
;; INTERP: represents the spaceship's direction

;; Deconstructor Template
;; direction-fn : Direction -> ???
#; (define (direction-fn direction)
     (cond
       [(symbol=? direction 'left) ...]
       [(symbol=? direction 'right) ...]))
;;> you could have used your LEFT and RIGHT constants in the deconstructor
;;> instead of using the symbol

;; A Location is a posn ;;> Posn* capitalize your data defs
;; INTERP: represents the location of the spaceship

;; Deconstructor Template
;; Location-fn : location -> ???
#; (define (location-fn location)
     ... (posn-x location) ...
     ... (posn-y location) ...)

;; A SpaceShip is a (make-spaceship Direction location NonNegInteger)
;;> design recipe: *Location, capitalize your data defs
;; INTERP: represents the a spaceship with direction and location
;;> design recipe: INTERP statement not complete, what about the lives?
(define-struct spaceship (direction location lives))

;; Deconstructor Template
;; space-ship-fn : SpaceShip -> ???
#; (define (spaceship-fn ss)
     ... (direciton-fn (spaceship-direction ss)) ... ;;> spelling *direction
     ... (location-fn (spaceship-location ss)) ...
     ... (spaceship-lives ss) ...)


;; A SpaceShipBullet is a posn ;;> design recipe: *Posn
;; INTERP: represents the spaceship bullet

;; Deconstructor Template
;; spaceship-bullet-fn : SpaceShipBullet -> ???
#; (define (spaceship-bullet-fn ssb)
     ... (posn-x ssb) ...
     ... (posn-y ssb) ...)

;; A ListOfSpaceShipBullet (LoSSB) is one of
;; - empty
;; - (cons (SpaceShipBullet LoSSB) ;;> *extra parenthesis
;; INTERP: represents a list of spaceship bullets

;; Deconstructor Template
;; lossb-fn : LoSSB -> ???
#; (define (lossb-fn lossb)
     (cond
       [(empty? lossb) ...]
       [(cons? lossb) ...(spaceship-bullet-fn (first lossb))...
                      ...(lossb-fn (rest lossb))...]))


;; An InvaderBullet is a posn ;;> design recipe: *Posn
;; INTERP: represents the invaders' bullet

;; Deconstructor Template
;; invader-bullet-fn : InvaderBullet -> ???
#; (define (invader-bullet-fn ib)
     ... (posn-x ib) ...
     ... (posn-y ib) ...)

;; A ListOfInvaderBullet (LoIB) is one of
;; - empty
;; - (cons InvaderBullet LoIB)
;; INTERP: represents a list of invader bullets

;; Deconstructor Template
;; loib-fn : LoIB -> ???
#; (define (loib-fn loib)
     (cond
       [(empty? loib) ...]
       [(cons? loib) ...(invader-bullet-fn (first loib))...
                     ...(loib-fn (rest loib))...]))


;; An Invader is a posn ;;> design recipe: *Posn
;; INTERP: represents one of the invaders

;; Deconstructor Template
;; invader-fn : Invader -> ???
#; (define (invader-fn invader)
     ... (posn-x invader)...
     ... (posn-y invader)...)


;; A ListOfInvaders (LoI) is one of
;; - empty
;; - (cons Invader LoI)
;; INTERP: represents a list of invaders

;; Deconstructor Template
;; loi-fn : LoI -> ???
#; (define (loi-fn loi)
     (cond
       [(empty? loi) ...]
       [(cons? loi) ...(invader-fn (first loi)) ...
                    ...(loi-fn (rest loi)) ...]))

;;;; Signature
;;> code style: bad fn name. Based on the name of your fn, I would assume that
;;> you are addind an invader to a given column. But you are actullay generating
;;> invaders for a given row. 
;; add-invader-in-column : Invader NonNegInteger -> LoI
;;;; Purpose
;; GIVEN: the initial invader and the column numbers of invaders
;; RETURN: the first row of invaders that is generated on the basis of
;;         the initial one and the colunmn numbers ;;> spelling *column
;;> purpose: a little confusing. the "column numbers of invaders" is not clear.
;;> Say "the number of columns". The return statement could also be clearer. You
;;> are creating a row of invaders where the number of invaders in the row is
;;> equal to the number of columns. Be specific and be clear.

;;;; Examples
;; (add-invader-in-column INVADER-INIT 2)
;; => (list (make-posn 165 45) (make-posn 225 45))

;;;; Function Definition
(define (add-invader-in-column invader column-num)
  (cond
    [(= column-num 0) empty]
    [else (cons invader
                (add-invader-in-column (make-posn (+ (posn-x invader)
                                                     (+ INVADER-WIDTH
                                                        INVADERS-COLUMN-GAP))
                                                  (posn-y invader))
                                       (sub1 column-num)))]))

(check-expect (add-invader-in-column INVADER-INIT 2)
              (list (make-posn 165 45) (make-posn 225 45)))

;;;; Signature
;; add-invader-in-row : LoI NonNegInteger -> LoI
;;;; Purpose
;; GIVEN: the first row of invaders and the row numbers of the invaders
;; RETURN: the list of invaders including the row numbers of invaders

;;;; Examples
;; (add-invader-in-row (list (make-posn 165 45) (make-posn 225 45))
;; => (list (make-posn 165 45) (make-posn 225 45) (make-posn 165 90)
;;          (make-posn 225 90))

;;;; Function Definition
(define (add-invader-in-row invader row-num column-num)
  (cond
    [(= row-num 0) empty]
    [else (append (add-invader-in-column invader column-num)
                  (add-invader-in-row
                   (make-posn (posn-x invader)
                              (+ (posn-y invader)
                                 (+ INVADER-HEIGHT
                                    INVADERS-ROW-GAP)))
                   (sub1 row-num)
                   column-num))]))

(check-expect (add-invader-in-row INVADER-INIT 2 2)
                                  (list (make-posn 165 45)
                                        (make-posn 225 45)
                                        (make-posn 165 90)
                                        (make-posn 225 90)))


(define-struct invaders (loi ticks))
;; A Invaders is (make-invaders LoI NonNegInteger)
;; INTERP: represents the invaders in the game with their positions and clock
;;         ticks
;;> design recipe: is this the best way to design your data? Why did you put
;;> ticks inside invaders? Is ticks specific to the invaders data type somehow?
;; Deconstructor Template
;; invaders-fn: Invaders -> ???
#; (define (invaders-fn invaders)
     ... (loi-fn (invaders-loi invaders)) ...
     ... (invaders-ticks invaders) ...)

(define-struct world (spaceship invaders lossb loib score))
;; A World is (make-world SpaceShip Invaders LoSSB LoIB NonNegInteger)
;; INTERP: spaceship represents the spaceship in the game
;;         invaders represents the invaders in the game
;;         lossb represents the list of spaceship bullets in the game
;;         loib represents the list of invader bullets in the game
;;         score represents the play's current score in the game

;; Deconstructor Template
;; world-fn : World -> ???
#; (define (world-fn world)
     ... (spaceship-fn (world-spaceship world)) ...
     ... (invaders-fn (world-invaders world)) ...
     ... (lossb-fn (world-lossb world)) ...
     ... (loib-fn (world-loib world)) ...
     ... (world-score world) ...)

(define SPACESHIP-INIT
  (make-spaceship LEFT
                  (make-posn (* (/ 1 2)
                                WIDTH)
                             (- HEIGHT
                                (+ DOWN-MARGIN
                                   (* (/ 1 2)
                                      SPACESHIP-HEIGHT))))
                  MAX-REMAINING-LIVES))

(define LOI-INIT (add-invader-in-row INVADER-INIT
                                     INVADERS-ROW
                                     INVADERS-COLUMN))
(define INVADERS-INIT (make-invaders LOI-INIT
                                     INVADERS-MOVE-TICKS))
(define LOSSB-INIT empty)
(define LOIB-INIT empty)
(define SCORE-INIT 0)
(define WORLD-INIT (make-world SPACESHIP-INIT
                               INVADERS-INIT
                               LOSSB-INIT
                               LOIB-INIT
                               SCORE-INIT))

(define WORLD-TEST-DRAW (make-world SPACESHIP-INIT
                                    INVADERS-INIT
                                    (cons (make-posn 240 350)
                                          empty)
                                    (cons (make-posn 534 235)
                                          (cons (make-posn 340 600)
                                                empty))
                                    SCORE-INIT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- Draw World -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Signature
;; draw-world: World -> Image
;; Purpose
;; GIVEN: a world
;; RETURN: an image representation of the given world
;;   1. the spaceship image
;;   2. the ship lives image
;;   3. the list of invaders images
;;   4. the list of spaceship bullets images
;;   5. the list of invaders bullets images
;;   6. the play's score image

;;;; Function Definition
(define (draw-world world)
  (draw-spaceship (world-spaceship world)
                  (draw-invaders (world-invaders world)
                            (draw-lossb (world-lossb world)
                                        (draw-loib (world-loib world)
                                                   (draw-score
                                                    (world-score world)
                                                    BG))))))
;;;; Tests
(check-expect (draw-world WORLD-INIT)
              (draw-spaceship SPACESHIP-INIT
                              (draw-invaders INVADERS-INIT
                                        (draw-lossb LOSSB-INIT
                                                    (draw-loib LOIB-INIT
                                                               (draw-score
                                                                SCORE-INIT
                                                                BG))))))

(check-expect (draw-world WORLD-TEST-DRAW)
              (draw-spaceship SPACESHIP-INIT
                              (draw-invaders INVADERS-INIT
                                        (draw-lossb
                                         (cons (make-posn 240 350)
                                               empty)
                                         (draw-loib
                                          (cons (make-posn 534 235)
                                                (cons (make-posn 340 600)
                                                      empty))
                                          (draw-score
                                           SCORE-INIT
                                           BG))))))

;;;; Signature
;; draw-score : Score Image -> Image
;;;; Purpose
;; GIVEN: a play's score for the game and an image
;; RETURNS: the image of the score drawn on the given image
;;;; Function Definition
(define (draw-score score img)
  (place-image (text (string-append "Score: "
                                    (number->string score))
                     24
                     "green")
               SCORE-POSN-X
               SCORE-POSN-Y
               img))
;;> code style: magic numbers and strings


;;;; Siagnature
;; draw-loib: LoIB Image -> Image
;;;; Purpose
;; GIVEN: a list of invader bullets and an image
;; RETURN: the image of the list of bullets drawn on the given image
;;;; Function Definition
(define (draw-loib loib img)
  (cond
    [(empty? loib) img]
    [(cons? loib) (draw-invader-bullet (first loib)
                                       (draw-loib (rest loib) img))]))

;;;; Signature
;; draw-invader-bullet : Invader Image -> Image
;;;; Purpose
;; GIVEN: an invader bullet and an image
;; RETURN: the image of the bullet drawn on the given image
;;;; Function Definition
(define (draw-invader-bullet invader-bullet img)
  (place-image INVADER-BULLET-IMAGE
               (posn-x invader-bullet) 
               (posn-y invader-bullet)
               img))
  
;;;; Signature
;; draw-lossb: LoSSB Image -> Image
;;;; Purpose
;; GIVEN: a list of spaceship bullet and an image ;;> grammar bullets*
;; RETURN: the image of the spaceship bullet drawn on the given image
;;;; Function Definition
(define (draw-lossb lossb img)
  (cond
    [(empty? lossb) img]
    [(cons? lossb) (draw-spaceship-bullet (first lossb)
                                          (draw-lossb (rest lossb) img))]))
  
;;;; Signature
;; draw-spaceship-bullet: SpaceShipBullet Image -> Image
;;;; Purpose
;; GIVEN: a spaceship bullet and an image
;; RETURN: the image of the spaceship bullet drawn on the given image
;;;; Function Definition
(define (draw-spaceship-bullet spaceship-bullet img)
  (place-image SPACESHIP-BULLET-IMAGE
               (posn-x spaceship-bullet)
               (posn-y spaceship-bullet)
               img))
;;;; Signature
;; draw-invaders: Invaders Image -> Image
;;;; Purpose
;; GIVEN: the invaders and an image
;; RETURNS: the image with the invaders drawn on the given image
;;;; Function Definition
(define (draw-invaders invaders img)
  (draw-loi (invaders-loi invaders)
            img))

;;;; Signature
;; draw-loi: LoI Image -> Image
;;;; Purpose
;; GIVEN: a list of invaders and an image
;; RETURN: the image of the list of invaders drawn on the given image
;;;; Function Definition
(define (draw-loi loi img)
  (cond
    [(empty? loi) img]
    [(cons? loi) (draw-invader (first loi)
                               (draw-loi (rest loi) img))]))

;;;; Signature
;; draw-invader: Invader Image -> Image
;;;; Purpose
;; GIVEN: an invader and an image
;; RETURN: the image of the invader drawn on the given image
;;;; Function Definition
(define (draw-invader invader img)
  (place-image INVADER-IMAGE
               (posn-x invader)
               (posn-y invader)
               img))


;;;; Signature
;; draw-ship-lives: ShipLives Image -> Image
;;;; Purpose
;; GIVEN: a ship lives and an image
;; RETURNS: the image of the ship lives drawn on the given image
;;;; Function Definition
(define (draw-ship-lives lives img)
  (place-image (text (string-append "Lives: "
                                    (number->string lives))
                     24
                     "blue")
               SHIP-LIVES-POSN-X
               SHIP-LIVES-POSN-Y
               img))
;;> code style: magic numbers and strings

;;;; Signature
;; draw-spaceship: SpaceShip Image -> Image
;;;; Purpose
;; GIVEN: a spaceship and an image
;; RETURN: the image of the spaceship drawn on the given image
;;;; Function Definition
(define (draw-spaceship spaceship img)
  (place-image SPACESHIP-IMAGE
               (posn-x (spaceship-location spaceship))
               (posn-y (spaceship-location spaceship))
               (draw-ship-lives (spaceship-lives spaceship)
                                img)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- World Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Signature
;; world-step: World -> World
;;;; Purpose
;; GIVEN: the current world
;; RETURNs: the next world after one clock tick
;;   1. If the spaceship is hit by an invaders' bullet and the ship live is
;;      more than 1, new spaceship will be generated from the initial position
;;      and the ship lives will be updated decreased by 1.
;;   2. If any of the spaceship bullets that are out of bounds
;;      or hit an invader, they will be removed from the canvas.
;;   3. If any of the invaders that are hit by the spaceship bullets,
;;      they will be removed from the canvas and the score will be updated
;;      increased by the corresponding multiple of 3.
;;   4. If any of the invaders bullets that are out of bounds,
;;      they will be removed from the canvas.
;;   5. new invader bullets could be generated if there are enough invaders
;;      and the bullets have not reached the maximum amount.
;;   6. new spaceship bullets could be generated the play press the space bar
;;      key.
;;   7. If the clock tick get to the multiple of 10, the invaders will move
;;      down a certain distance.
;;   8. All the other, including the spaceship, invaders that are not hit,
;;      spaceship bullets that not hit an invader or out of bounds, and
;;      invaders bullets that not hit the spaceship or out of bounds,
;;      will all be kept in the next world.
;;> purpose: while this is descriptive, it is also long. You can keep the
;;> details of each individual step fn in their purpose statements and provide a
;;> general description here. For example: RETURNS the next world after:
;;> (1) Updating the spaceship's direction, location, and number of lives if the
;;>     ship was hit
;;> (2) Removes bullets that are out of bounds or that have been involved in a
;;>     collision
;;> etc ... 

;; Function Definition
(define (world-step world)
  (make-world (spaceship-step world)
              (invaders-step world)
              (lossb-step world)
              (loib-step world)
              (score-step world)))


;;;;;;;;;;;;;;;;;;;;;;;;;- SpaceShip Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;;; Signature
;; spaceship-step: World -> SpaceShip
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next spaceship after one clock tick
;;> purpose: this is where you should be putting the specific details about
;;> spaceship-step, not in world-step
;;;; Function Definition
(define (spaceship-step world)
  (if (spaceship-hit (world-spaceship world)
                     (world-loib world))
      (make-spaceship (spaceship-direction SPACESHIP-INIT)
                      (spaceship-location SPACESHIP-INIT)
                      (- (spaceship-lives (world-spaceship world))
                         1))
      (move-spaceship (world-spaceship world))))
       

;;;; Tests
(define SPACESHIP-TEST-STEP
  (make-spaceship LEFT
                  (make-posn 435 662.5)
                  1))
(define LOIB-TEST-STEP (cons (make-posn 250 360)
                             (cons (make-posn 435 662.5)
                                   empty)))
(define WORLD-TEST-STEP
  (make-world SPACESHIP-TEST-STEP
              INVADERS-INIT
              LOSSB-INIT
              LOIB-TEST-STEP
              SCORE-INIT))

(check-expect (spaceship-step WORLD-INIT)
              (make-spaceship LEFT
                              (make-posn 425 662.5)
                              MAX-REMAINING-LIVES))
(check-expect (spaceship-step WORLD-TEST-STEP)
              (make-spaceship LEFT
                              (make-posn 435 662.5)
                              0))
              

;;;; Signature
;; move-spaceship: SpaceShip -> SpaceShip
;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: the spaceship after it moves one speed unit to its direction
;;          or the origin spaceship if it reaches the corner.
;;> the *given spaceship if it reaches the corner. "origin" sounds like the
;;> initial spaceship

;;;; Function Definition
(define (move-spaceship spaceship)
  (cond
    [(symbol=? (spaceship-direction spaceship) LEFT)
     (spaceship-move-to-left spaceship)]
    [(symbol=? (spaceship-direction spaceship) RIGHT)
     (spaceship-move-to-right spaceship)]))

;;;; Signatrue
;; spaceship-move-to-left: SpaceShip -> SpaceShip
;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: the spaceship that moves towards left for a speed unit
;;;; Function Definition
(define (spaceship-move-to-left spaceship)
  (if (> (posn-x (spaceship-location spaceship))
         (+ (* (/ 1 2) SPACESHIP-WIDTH)
            LEFT-MARGIN))
      (make-spaceship (spaceship-direction spaceship)
                      (make-posn (- (posn-x (spaceship-location spaceship))
                                    SPACESHIP-SPEED)
                                 (posn-y (spaceship-location spaceship)))
                      (spaceship-lives spaceship))
      spaceship))

;;;; Signatrue
;; spaceship-move-to-right: SpaceShip -> SpaceShip
;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: the spaceship that moves towards right for a speed unit
;;;; Function Definition
(define (spaceship-move-to-right spaceship)
  (if (< (posn-x (spaceship-location spaceship))
         (- RIGHT-MARGIN (* (/ 1 2) SPACESHIP-WIDTH)))
      (make-spaceship (spaceship-direction spaceship)
                      (make-posn (+ (posn-x (spaceship-location spaceship))
                                    SPACESHIP-SPEED)
                                 (posn-y (spaceship-location spaceship)))
                      (spaceship-lives spaceship))
      spaceship))

;;;;Tests
(define SPACESHIP-TEST-1
  (make-spaceship LEFT
                  (make-posn 185 662.5)
                  MAX-REMAINING-LIVES))
(define SPACESHIP-TEST-2
  (make-spaceship LEFT
                  (make-posn 150 662.5)
                  MAX-REMAINING-LIVES))
(define SPACESHIP-TEST-3
  (make-spaceship RIGHT
                  (make-posn 720 662.5)
                  1))

(check-expect (move-spaceship SPACESHIP-INIT)
              (make-spaceship LEFT
                              (make-posn 425 662.5)
                              MAX-REMAINING-LIVES))
(check-expect (move-spaceship SPACESHIP-TEST-1)
              (make-spaceship LEFT
                              (make-posn 175 662.5)
                              MAX-REMAINING-LIVES))
(check-expect (move-spaceship SPACESHIP-TEST-2)
              (make-spaceship LEFT
                              (make-posn 150 662.5)
                              MAX-REMAINING-LIVES))
(check-expect (move-spaceship (make-spaceship RIGHT
                                              (make-posn 345 662.5)
                                              1))
              (make-spaceship RIGHT
                              (make-posn 355 662.5)
                              1))
(check-expect (move-spaceship SPACESHIP-TEST-3)
              (make-spaceship RIGHT
                              (make-posn 720 662.5)
                              1))

;;;; Signature
;; spaceship-hit: SpaceShip LoIB -> Boolean
;;;; Purpose
;; GIVEN: a spaceship and a list of invader bullets
;; RETURNS: true if the spaceship is hit by any of the bullets in the list
;;          false otherwise
;;;; Function Definition
(define (spaceship-hit spaceship loib)
  (cond
    [(empty? loib) false]
    [(cons? loib) (or (bullet-hit-spaceship spaceship (first loib))
                      (spaceship-hit spaceship (rest loib)))]))

;;;; Signature
;; bullet-hit-spaceship : SpaceShip InvaderBullet -> Boolean
;;;; Purpose
;; GIVEN: a spaceship and an invader bullet
;; RETURNS: true if the invader bullet hit the spaceship
;;          false otherwise

;;;; Function Definition
(define (bullet-hit-spaceship spaceship invader-bullet)
  (and (between? (posn-x invader-bullet)
                 (- (posn-x (spaceship-location spaceship))
                    (* (/ 1 2)
                       SPACESHIP-WIDTH))
                 (+ (posn-x (spaceship-location spaceship))
                    (* (/ 1 2)
                       SPACESHIP-WIDTH)))
       (between? (posn-y invader-bullet)
                 (- (posn-y (spaceship-location spaceship))
                    (* (/ 1 2)
                       SPACESHIP-HEIGHT))
                 (+ (posn-y (spaceship-location spaceship))
                    (* (/ 1 2)
                       SPACESHIP-HEIGHT)))))


;;;;;;;;;;;;;;;;;;;;;;;;;- Invaders Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                                   

;;;; Signature
;; invaders-step : World -> Invaders
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next invaders after one clock tick
;;> purpose: same as comment for spaceship-step
;;;; Function Definition
(define (invaders-step world)
  (move-invaders (remove-hits-invaders world)))
  

(define LOSSB-TEST-1 (cons (make-posn 162 190)
                           (cons (make-posn 223 300)
                                 (cons (make-posn 270 500)
                                       empty))))

(define WORLD-TEST-1 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 LOSSB-TEST-1
                                 LOIB-INIT
                                 SCORE-INIT))
(define INVADERS-TEST-1 (make-invaders (remove (make-posn 165 180) LOI-INIT)
                                       (- (invaders-ticks INVADERS-INIT)
                                          1)))

(define INVADERS-TEST-2 (make-invaders LOI-INIT
                                       0))
(define WORLD-TEST-2 (make-world SPACESHIP-INIT
                                 INVADERS-TEST-2
                                 LOSSB-TEST-1
                                 LOIB-INIT
                                 SCORE-INIT))

(define LOI-INIT-MOVE (add-invader-in-row (make-posn 165 60)
                                          INVADERS-ROW
                                          INVADERS-COLUMN))
;;;; Tests
(check-expect (invaders-step WORLD-TEST-1)
              INVADERS-TEST-1)

(check-expect (invaders-step WORLD-TEST-2)
                             (make-invaders (remove (make-posn 165 195)
                                                    LOI-INIT-MOVE)
                                            INVADERS-MOVE-TICKS))

;;;; Signature
;; move-invaders: Invaders -> Invaders
;;;; Purpose
;; GIVEN: the invaders
;; RETURNS: the invaders that move downwards for some distance
;;          at a certain clock ticks.
;;> purpose: be specific. It is not just some distance. It is a fixed distance
;;> that is equal to half the length of an invader.
;;;; Function Definition
(define (move-invaders invaders)
  (cond
    [(= 0 (invaders-ticks invaders))
     (make-invaders (move-loi (invaders-loi invaders))
                    INVADERS-MOVE-TICKS)]
    [else
     (make-invaders (invaders-loi invaders)
                    (- (invaders-ticks invaders)
                        1))]))
     
;;;; Signature
;; move-loi: LoI -> LoI
;;;; Purpose
;; GIVEN: a list of invaders
;; RETURNS: the list move downward for a some distance at every 10 clock ticks
;;> purpose: when this fn is called, all the invaders are moved down. It has
;;> nothing to do with clock ticks.
;;;; Function Definition
(define (move-loi loi)
  (cond
    [(empty? loi) empty]
    [(cons? loi) (cons (move-invader (first loi))
                       (move-loi (rest loi)))]))

;;;; Signature
;; move-invader: Invader -> Invader
;;;; Purpose
;; GIVEN: an invader
;; RETURNS: the invader after it moves downward for some distance every 10
;;          clock ticks.
;;> purpose: same as above. Your purpose statement should describe this fn, not
;;> the fn that calls this fn. This function moves the given Invader down at a
;;> fixed speed. 
;;;; Function Definition
(define (move-invader invader)
  (make-posn (posn-x invader)
             (+ (posn-y invader)
                INVADERS-MOVE-UNIT)))

;;;; Signature
;; remove-hits-invaders: World -> Invaders
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the invaders with those that are hit by spaceship bullets removed
;;;; Function Definition
(define (remove-hits-invaders world)
  (make-invaders (remove-hits-invader (invaders-loi (world-invaders world))
                                      (world-lossb world))
                 (invaders-ticks (world-invaders world))))                                      

;;> code style: bad fn names. This fn and the previous one only differ by a
;;> single letter ('s') on the end. That makes me assume that the first one
;;> deals with invaderS and this one deals with a single invader. But thats not
;;> the case. 
;;;; Signature
;; remove-hits-invader: LoI LoSSB -> LoI
;;;; Purpose
;; GIVEN: a list of invaders and a list of spaceship bullets
;; RETURNS: the list of invaders with the invader that is hit by the spaceship
;;          bullets removed.
;;;; Function Definition
(define (remove-hits-invader loi lossb)
  (cond
    [(empty? loi) empty]
    [(cons? loi) (if (invader-hit? (first loi) lossb)
                     (rest loi)
                     (cons (first loi)
                           (remove-hits-invader (rest loi) lossb)))]))

;;;; Tests
(define LOI-TEST-MT empty)
(define LOI-TEST-2 (cons (make-posn 165 180)
                         empty))
(define LOI-TEST-3 (cons (make-posn 225 90)
                         (cons (make-posn 165 180)
                               empty)))

(check-expect (remove-hits-invader LOI-TEST-MT LOSSB-TEST-1)
              empty)
(check-expect (remove-hits-invader LOI-TEST-2 LOSSB-TEST-1)
              empty)
(check-expect (remove-hits-invader LOI-TEST-3 LOSSB-TEST-1)
              (cons (make-posn 225 90) empty))
     
;;;; Signature
;; invader-hit?: Invader LoSSB -> Boolean
;;;; Purpose
;; GIVEN: an invader and a list of spaceship bullets
;; RETURN: true if the invader is hit any of the spaceship bullets in the list
;;         false otherwise
;;> grammar: is hit *by any of the spaceship bullets
;;;; Function Definition
(define (invader-hit? invader lossb)
  (cond
    [(empty? lossb) false]
    [(cons? lossb) (or (bullet-hit-invader invader (first lossb))
                       (invader-hit? invader (rest lossb)))]))

;;;; Test
(check-expect (invader-hit? (make-posn 165 180) LOSSB-INIT)
              false)

;;;; Signature
;; bullet-hit-invader: Invader SpaceShipBullet -> Boolean
;;> code style: fn name should end in '?' since it returns a boolean
;;;; Purpose
;; GIVEN: an invader and a spaceship bullet
;; RETURN: true if the spaceship bullet hit the invader
;;         false otherwise
;;;; Function Definition
(define (bullet-hit-invader invader spaceship-bullet)
  (if (and (between? (posn-x spaceship-bullet)
                     (- (posn-x invader) (* (/ 1 2) INVADER-WIDTH))
                     (+ (posn-x invader) (* (/ 1 2) INVADER-WIDTH)))
           (between? (posn-y spaceship-bullet)
                     (- (posn-y invader) (* (/ 1 2) INVADER-HEIGHT))
                     (+ (posn-y invader) (* (/ 1 2) INVADER-HEIGHT))))
      true
      false))
;;> code style: unecessary code. This is what you are doing:
#;(cond
    [true? true]
    [false? false])
;;> you can remove the if statement entirely from this fn. 

;;;; Signature
;; between? : NonNegInteger NonNegInteger NonNegInteger
;;> design recipe: signature incomplete
;; Purpose
;; GIVEN: a target number and two other numbers
;;> purpose: be specific: a target number, a low bound, and a high bound
;; RETURNS: true if the target number is between those two numbers
;;          false otherwise
;;;; Function Definition
(define (between? num low high)
  (and (>= num low)
       (<= num high)))

(define LOSSB-2 (cons (make-posn 120 300)
                      (cons (make-posn 100 90)
                            (cons (make-posn 158 173)
                                  (cons (make-posn 195 135)
                                        empty)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;- LoSSB Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Siganture
;; lossb-step : World -> LoSSB
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next list of spaceship bullets after one clock tick
;;;; Function Definition
(define (lossb-step world)
  (move-spaceship-bullets
   (remove-hits-spaceship-bullet 
    (remove-out-of-bounds-spaceship-bullets (world-lossb world))
    (invaders-loi (world-invaders world)))))

;;;; Tests
(define LOSSB-TEST-2 (cons (make-posn 223 300)
                           (cons (make-posn 162 190)
                                 (cons (make-posn 270 -1)
                                       empty))))

(define WORLD-TEST-3 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 LOSSB-TEST-2
                                 LOIB-INIT
                                 SCORE-INIT))

(check-expect (lossb-step WORLD-TEST-3)
              (cons (make-posn 223 290)
                    empty))
          
;;;; Signature
;; move-spaceship-bullets: LoSSB -> LoSSB
;;;; Purpose
;; GIVEN: the list of spaceship bullets
;; RETURN: the next list of spaceship bullets after one clock tick
;;> purpose: this tells me nothing about what your fn does. Be specific
;;;; Function Definition
(define (move-spaceship-bullets lossb)
  (cond
    [(empty? lossb) empty]
    [(cons? lossb) (cons (move-spaceship-bullet (first lossb))
                         (move-spaceship-bullets (rest lossb)))]))

;;;; Signature
;; move-spaceship-bullet: SpaceShipBullet -> SpaceShipBullet
;;;; Purpose
;; GIVEN: a spaceship bullet
;; RETURNS: the next list of spaceship bullet after one clock tick
;;> purpose: this tells me nothing about what your fn does. Be specific
;;;; Function Definition
(define (move-spaceship-bullet spaceship-bullet)
  (make-posn (posn-x spaceship-bullet)
             (- (posn-y spaceship-bullet)
                SPACESHIP-BULLET-SPEED)))

;;;; Tests
(define LOSSB-TEST-3 (cons (make-posn 230 450)
                           (cons (make-posn 320 300)
                                 (cons (make-posn 100 600)
                                       empty))))
(check-expect (move-spaceship-bullets LOSSB-INIT)
              empty)
(check-expect (move-spaceship-bullets LOSSB-TEST-3)
              (cons (make-posn 230 440)
                    (cons (make-posn 320 290)
                          (cons (make-posn 100 590)
                                empty))))

;;;; Signature
;; remove-hits-spaceship-bullet: LoSSB LoI -> LoSSB
;;;; Purpose
;; GIVEN: a list of spaceship bullets and a list of invaders
;; RETURNS: the list of spaceship bullets with the bullets that hit the
;;          invaders removed.
;;;; Function Definition
(define (remove-hits-spaceship-bullet lossb loi)
  (cond
    [(empty? lossb) empty]
    [(cons? lossb) (if (invader-hit? (first lossb) loi)
                     (rest lossb)
                     (cons (first lossb)
                           (remove-hits-spaceship-bullet (rest lossb)
                                                         loi)))]))
;;;; Tests
(check-expect (remove-hits-spaceship-bullet LOSSB-INIT LOI-INIT)
              empty)

;;;; Signature
;; remove-out-of-bounds-spaceship-bullets: LoSSB -> LoSSB
;;;; Purpose
;; GIVEN: a list of spaceship bullets
;; RETURNS: the list of spaceship bullets with those bullets that are out of
;;          bounds removed.
;;;; Function Definition
(define (remove-out-of-bounds-spaceship-bullets lossb)
  (cond
    [(empty? lossb) empty]
    [(cons? lossb) (if (spaceship-bullet-out? (first lossb))
                       (rest lossb)
                       (cons (first lossb)
                             (remove-out-of-bounds-spaceship-bullets
                              (rest lossb))))]))

;;;; Tests
(check-expect (remove-out-of-bounds-spaceship-bullets LOSSB-INIT)
              empty)

;;;; Signature
;; spaceship-bullet-out?: SpaceShipBullet
;;> design recipe: incomplete signature
;;;; Purpose
;; GIVEN: a spaceship bullet
;; RETURNS: true if the spaceship bullet is out of bounds of the canvas
;;          false otherwise
;;;; Function Definition
(define (spaceship-bullet-out? spaceship-bullet)
  (or (not (between? (posn-x spaceship-bullet)
                     0
                     WIDTH))
      (not (between? (posn-y spaceship-bullet)
                     UP-MARGIN
                     HEIGHT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;- LoIB Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; loib-step : World -> LoIB
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next list of invader bullets after one clock tick
;;;; Function Definition
(define (loib-step world)
  (invaders-fire (- MAX-INVADER-BULLETS
                    (length (remove-out-of-bounds-invader-bullets
                             (world-loib world))))
                 (invaders-loi (world-invaders world))
                 (move-invader-bullets
                  (remove-out-of-bounds-invader-bullets (world-loib world)))))

;;;; Signature
;; move-invader-bullets: LoIB -> LoIB
;;;; Purpose
;; GIVEN: a list of invader bullets
;; RETURNS: the next list of invader bullets after one clock tick
;;> purpose: this tells me nothing about what your fn does. Be specific
;;;;Function Definition
(define (move-invader-bullets loib)
  (cond
    [(empty? loib) empty]
    [(cons? loib) (cons (move-invader-bullet (first loib))
                        (move-invader-bullets (rest loib)))]))

;;;; Tests
(define LOIB-TEST-1 (cons (make-posn 230 450)
                      (cons (make-posn 320 300)
                            (cons (make-posn 100 600)
                                  empty))))
(check-expect (move-invader-bullets LOIB-TEST-1)
              (cons (make-posn 230 460)
                    (cons (make-posn 320 310)
                          (cons (make-posn 100 610)
                                empty))))
(check-expect (move-invader-bullets LOIB-INIT)
              empty)

;;;; Signature
;; move-invader-bullet: InvaderBullet -> InvaderBullet
;;;; Purpose
;; GIVEN: an invader bullet
;; RETURN: the next invader bullet after one clock tick
;;> purpose: this tells me nothing about what your fn does. Be specific
;;;; Function Definition
(define (move-invader-bullet invader-bullet)
  (make-posn (posn-x invader-bullet)
             (+ (posn-y invader-bullet)
                INVADER-BULLET-SPEED)))

;;;; Signature
;;> design recipe: List is not a defined data type
;; item-nums-of-list : List -> NonNegInteger
;;;; Purpose
;; GIVEN: a list
;; RETURNS: the total numbers of items in the list
;;;; Function Definition
(define (item-nums-of-list lis)
  (cond
    [(empty? lis) 0]
    [(cons? lis) (+ 1
                    (item-nums-of-list (rest lis)))]))

;;;;Tests
(check-expect (item-nums-of-list LOI-TEST-MT)
              0)
(check-expect (item-nums-of-list LOI-INIT)
              40)

;;;; Signature
;; invaders-fire: NonNegInteger LoI LoIB -> LoIB
;; Purpose
;; GIVEN: an amount of bullets that the invaders could shoot and
;;        a list of invaders and a list of invader bullets
;; RETURNS: the update list of invader bullets after any of the invaders in the
;;          list of invaders fire a bullet
;;> grammar: *updated
;;> purpose: your RETURNS statement is not descriptive of your function. After
;;> *any of the invaders fire? So all of them could fire if this fn is called?
;;> Tell me that you are going to randomly select invaders to shoot the number
;;> of bullets specified by the first arg.
;;;; Function Definition
(define (invaders-fire bullet-amount loi loib)
  (cond
    [(or (> (+ bullet-amount (item-nums-of-list loib))
            MAX-INVADER-BULLETS)
         (< bullet-amount
            1))
     loib]
    [else (cons (list-ref loi (random (item-nums-of-list loi)))
                (invaders-fire (sub1 bullet-amount)
                               loi
                               loib))]))
;;> -1 You were not allowed to use list-ref in this assignment. You should have
;;> written your own helper function. 

;;;; Tests
(check-expect (invaders-fire 10 LOI-INIT LOIB-TEST-1)
              LOIB-TEST-1)
(check-expect (invaders-fire 0 LOI-INIT LOIB-TEST-1)
              LOIB-TEST-1)

;;;; Signature
;; remove-out-of-bounds-invader-bullets: LoIB -> LoIB
;;;; Purpose
;; GIVEN: a list of invader bullets
;; RETURNS: the list of invader bullets with those bullets that are out of
;;          bounds removed.
;;;; Function Definition
(define (remove-out-of-bounds-invader-bullets loib)
  (cond
    [(empty? loib) empty]
    [(cons? loib) (if (invader-bullet-out? (first loib))
                       (rest loib)
                       (cons (first loib)
                             (remove-out-of-bounds-invader-bullets
                              (rest loib))))]))

;;;; Tests

(define LOIB-TEST-2 (cons (make-posn 230 450)
                      (cons (make-posn 320 878)
                            (cons (make-posn 100 235)
                                  empty))))
(check-expect (remove-out-of-bounds-invader-bullets LOIB-TEST-2)
              (cons (make-posn 230 450)
                    (cons (make-posn 100 235)
                          empty)))
               
(check-expect (remove-out-of-bounds-invader-bullets LOIB-INIT)
              empty)

;;;; Signature
;; invader-bullet-out?: InvaderBullet
;;> design recipe: incomplete signature
;;;; Purpose
;; GIVEN: an invader bullet
;; RETURNS: true if the invaderbullet is out of bounds of the canvas
;;          false otherwise
;;;; Function Definition
(define (invader-bullet-out? invader-bullet)
  (or (not (between? (posn-x invader-bullet)
                     0
                     WIDTH))
      (not (between? (posn-y invader-bullet)
                     0
                     (- HEIGHT DOWN-MARGIN)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;- Score Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Signature
;; score-step: World -> NonNegInteger
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the player's next score after one clock tick
;;;; Function Defintion
(define (score-step world)
  (+ (world-score world)
     (* (nums-of-hit (invaders-loi (world-invaders world))
                     (world-lossb world))
        SCORE-UNIT)))


;;;; Tests
(check-expect (score-step WORLD-INIT)
              0)
(check-expect (score-step WORLD-TEST-2)
              3)
(check-expect (score-step WORLD-TEST-3)
              3)
                          
       
;;;; Signature
;;> code style: "nums" => "numbers". You are returning the number* of hits*. a
;;> better function name would simple be num-of-hits or num-hits
;; nums-of-hit: LoI LoSSB -> NonNegInteger
;;;; Purpose
;; GIVNEN: the current list of invaders and list of spaceship bullets
;; RETURNS: the numbers of invaders that are hit by the spaceship bullets after
;;          one clock tick.
;;;; Function Definition
(define (nums-of-hit loi lossb)
  (cond
    [(empty? loi) 0]
    [(cons? loi) (if (invader-hit? (first loi) lossb) 
                     (+ 1
                        (nums-of-hit (rest loi) lossb))
                     (nums-of-hit (rest loi) lossb))]))                 

   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- End Game -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
;;;; Signature
;; end-game? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: true if either the player win or lose
;;> grammar: *wins or *loses
;; Function Definition
(define (end-game? world)
  (or (player-win? world)
      (player-lose? world)))

;;;; Signature
;; player-win? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: ture if all the invaders have been hit by the spaceship bullets
;;;; Function Definition
(define (player-win? world)
  (empty? (invaders-loi (world-invaders world))))


;;;; Signature
;; player-lose? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: true if either of the two conditions has been met
;;          1. the lives of spaceship turn to zero.
;;          2. the down most invaders have reached the same y coordinate with
;;             the spaceship.
;;          false otherwise
;;;; Function Definition
(define (player-lose? world)
  (or (and (= 0 (spaceship-lives (world-spaceship world)))
           (spaceship-hit (world-spaceship world)
                          (world-loib world)))
      (>= (+ (invaders-max-y (invaders-loi (world-invaders world)))
             (* (/ 1 2)
                INVADER-HEIGHT))
          (- (posn-y (spaceship-location (world-spaceship world)))
             (* (/ 1 2)
                SPACESHIP-HEIGHT)))))
;;> fn design: this is borderline messy. Would have been cleaner if you wrote
;;> helper fns for each task (1) did I run out of lives? (2) have the invaders
;;> reached the ship?

;;;; Signature
;; invaders-max-y: LoI -> NonNegInteger
;;;; Purpose
;; GIVNE: a list of invaders
;; RETURNS: the max y coordinate of that all the invaders have got
;;> purpose: "of that all the invaders have got" is confusing. You could simply
;;> say "the max y coordinate of all invaders" 
;;;; Function Definition
(define (invaders-max-y loi)
  (cond
    [(empty? (rest loi)) (posn-y (first loi))]
    [(cons? (rest loi)) (if (> (posn-y (first loi))
                               (posn-y (first (rest loi))))
                            (invaders-max-y (cons (first loi)
                                                  (rest (rest loi))))
                            (invaders-max-y (rest loi)))]))
    

;;;; Tests
(define INVADERS-TEST-3 (make-invaders LOI-TEST-MT
                                         5))
(define INVADERS-TEST-4 (make-invaders (cons (make-posn 165 675)
                                                 (cons (make-posn 165 630)
                                                       empty))
                                           10))
(define SPACESHIP-TEST-4
  (make-spaceship LEFT
                  (make-posn (* (/ 1 2)
                                WIDTH)
                             (- HEIGHT
                                (+ DOWN-MARGIN
                                   (* (/ 1 2)
                                      SPACESHIP-HEIGHT))))
                  0))
(define LOIB-TEST-3 (cons (make-posn 418 660)
                          LOIB-TEST-2))
(define WORLD-TEST-4 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 LOSSB-INIT
                                 LOIB-TEST-3
                                 SCORE-INIT))
(define WORLD-TEST-5 (make-world SPACESHIP-INIT
                                 INVADERS-TEST-3
                                 LOSSB-TEST-2
                                 LOIB-TEST-2
                                 30))

(define WORLD-TEST-6 (make-world SPACESHIP-TEST-4
                                 INVADERS-INIT
                                 LOSSB-INIT
                                 LOIB-TEST-3
                                 SCORE-INIT))


(define WORLD-TEST-7 (make-world SPACESHIP-INIT
                                 INVADERS-TEST-4
                                 LOSSB-INIT
                                 LOIB-INIT
                                 SCORE-INIT))

(check-expect (end-game? WORLD-TEST-1)
              false)                
(check-expect (end-game? WORLD-TEST-4)
              false)
(check-expect (end-game? WORLD-TEST-5)
              true)
(check-expect (end-game? WORLD-TEST-6)
              true)
(check-expect (end-game? WORLD-TEST-7)
              true)

;;;; Signature
;; final-scene: World -> Image
;;;; Purpose
;; GIVEN: the final world
;; RETURNS: "Congratulations, You Win!" if the player wins
;;          "So sad, You Lose!" otherwise
;;> purpose: your RETURNS statements suggests that this fn returns strings but
;;> it reutrns an image. 
;;;; Function Definition 
(define (final-scene world)
  (cond
    [(player-win? world)
     (place-image (text "Congratulations, You Win!" 50 "red")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG)]
    [(player-lose? world)
     (place-image (text "So sad, You Lose!" 50 "black")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG)]))
;;> code style: magic strings/number

;;;; Tests
(check-expect (final-scene WORLD-TEST-5)
              (place-image (text "Congratulations, You Win!" 50 "red")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG))
(check-expect (final-scene WORLD-TEST-6)
              (place-image (text "So sad, You Lose!" 50 "black")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;- Key Handler -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Signature
;; key-handler : World Key-Event -> World
;;;; Purpose
;; GIVEN: the current world and a key event
;; RETURNS: a new world with update after the key event
;;> purpose: RETURNS statement is vague. What does "with update" mean? 
;;;; Function Definition
(define (key-handler world ke)
  (cond 
    [(key=? ke "left") (spaceship-direction-to-left world)]
    [(key=? ke "right") (spaceship-direction-to-right world)]
    [(key=? ke " ") (spaceship-fires world)]
    [else world]))
;;> code style: magic strings

;;;; Tests
(check-expect (key-handler WORLD-INIT "left")
              WORLD-INIT)
(check-expect (key-handler WORLD-INIT "up")
              WORLD-INIT)
(check-expect (key-handler WORLD-INIT "right")
              (make-world (make-spaceship RIGHT
                                          (spaceship-location
                                           (world-spaceship WORLD-INIT))
                                          (spaceship-lives
                                           (world-spaceship WORLD-INIT)))
                          (world-invaders WORLD-INIT)
                          (world-lossb WORLD-INIT)
                          (world-loib WORLD-INIT)
                          (world-score WORLD-INIT)))
(check-expect (key-handler WORLD-INIT " ")
              (make-world (world-spaceship WORLD-INIT)
                          (world-invaders WORLD-INIT)
                          (cons (spaceship-location
                                 (world-spaceship WORLD-INIT))
                                (world-lossb WORLD-INIT))
                          (world-loib WORLD-INIT)
                          (world-score WORLD-INIT)))

(define WORLD-TEST-8 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 LOSSB-TEST-3
                                 LOIB-TEST-2
                                 SCORE-INIT))


(check-expect (key-handler WORLD-TEST-8 " ")
              WORLD-TEST-8)

;;;; Signature
;; spaceship-direction-to-left: World -> World
;;;; Purpose
;; GIVEN: a world
;; RETURNS: the world with its spaceship's direction to the left
;;;; Function Definition
(define (spaceship-direction-to-left world)
  (make-world (make-spaceship LEFT
                              (spaceship-location (world-spaceship world))
                              (spaceship-lives (world-spaceship world)))
              (world-invaders world)
              (world-lossb world)
              (world-loib world)
              (world-score world)))


;;;; Signature
;; spaceship-direction-to-right: World -> World
;;;; Purpose
;; GIVEN: a world
;; RETURNS: the world with its spaceship's direction to the right
;;;; Function Definition
(define (spaceship-direction-to-right world)
  (make-world (make-spaceship RIGHT
                              (spaceship-location (world-spaceship world))
                              (spaceship-lives (world-spaceship world)))
              (world-invaders world)
              (world-lossb world)
              (world-loib world)
              (world-score world)))

;;;; Signature
;; spaceship-fires: World -> World
;;;; Purpose
;; GIVEN: a world
;; RETURNS: the world with its spaceship shoot a bullet
;;> purpose: not descriptive enough. Mention that the ship only fires a bullet
;;> if the number of spaceship bullets is below MAX. 
;;;; Function Definition
(define (spaceship-fires world)
  (if (>= (item-nums-of-list (world-lossb world))
          MAX-SPACESHIP-BULLETS)
      world
      (make-world (world-spaceship world)
                  (world-invaders world)
                  (cons (spaceship-location (world-spaceship world))
                        (world-lossb world))
                  (world-loib world)
                  (world-score world))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;- Big Bang -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(big-bang WORLD-INIT
          (on-tick world-step 0.15)
          (to-draw draw-world)
          (on-key key-handler)
          (stop-when end-game? final-scene))
