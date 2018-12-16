;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space-invaders-refactor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;; Assignment 8

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
(define SHIP-LIVES-TEXT "LIVES: ")
(define SHIP-LIVES-COLOR "blue")
(define SHIP-LIVES-FONT-SIZE 24)

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
(define SCORE-TEXT "SCORE: ")
(define SCORE-COLOR "green")
(define SCORE-FONT-SIZE 24)

(define INVADERS-MOVE-TICK 60)
(define FINAL-WIN-SCENE (text "Congratulations, You Win!" 50 "red"))
(define FINAL-LOSE-SCENE (text "So sad, You Lose!" 50 "black"))

(define LEFT-HANDLER "left")
(define RIGHT-HANDLER "right")
(define SHIP-FIRE-HANDLER " ")


;; A Direction is one of
;; - LEFT
;; - RIGHT
;; INTERP: represents the spaceship's direction

;; Deconstructor Template
;; direction-fn : Direction -> ???
#; (define (direction-fn direction)
     (cond
       [(symbol=? direction LEFT) ...]
       [(symbol=? direction RIGHT) ...]))

;; A Location is a Posn
;; INTERP: represents the location of the spaceship

;; Deconstructor Template
;; Location-fn : location -> ???

(define-struct spaceship (direction location lives))
;; A SpaceShip is a (make-spaceship Direction Location NonNegInteger)
;; INTERP: represents a spaceship with its direction, location and lives.

;; Deconstructor Template
;; space-ship-fn : SpaceShip -> ???
#; (define (spaceship-fn ss)
     ... (direction-fn (spaceship-direction ss)) ... 
     ... (location-fn (spaceship-location ss)) ...
     ... (spaceship-lives ss) ...)

;; A List<X> is one of
;; - empty
;; - (cons X List<X>)
;; INTERP: represents a list of X

;; Deconstructor Template
;; lox-fn: List<X> -> ???
#; (define (lox-fn lox)
     (cond
       [(empty? lox) ...]
       [(cons? lox) ...(x-fn (first lox))...
                    ...(lox-fn (rest lox))...]))


;; An Invader is a Posn 
;; INTERP: represents one of the invaders

;; Deconstructor Template
;; invader-fn : Invader -> ???
#; (define (invader-fn invader)
     ... (posn-x invader)...
     ... (posn-y invader)...)

;; An Invaders is List<Invader>
;; INTERP: represents invaders in the game

;; A ShipBullet is a Posn
;; INTERP: represents the spaceship bullet
;; Deconstructor Template
;; ship-bullet-fn : ShipBullet -> ???
#; (define (ship-bullet-fn sb)
     ... (posn-x sb) ...
     ... (posn-y sb) ...)

;; A ShipBullets is List<ShipBullet>
;; INTERP: represents the spaceship bullets in the game


;; An InvaderBullet is a Posn
;; INTERP: represents the invaders' bullet
;; Deconstructor Template
;; invader-bullet-fn : InvaderBullet -> ???
#; (define (invader-bullet-fn ib)
     ... (posn-x ib) ...
     ... (posn-y ib) ...)

;; An InvaderBullets is List<InvaderBullet>
;; INTERP: represents the invader bullets in the game


;;;; Signature
;; populate-invaders : List<invader> NonNegInteger -> List<Invader>
;;;; Purpose
;; GIVEN: the first row of invaders and the row numbers of the invaders
;; RETURNS: the list of invaders including the row numbers of invaders

;;;; Examples
;; (populate-invaders (list (make-posn 165 45) (make-posn 225 45))
;; => (list (make-posn 165 45) (make-posn 225 45) (make-posn 165 90)
;;          (make-posn 225 90))

;;;; Function Definition
(define (populate-invaders invader row-num column-num)
  (cond
    [(= row-num 0) empty]
    [else
     (local (;;;; Signature
            ;; populate-invader-for-row: Invader NonNegInteger -> List<Invader>
            ;;;; Purpose
            ;; GIVEN: the initial invader and the numbers of column of invaders
            ;; RETURN: the first row of invaders that is generated on the basis
            ;;         of the initial one and the column numbers
            ;;;; Function Definition
            (define (populate-invader-for-row invader column-num)
              (cond
                [(= column-num 0) empty]
                [else (cons invader
                            (populate-invader-for-row
                             (make-posn (+ (posn-x invader)
                                           (+ INVADER-WIDTH
                                              INVADERS-COLUMN-GAP))
                                        (posn-y invader))
                             (sub1 column-num)))])))
       (append (populate-invader-for-row invader column-num)
               (populate-invaders
                (make-posn (posn-x invader)
                           (+ (posn-y invader)
                              (+ INVADER-HEIGHT
                                 INVADERS-ROW-GAP)))
                (sub1 row-num)
                column-num)))]))
;;;; Tests
(check-expect (populate-invaders INVADER-INIT 2 2)
              (list (make-posn 165 45)
                    (make-posn 225 45)
                    (make-posn 165 90)
                    (make-posn 225 90)))

(define-struct world
  (spaceship invaders ship-bullets invader-bullets score tick))
;; A World is (make-world SpaceShip
;;                        Invaders
;;                        ShipBullets
;;                        InvaderBullets
;;                        NonNegInteger
;;                        NonNegInteger)

;; WHERE: spaceship represents the spaceship in the game
;;        invaders represents the invaders in the game
;;        ship bullets represents the spaceship bullets in the game
;;        invader bullets represents the invader bullets in the game
;;        score represents the player's current score in the game
;;        tick represents the clock ticks of the game
;; INTERP: represents the world in the game

;; Deconstructor Template
;; world-fn : World -> ???
#; (define (world-fn world)
     ... (spaceship-fn (world-spaceship world)) ...
     ... (invaders-fn (world-invaders world)) ...
     ... (lox-fn (world-ship-bullets world)) ...
     ... (lox-fn (world-invader-bullets world)) ...
     ... (world-score world) ...
     ... (world-tick world) ...)

(define SPACESHIP-INIT
  (make-spaceship LEFT
                  (make-posn (* (/ 1 2) WIDTH)  
                             (- HEIGHT (+ DOWN-MARGIN
                                          (* (/ 1 2)
                                             SPACESHIP-HEIGHT))))
                  MAX-REMAINING-LIVES))

(define INVADERS-INIT (populate-invaders INVADER-INIT
                                         INVADERS-ROW
                                         INVADERS-COLUMN))
(define SHIP-BULLETS-INIT empty)
(define INVADER-BULLETS-INIT empty)
(define SCORE-INIT 0)
(define TICK-INIT 0)
(define WORLD-INIT (make-world SPACESHIP-INIT
                               INVADERS-INIT
                               SHIP-BULLETS-INIT
                               INVADER-BULLETS-INIT
                               SCORE-INIT
                               TICK-INIT))
(define WORLD-TEST-1 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 (cons (make-posn 240 350)
                                       empty)
                                 (cons (make-posn 534 235)
                                       (cons (make-posn 340 600)
                                             empty))
                                 SCORE-INIT
                                 TICK-INIT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- Draw World -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; draw-world: World -> Image
;; Purpose
;; GIVEN: a world
;; RETURN: an image representation of the given world
;;   1. the image of spaceship 
;;   2. the image of invaders
;;   3. the image of ship bullets
;;   4. the image of invaders bullets
;;   5. the image of player's score

;;;; Function Definition
(define (draw-world world)
  (draw-spaceship
   (world-spaceship world)
   (draw-invaders
    (world-invaders world)
    (draw-ship-bullets
     (world-ship-bullets world)
     (draw-invader-bullets
      (world-invader-bullets world)
      (draw-score (world-score world)
                  BG))))))
;;;; Signature
;; draw-score : NonNegInteger Image -> Image
;;;; Purpose
;; GIVEN: a player's score for the game and an image
;; RETURNS: the image of the score drawn on the given image
;;;; Function Definition
(define (draw-score score img)
  (place-image (text (string-append SCORE-TEXT
                                    (number->string score))
                     SCORE-FONT-SIZE
                     SCORE-COLOR)
               SCORE-POSN-X
               SCORE-POSN-Y
               img))
;;;; Siagnature
;; draw-invader-bullets: InvaderBullets Image -> Image
;;;; Purpose
;; GIVEN: a list of invader bullets and an image
;; RETURN: the image of the bullets drawn on the given image
;;;; Function Definition
(define (draw-invader-bullets ibs img)
  (cond
    [(empty? ibs) img]
    [(cons? ibs)
     (local (;;;; Signature
             ;; draw-invader-bullet : Invader Image -> Image
             ;;;; Purpose
             ;;GIVEN: an invader bullet and an image
             ;;RETURN: the image of bullet drawn on the given image
             ;;;; Function Definition
             (define (draw-invader-bullet ib img)
               (place-image INVADER-BULLET-IMAGE
                            (posn-x ib) 
                            (posn-y ib)
                            img)))
       (draw-invader-bullet (first ibs)
                            (draw-invader-bullets (rest ibs)
                                                  img)))]))                  
;;;; Signature
;; draw-ship-bullets: ShipBullets Image -> Image
;;;; Purpose
;; GIVEN: a list of spaceship bullets and an image
;; RETURN: the image of the spaceship bullet drawn on the given image
;;;; Function Definition
(define (draw-ship-bullets sbs img)
  (cond
    [(empty? sbs) img]
    [(cons? sbs)
     (local (;;;; Signature
             ;; draw-spaceship-bullet: SpaceShipBullet Image -> Image
             ;;;; Purpose
             ;; GIVEN: a spaceship bullet and an image
             ;; RETURN: the image of spaceship bullet drawn on the
             ;;         given image
             ;;;; Function Definition
             (define (draw-spaceship-bullet sb img)
               (place-image SPACESHIP-BULLET-IMAGE
                            (posn-x sb)
                            (posn-y sb)
                            img)))
       (draw-spaceship-bullet (first sbs)
                              (draw-ship-bullets (rest sbs)
                                                 img)))]))
;;;; Signature
;; draw-invaders: List<Invader> Image -> Image
;;;; Purpose
;; GIVEN: a list of invaders and an image
;; RETURN: the image of the list of invaders drawn on the given image
;;;; Function Definition
(define (draw-invaders invaders img)
  (cond
    [(empty? invaders) img]
    [(cons? invaders)
     (local (;;;; Signature
             ;; draw-invader: Invader Image -> Image
             ;;;; Purpose
             ;; GIVEN: an invader and an image
             ;; RETURN: the image of the invader drawn on the
             ;;         given image
             ;;;; Function Definition
             (define (draw-invader invader img)
               (place-image INVADER-IMAGE
                            (posn-x invader)
                            (posn-y invader)
                            img)))
       (draw-invader (first invaders)
                     (draw-invaders (rest invaders)
                                    img)))]))
;;;; Signature
;; draw-spaceship: SpaceShip Image -> Image
;;;; Purpose
;; GIVEN: a spaceship and an image
;; RETURN: the image of the spaceship drawn on the given image
;;;; Function Definition
(define (draw-spaceship ship img)
  (local (;;;; Signature
          ;; draw-lives: NonNegInteger Image -> Image
          ;;;; Purpose
          ;; GIVEN: a ship lives and an image
          ;; RETURNS: the image of the ship lives drawn on
          ;;          the given image
          ;;;; Function Definition
          (define (draw-lives lives img)
            (place-image (text (string-append SHIP-LIVES-TEXT
                                              (number->string lives))
                               SHIP-LIVES-FONT-SIZE
                               SHIP-LIVES-COLOR)
                         SHIP-LIVES-POSN-X
                         SHIP-LIVES-POSN-Y
                         img)))
    (place-image SPACESHIP-IMAGE
                 (posn-x (spaceship-location ship))
                 (posn-y (spaceship-location ship))
                 (draw-lives (spaceship-lives ship)
                             img))))  
;;;; Tests
(check-expect (draw-world WORLD-INIT)
              (draw-spaceship
               SPACESHIP-INIT
               (draw-invaders
                INVADERS-INIT
                (draw-ship-bullets
                 SHIP-BULLETS-INIT
                 (draw-invader-bullets
                  INVADER-BULLETS-INIT
                  (draw-score
                   SCORE-INIT
                   BG))))))
(check-expect (draw-world WORLD-TEST-1)
              (draw-spaceship
               SPACESHIP-INIT
               (draw-invaders
                INVADERS-INIT
                (draw-ship-bullets
                 (cons (make-posn 240 350)
                       empty)
                 (draw-invader-bullets
                  (cons (make-posn 534 235)
                        (cons (make-posn 340 600)
                              empty))
                  (draw-score
                   SCORE-INIT
                   BG))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- World Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; world-step: World -> World
;;;; Purpose
;; GIVEN: the current world
;; RETURNs: the next world after：
;;   1. Updates the spaceship's direction, location, and number of lives if 
;;      the ship was hit.
;;   2. Removes bullets that are out of bounds or that have been involved in a
;;      collision.
;;   3. Removes invaders that are out of bounds or hit by a spaceship bullets,
;;      and updating the player's score accordingly.  
;;   4. Generates invader bullets if there are still room for it, and spaceship
;;      bullets according to the key movement.
;;   5. Update world ticks and moves invaders downwards accordingly

;; Function Definition
(define (world-step world)
  (make-world (spaceship-step world)
              (invaders-step world)
              (ship-bullets-step world)
              (invader-bullets-step world)
              (score-step world)
              (tick-step world)))


;;;;;;;;;;;;;;;;;;;;;;;;;- SpaceShip Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;;;; Signature
;; spaceship-step: World -> SpaceShip
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next spaceship after one clock tick:
;;          Generates new spaceship at the initial position if the spaceship is
;;          hit by one invader bullet and updates the ship lives accordingly.          

;;;; Function Definition
(define (spaceship-step world)
  (local (;;;; Signature
          ;; move-ship: SpaceShip -> SpaceShip
          ;;;; Purpose
          ;; GIVEN: a spaceship
          ;; RETURNS: the spaceship that moves one speed unit to its
          ;;          direction or the given spaceship if it reaches the corner
          ;;;; Function Definition
          (define (move-ship ship)
            (cond
              [(symbol=? (spaceship-direction ship) LEFT)
               (local (;;;; Signatrue
                       ;; ship-move-to-left: SpaceShip -> SpaceShip
                       ;;;; Purpose
                       ;; GIVEN: a spaceship
                       ;; RETURNS: the spaceship that moves towards left for a
                       ;;          speed unit
                       ;;;; Function Definition
                       (define (ship-move-to-left ship)
                         (if (> (posn-x (spaceship-location ship))
                                (+ (* (/ 1 2) SPACESHIP-WIDTH)
                                   LEFT-MARGIN))
                             (make-spaceship
                              (spaceship-direction ship)
                              (make-posn (- (posn-x (spaceship-location ship))
                                            SPACESHIP-SPEED)
                                         (posn-y (spaceship-location ship)))
                              (spaceship-lives ship))
                             ship)))
                 (ship-move-to-left ship))]
              [(symbol=? (spaceship-direction ship) RIGHT)
               (local (;;;; Signatrue
                       ;; ship-move-to-right: SpaceShip -> SpaceShip
                       ;;;; Purpose
                       ;; GIVEN: a spaceship
                       ;; RETURNS: the spaceship that moves right for a
                       ;;          speed unit
                       ;;;; Function Definition
                       (define (ship-move-to-right ship)
                         (if (< (posn-x (spaceship-location ship))
                                (- RIGHT-MARGIN (* (/ 1 2) SPACESHIP-WIDTH)))
                             (make-spaceship
                              (spaceship-direction ship)
                              (make-posn (+ (posn-x (spaceship-location ship))
                                            SPACESHIP-SPEED)
                                         (posn-y (spaceship-location ship)))
                              (spaceship-lives ship))
                             ship)))
                 (ship-move-to-right ship))])))           
    (if (ship-hit? (world-spaceship world)
                  (world-invader-bullets world))
        (make-spaceship (spaceship-direction SPACESHIP-INIT)
                        (spaceship-location SPACESHIP-INIT)
                        (- (spaceship-lives (world-spaceship world))
                           1))
        (move-ship (world-spaceship world)))))
       
;;;; Tests
(define SPACESHIP-TEST-1 (make-spaceship LEFT
                                         (make-posn 435 662.5)
                                         1))
  
(define INVADER-BULLETS-TEST-1 (cons (make-posn 250 360)
                                     (cons (make-posn 435 662.5)
                                           empty)))


(define WORLD-TEST-2 (make-world SPACESHIP-TEST-1
                                 INVADERS-INIT
                                 SHIP-BULLETS-INIT
                                 INVADER-BULLETS-TEST-1
                                 SCORE-INIT
                                 TICK-INIT))
;;;; Tests
(check-expect (spaceship-step WORLD-INIT)
              (make-spaceship LEFT
                              (make-posn 425 662.5)
                              MAX-REMAINING-LIVES))
(check-expect (spaceship-step WORLD-TEST-2)
              (make-spaceship LEFT
                              (make-posn 435 662.5)
                              0))
;;;; Signature
;; between? : NonNegInteger NonNegInteger NonNegInteger -> Boolean

;; Purpose
;; GIVEN: a target number and two other numbers
;; RETURNS: true if the target number is between the low bound and high bound
;;          false otherwise
;;;; Function Definition
(define (between? num low high)
  (and (>= num low)
       (<= num high)))
;;;; Tests
(check-expect (between? 3 2 4)
              #true)
(check-expect (between? 3 4 6)
              #false)

;;;; Signature
;; ship-hit?: SpaceShip InvaderBullets -> Boolean
;;;; Purpose
;; GIVEN: a spaceship and a list of invader bullets
;; RETURNS: true if the spaceship is hit by any of the invader bullets
;;          false otherwise
;;;; Function Definition
(define (ship-hit? ship invader-bullets)
  (local (;;;; Signature
          ;;bullet-hit-ship?: SpaceShip InvaderBullet -> Boolean
          ;;;; Purpose
          ;;GIVEN: a spaceship and an invader bullet
          ;;RETURNS: true if the invader bullet hits the spaceship
          ;;          false otherwise
          ;;;; Function Definition
          (define (bullet-hit-ship? ship invader-bullet)
            (and (between? (posn-x invader-bullet)
                           (- (posn-x (spaceship-location ship))
                              (* (/ 1 2)
                                 SPACESHIP-WIDTH))
                           (+ (posn-x (spaceship-location ship))
                              (* (/ 1 2)
                                 SPACESHIP-WIDTH)))
                 (between? (posn-y invader-bullet)
                           (- (posn-y (spaceship-location ship))
                              (* (/ 1 2)
                                 SPACESHIP-HEIGHT))
                           (+ (posn-y (spaceship-location ship))
                              (* (/ 1 2)
                                 SPACESHIP-HEIGHT))))))
    (ormap (lambda (x) (bullet-hit-ship? ship x))
           invader-bullets)))

;;;; Tests
(define INVADER-BULLETS-TEST-2 (list (make-posn 230 350)
                                     (make-posn 430 665)))
(check-expect (ship-hit? SPACESHIP-INIT INVADER-BULLETS-INIT)
              #false)
(check-expect (ship-hit? SPACESHIP-INIT INVADER-BULLETS-TEST-2)
              #true)

;;;;;;;;;;;;;;;;;;;;;;;;;- Invaders Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                                   

;;;; Signature
;; invaders-step : World -> Invaders
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next invaders after one clock tick:
;;     (1). Removes the invaders that have been hit by the spaceship bullets
;;          from the canvas.
;;     (2). Moves the invaders downward by a certain distance if the clock
;;          ticks get to the multiple of 10.

;;;; Function Definition
(define (invaders-step world)
  (local
    (;;;; Signature
     ;; move-invaders: Invaders -> Invaders
     ;;;; Purpose
     ;; GIVEN: a list of invaders
     ;; RETURNS: the invaders move downwards for a move unit.

     ;;;; Function Definition
     (define (move-invaders invaders)
       (local (;;;; Signature
               ;; move-invader: Invader -> Invader
               ;;;; Purpose
               ;; GIVEN: an invader
               ;; RETURNS: the invader after it moves downwards
               ;;          for a move unit

               ;;;; Function Definition
               (define (move-invader invader)
                 (make-posn (posn-x invader)
                            (+ (posn-y invader)
                               INVADERS-MOVE-UNIT))))
         (map (lambda (x) (move-invader x))
              invaders)))
        
     ;;;; Signature
     ;; remove-hits-invaders: Invaders ShipBullets -> Invaders
     ;;;; Purpose
     ;; GIVEN: a list of invaders and a list of spaceship bullets
     ;; RETURNS: the list of invaders with the invader that is hit by 
     ;;          the spaceship bullets removed.
     ;;;; Function Definition
     (define (remove-hit-invaders invaders ship-bullets)
       (filter (lambda (x) (not (invader-hit? x ship-bullets)))
               invaders)))
  (if (>= (world-tick world) INVADERS-MOVE-TICK)
      (move-invaders (remove-hit-invaders (world-invaders world)
                                           (world-ship-bullets world)))
      (remove-hit-invaders (world-invaders world)
                           (world-ship-bullets world)))))
;;;; Tests
(define SHIP-BULLETS-TEST-1 (cons (make-posn 120 300)
                                  (cons (make-posn 100 90)
                                        (cons (make-posn 158 173)
                                              (cons (make-posn 195 135)
                                                    empty)))))

(define SHIP-BULLETS-TEST-2 (cons (make-posn 162 190)
                                  (cons (make-posn 223 300)
                                        (cons (make-posn 270 500)
                                              empty))))

(define WORLD-TEST-3 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 SHIP-BULLETS-TEST-1
                                 INVADER-BULLETS-INIT
                                 SCORE-INIT
                                 TICK-INIT))

(define INVADERS-TEST-1 (remove (make-posn 165 180) INVADERS-INIT))

(define WORLD-TEST-4 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 SHIP-BULLETS-TEST-2
                                 INVADER-BULLETS-INIT
                                 SCORE-INIT
                                 61))
(define INVADERS-TEST-2 (populate-invaders (make-posn 165 60)
                                           INVADERS-ROW
                                           INVADERS-COLUMN))
;;;; Tests
(check-expect (invaders-step WORLD-INIT)
              INVADERS-INIT)
(check-expect (invaders-step WORLD-TEST-3)
              INVADERS-TEST-1)

(check-expect (invaders-step WORLD-TEST-4)
              (remove (make-posn 165 195)
                      INVADERS-TEST-2))
                                        
;;;; Signature
;; invader-hit?: Invader ShipBullets -> Boolean
;;;; Purpose
;; GIVEN: an invader and a list of spaceship bullets
;; RETURN: true if the invader is hit by any of the spaceship
;;         bullets in the list.
;;         false otherwise.
;;;; Function Definition

(define (invader-hit? invader ship-bullets)
  (local
    (;;;; Signature
     ;; bullet-hit-invader?: Invader SpaceShipBullet -> Boolean
     ;;;; Purpose
     ;; GIVEN: an invader and a spaceship bullet
     ;; RETURN: true if the spaceship bullet hit the invader
     ;;         false otherwise
     ;;;; Function Definition
     (define (bullet-hit-invader? invader ship-bullet)
       (local (;;;; Signature
               ;; between?:
               ;; NonNegInteger NonNegInteger NonNegInteger -> Boolean

               ;; Purpose
               ;; GIVEN:a target number, a low bound and a high bound
               ;; RETURNS: true if the target is between two bounds
               ;;          false otherwise
               ;;;; Function Definition
               (define (between? num low high)
                 (and (>= num low)
                      (<= num high))))
         (and (between? (posn-x ship-bullet)
                        (- (posn-x invader) (* (/ 1 2)
                                               INVADER-WIDTH))
                        (+ (posn-x invader) (* (/ 1 2)
                                               INVADER-WIDTH)))
              (between? (posn-y ship-bullet)
                        (- (posn-y invader) (* (/ 1 2)
                                               INVADER-HEIGHT))
                        (+ (posn-y invader) (* (/ 1 2)
                                               INVADER-HEIGHT)))))))
    (ormap (lambda (x) (bullet-hit-invader? invader x))
           ship-bullets)))
                     

;;;;;;;;;;;;;;;;;;;;;;;;;;;- Ship Bullets Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Siganture
;; ship-bullets-step : World -> ShipBullets
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next list of spaceship bullets after one clock tick:

;;;; Function Definition
(define (ship-bullets-step world)
  (local
    (;;;; Signature
     ;; remove-out-of-bounds-ship-bullets: ShipBullets -> ShipBullets
     ;;;; Purpose
     ;; GIVEN: a list of spaceship bullets
     ;; RETURNS: the list of spaceship bullets with those bullets that are out
     ;;           of bounds removed.
     ;;;; Function Definition
     (define (remove-out-of-bounds-ship-bullets ship-bullets)
       (local (;;;; Signature
               ;; ship-bullet-out?: ShipBullet -> Boolean
               ;;;; Purpose
               ;; GIVEN: a spaceship bullet
               ;; RETURNS: true if the spaceship bullet is out of bounds
               ;;          false otherwise
               ;;;; Function Definition
               (define (ship-bullet-out? ship-bullet)
                 (or (not (between? (posn-x ship-bullet)
                                    0
                                    WIDTH))
                     (not (between? (posn-y ship-bullet)
                                    UP-MARGIN
                                    HEIGHT)))))
         (filter (lambda (x) (not (ship-bullet-out? x)))
                 ship-bullets)))
     ;;;; Signature
     ;; remove-hits-ship-bullets: ShipBullets Invaders -> ShipBullets
     ;;;; Purpose
     ;; GIVEN: a list of spaceship bullets and a list of invaders
     ;; RETURNS: the list of spaceship bullets with the bullets that hit the
     ;;          invaders removed.
     ;;;; Function Definition
     (define (remove-hits-ship-bullets ship-bullets invaders)
       (filter (lambda (x) (not (invader-hit? x invaders)))
               ship-bullets))
     ;;;; Signature
     ;; move-ship-bullets: ShipBullets -> ShipBullets
     ;;;; Purpose
     ;; GIVEN: the list of spaceship bullets
     ;; RETURN: the next list of spaceship bullets that moves upwards for a
     ;;         ship bullet speed unit

     ;;;; Function Definition
     (define (move-ship-bullets ship-bullets)
       (local (;;;; Signature
               ;; move-spaceship-bullet: ShipBullet -> ShipBullet
               ;;;; Purpose
               ;; GIVEN: a spaceship bullet
               ;; RETURNS: the ship bullet that moves upwards for a speed unit
               ;;;; Function Definition
               (define (move-ship-bullet ship-bullet)
                 (make-posn (posn-x ship-bullet)
                            (- (posn-y ship-bullet)
                               SPACESHIP-BULLET-SPEED))))
         (map move-ship-bullet
              ship-bullets))))     
  (move-ship-bullets
   (remove-hits-ship-bullets 
    (remove-out-of-bounds-ship-bullets (world-ship-bullets world))
    (world-invaders world)))))

(define SHIP-BULLETS-TEST-3 (cons (make-posn 223 300)
                                  (cons (make-posn 162 190)
                                        (cons (make-posn 270 -1)
                                              empty))))

(define WORLD-TEST-5 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 SHIP-BULLETS-TEST-3
                                 INVADER-BULLETS-INIT
                                 SCORE-INIT
                                 TICK-INIT))
(define SHIP-BULLETS-TEST-4 (cons (make-posn 230 450)
                                  (cons (make-posn 320 300)
                                        (cons (make-posn 100 600)
                                              empty))))

;;;; Tests
(check-expect (ship-bullets-step WORLD-TEST-5)
              (cons (make-posn 223 290)
                    empty))          

;;;;;;;;;;;;;;;;;;;;;;;;;;- Invader Bullets Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; invader-bullets-step : World -> InvaderBullets
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the next list of invader bullets after one clock tick
;;;; Function Definition
(define (invader-bullets-step world)
  (local
    (;;;; Signature
     ;; remove-out-of-bounds-invader-bullets: InvaderBullets -> InvaderBullets
     ;;;; Purpose
     ;; GIVEN: a list of invader bullets
     ;; RETURNS: the list of invader bullets with those bullets that are out of
     ;;          bounds removed.
     ;;;; Function Definition
     (define (remove-out-of-bounds-invader-bullets invader-bullets)
       (cond
         [(empty? invader-bullets) empty]
         [(cons? invader-bullets)
          (local (;;;; Signature
                  ;; invader-bullet-out?: InvaderBullet -> Boolean
                  ;;;; Purpose
                  ;; GIVEN: an invader bullet
                  ;; RETURNS: true if the invader bullet is out of bounds
                  ;;          false otherwise
                  ;;;; Function Definition
                  (define (invader-bullet-out? invader-bullet)
                    (or (not (between? (posn-x invader-bullet)
                                       0
                                       WIDTH))
                        (not (between? (posn-y invader-bullet)
                                       0
                                       (- HEIGHT DOWN-MARGIN))))))
            (if (invader-bullet-out? (first invader-bullets))
                (rest invader-bullets)
                (cons (first invader-bullets)
                      (remove-out-of-bounds-invader-bullets
                       (rest invader-bullets)))))]))
     ;;;; Signature
     ;; move-invader-bullets: InvaderBullets ->  InvaderBullets
     ;;;; Purpose
     ;; GIVEN: a list of invader bullets
     ;; RETURNS: the next list of invader bullets that all move downwards for a
     ;;          invader bullet speed unit.
     (define (move-invader-bullets invader-bullets)
       (local
         (;;;; Signature
          ;; move-invader-bullet: InvaderBullet -> InvaderBullet
          ;;;; Purpose
          ;; GIVEN: an invader bullet
          ;; RETURN: the invader bullet that moves upwards for a speed unit
          ;;;; Function Definition
          (define (move-invader-bullet invader-bullet)
            (make-posn (posn-x invader-bullet)
                       (+ (posn-y invader-bullet)
                          INVADER-BULLET-SPEED))))
         (map move-invader-bullet invader-bullets)))
     ;;;; Signature
     ;; invaders-fire: NonNegInteger Invaders InvaderBullets -> InvaderBullets
     ;; Purpose
     ;; GIVEN: an amount of bullets that the invaders could shoot and
     ;;        a list of invaders and a list of invader bullets
     ;; RETURNS: if the number of bullets is less than the max number of 
     ;;          invader bullets, randomly choosen invaders will fire bullets
     ;;           which makes the total bullets number up to the max.

     ;;;; Function Definition
     (define (invaders-fire bullet-amount invaders invader-bullets)
       (cond
         [(or (> (+ bullet-amount
                    (length invader-bullets))
                 MAX-INVADER-BULLETS)
              (< bullet-amount
                 1))
          invader-bullets]
         [else
          (local (;;;; Signature
                  ;; list-item-at-ref: Invaders NonNegInteger -> Invader
                  ;;;; Purpose
                  ;; GIVEN: a list of invaders and a reference index
                  ;; RETURNS: the invader that is at the reference index
                  ;;          in the list invaders.
                  (define (list-item-at-ref invaders ref)
                    (cond
                      [(empty? invaders) (error "Empty list does not have ref")]
                      [(cons? invaders)
                       (if (< ref (length invaders))
                           (if (= ref 0)
                               (first invaders)
                               (list-item-at-ref (rest invaders)
                                                 (sub1 ref)))
                           (error "ref is out of bounds"))])))
            (cons (list-item-at-ref invaders
                                    (random (length invaders)))
                  (invaders-fire (sub1 bullet-amount)
                                 invaders
                                 invader-bullets)))])))           
     (invaders-fire (- MAX-INVADER-BULLETS
                        (length (remove-out-of-bounds-invader-bullets
                                 (world-invader-bullets world))))
                     (world-invaders world)
                     (move-invader-bullets
                      (remove-out-of-bounds-invader-bullets
                       (world-invader-bullets world))))))
;;;; Tests
(define INVADER-BULLETS-TEST-3 (cons (make-posn 230 450)
                                     (cons (make-posn 320 300)
                                           (cons (make-posn 100 600)
                                                 empty))))

(define INVADER-BULLETS-TEST-4 (cons (make-posn 230 450)
                                     (cons (make-posn 320 878)
                                           (cons (make-posn 100 235)
                                                 empty))))
(define WORLD-TEST-6 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 SHIP-BULLETS-INIT
                                 INVADER-BULLETS-TEST-3
                                 SCORE-INIT
                                 TICK-INIT))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;- Score Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; score-step: World -> NonNegInteger
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the player's next score after one clock tick
;;;; Function Defintion
(define (score-step world)
  (+ (world-score world)
     (local
       (;;;; Signature
        ;; num-of-hits: Invaders ShipBullets -> NonNegInteger
        ;;;; Purpose
        ;; GIVNEN: the current list of invaders and list of spaceship bullets
        ;; RETURNS: the number of invaders that are hit by the spaceship 
        ;;          bullets after one clock tick.
        ;;;; Function Definition
        (define (num-of-hits invaders ship-bullets)
          (cond
            [(empty? invaders) 0]
            [(cons? invaders) (if (invader-hit? (first invaders) ship-bullets) 
                             (+ 1
                                (num-of-hits (rest invaders) ship-bullets))
                             (num-of-hits (rest invaders) ship-bullets))])))                 
     (* (num-of-hits (world-invaders world)
                     (world-ship-bullets world))
        SCORE-UNIT))))

;;;; Tests
(check-expect (score-step WORLD-INIT)
              0)
(check-expect (score-step WORLD-TEST-3)
              3)
(check-expect (score-step WORLD-TEST-4)
              3)
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;- Tick Step -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; tick-step: World -> NonNegInteger
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: the tick of the world after one clock tick
;;;; Function Definiton
(define (tick-step world)
  (if (>= (world-tick world) INVADERS-MOVE-TICK)
      TICK-INIT
      (+ (world-tick world)
         1)))
;;;; Test
(check-expect (tick-step WORLD-INIT)
              1)
(check-expect (tick-step WORLD-TEST-4)
              0)   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;- End Game -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
;;;; Signature
;; end-game? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: true if either the player wins or loses
;; Function Definition
(define (end-game? world)
  (or (player-wins? world)
      (player-loses? world)))

;;;; Signature
;; player-wins? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: ture if all the invaders have been hit by the spaceship bullets
;;          false otherwise
;;;; Function Definition
(define (player-wins? world)
  (empty? (world-invaders world)))

;;;; Signature
;; player-loses? : World -> Boolean
;;;; Purpose
;; GIVEN: the current world
;; RETURNS: true if either of the two conditions has been met:
;;          1. the lives of spaceship turn to zero.
;;          2. the down most invaders have reached the same y coordinate with
;;             the spaceship.
;;          false otherwise
;;;; Function Definition
(define (player-loses? world)
  (local (;;;; Signature
          ;; run-out-of-lives?: World -> Boolean
          ;;;; Purpose
          ;; GIVEN: the current world
          ;; RETURNS: true if the the player is running out of lives
          ;;          false otherwise
          ;;;; Function Definition
          (define (run-out-of-lives? world)
            (and (= 0 (spaceship-lives (world-spaceship world)))
                 (ship-hit? (world-spaceship world)
                            (world-invader-bullets world))))
          ;;;; Signature
          ;; invaders-reach-ship? : World -> Boolean
          ;;;; Purpose
          ;; GIVEN: the current world
          ;; RETURNS: true if the max of y coordinate of all invaders reach
          ;;          that of the spaceship
          ;;          false otherwise
          ;;;; Function Definition
          (define (invaders-reach-ship? world)
            (local
              (;;;; Signature
               ;; invaders-max-y: Invaders -> NonNegInteger
               ;;;; Purpose
               ;; GIVNE: a list of invaders
               ;; RETURNS: the max y coordinate of all invaders

               ;;;; Function Definition
               (define (invaders-max-y invaders)
                 (cond
                   [(empty? (rest invaders)) (posn-y (first invaders))]
                   [(cons? (rest invaders))
                    (if (> (posn-y (first invaders))
                           (posn-y (first (rest invaders))))
                        (invaders-max-y (cons (first invaders)
                                              (rest (rest invaders))))
                        (invaders-max-y (rest invaders)))])))    
            (>= (+ (invaders-max-y (world-invaders world))
                   (* (/ 1 2)
                      INVADER-HEIGHT))
                (- (posn-y (spaceship-location (world-spaceship world)))
                   (* (/ 1 2)
                      SPACESHIP-HEIGHT))))))
    (or (run-out-of-lives? world)
        (invaders-reach-ship? world))))

       
;;;; Tests
(define INVADERS-TEST-3 empty);; to test the situation that player wins
(define INVADERS-TEST-4 (cons (make-posn 165 675)
                              (cons (make-posn 165 630)
                                    empty))) ;; to test the invaders has
;; reached the same y level as the spaceship
(define SPACESHIP-TEST-2
  (make-spaceship LEFT
                  (make-posn (* (/ 1 2)
                                WIDTH)
                             (- HEIGHT
                                (+ DOWN-MARGIN
                                   (* (/ 1 2)
                                      SPACESHIP-HEIGHT))))
                  0))
;; (435, 662.5) and 0 lives left


(define INVADER-BULLETS-TEST-5 (cons (make-posn 418 660)
                                     INVADER-BULLETS-TEST-3))
(define WORLD-TEST-7 (make-world SPACESHIP-INIT
                                 INVADERS-INIT
                                 SHIP-BULLETS-INIT
                                 INVADER-BULLETS-TEST-5
                                 SCORE-INIT
                                 TICK-INIT))
(define WORLD-TEST-8 (make-world SPACESHIP-INIT
                                 INVADERS-TEST-3
                                 SHIP-BULLETS-TEST-2
                                 INVADER-BULLETS-TEST-5
                                 30
                                 40))

(define WORLD-TEST-9 (make-world SPACESHIP-TEST-2
                                 INVADERS-INIT
                                 SHIP-BULLETS-INIT
                                 INVADER-BULLETS-TEST-5
                                 SCORE-INIT
                                 35))


(define WORLD-TEST-10 (make-world SPACESHIP-INIT
                                  INVADERS-TEST-4
                                  SHIP-BULLETS-INIT
                                  INVADER-BULLETS-INIT
                                  SCORE-INIT
                                  21))

(check-expect (end-game? WORLD-TEST-1)
              false)                
(check-expect (end-game? WORLD-TEST-7)
              false)
(check-expect (end-game? WORLD-TEST-8)
              true)
(check-expect (end-game? WORLD-TEST-9)
              true)
(check-expect (end-game? WORLD-TEST-10)
              true)


    
;;;; Signature
;; final-scene: World -> Image
;;;; Purpose
;; GIVEN: the final world
;; RETURNS: Image that shows "Congratulations, You Win!" if the player wins
;;          Image that shows "So sad, You Lose!" otherwise
;;;; Function Definition 
(define (final-scene world)
  (cond
    [(player-wins? world)
     (place-image FINAL-WIN-SCENE
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG)]
    [(player-loses? world)
     (place-image FINAL-LOSE-SCENE
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG)]))
;;;; Tests
(check-expect (final-scene WORLD-TEST-8)
              (place-image (text "Congratulations, You Win!" 50 "red")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG))
(check-expect (final-scene WORLD-TEST-9)
              (place-image (text "So sad, You Lose!" 50 "black")
                  (* (/ 1 2) WIDTH)
                  (* (/ 1 2) HEIGHT)
                  BG))
;;;;;;;;;;;;;;;;;;;;;;;;;;;- Key Handler -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Signature
;; key-handler : World Key-Event -> World
;;;; Purpose
;; GIVEN: the current world and a key event
;; RETURNS: a new world updated after the key event
;;;; Function Definition
(define (key-handler world ke)
  (local
    (;;;; Signature
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
                   (world-ship-bullets world)
                   (world-invader-bullets world)
                   (world-score world)
                   (world-tick world)))
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
                   (world-ship-bullets world)
                   (world-invader-bullets world)
                   (world-score world)
                   (world-tick world)))
     ;;;; Signature
     ;; spaceship-fires: World -> World
     ;;;; Purpose
     ;; GIVEN: a world
     ;; RETURNS: the world with its spaceship shoot a bullet if the number of
     ;;          spaceship bullets is below MAX

     ;;;; Function Definition
     (define (spaceship-fires world)
       (if (>= (length (world-ship-bullets world))
               MAX-SPACESHIP-BULLETS)
           world
           (make-world (world-spaceship world)
                       (world-invaders world)
                       (cons (spaceship-location (world-spaceship world))
                             (world-ship-bullets world))
                       (world-invader-bullets world)
                       (world-score world)
                       (world-tick world)))))
     (cond 
       [(key=? ke LEFT-HANDLER) (spaceship-direction-to-left world)]
       [(key=? ke RIGHT-HANDLER) (spaceship-direction-to-right world)]
       [(key=? ke SHIP-FIRE-HANDLER) (spaceship-fires world)]
       [else world])))

;;;; Tests
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
                          (world-ship-bullets WORLD-INIT)
                          (world-invader-bullets WORLD-INIT)
                          (world-score WORLD-INIT)
                          (world-tick WORLD-INIT)))
(check-expect (key-handler WORLD-INIT " ")
              (make-world (world-spaceship WORLD-INIT)
                          (world-invaders WORLD-INIT)
                          (cons (spaceship-location
                                 (world-spaceship WORLD-INIT))
                                (world-ship-bullets WORLD-INIT))
                          (world-invader-bullets WORLD-INIT)
                          (world-score WORLD-INIT)
                          (world-tick WORLD-INIT)))

(define WORLD-TEST-11 (make-world SPACESHIP-INIT
                                  INVADERS-INIT
                                  SHIP-BULLETS-TEST-3
                                  INVADER-BULLETS-TEST-2
                                  SCORE-INIT
                                  TICK-INIT))


(check-expect (key-handler WORLD-TEST-11 " ")
              WORLD-TEST-11)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;- Big Bang -;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(big-bang WORLD-INIT
          (on-tick world-step 0.15)
          (to-draw draw-world)
          (on-key key-handler)
          (stop-when end-game? final-scene))
