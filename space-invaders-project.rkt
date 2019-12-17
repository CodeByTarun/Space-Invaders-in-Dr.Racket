;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; To run (main G1)
(define (main s)
      (big-bang s
        (on-tick mover)               ; on tick, move the missles and the invaders
        (on-key handle-key)           ; when left or right key down move tank
        (to-draw allimages)           ; draw all images 
        (stop-when invader-bottom)))  ; when invader hits the bottom end the game


;; Purpose: on tick, the missile and the invaders y and x,y positions change respectively
;; - Need to add a removal function for the invaders and for the missiles
;; - Also need to add invaders randomly
      
(define (mover s)
  (filterhits (make-game (invadermover (addinvader (game-invaders s)))
                         (removemissiles(missilemover (game-missiles s)))
                         (tankmove (game-tank s)))))

;; Purpose: add invaders randomly to the list
(define (addinvader loi)
  (if (> 2 (random 200))
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi) 
      loi))
  
;; Purpose: Loop through invader list
(define (invadermover loi)
  (cond [(empty? loi) empty]
        [else
         (cons (invadermove (first loi))
               (invadermover (rest loi)))]))

;; Purpose: move the invader position by dx for x and y
;; If the invader is going past the wall then create a new invader with the opposite direction!!!
(define (invadermove i)
  (cond [(> (+ (invader-x i) (invader-dx i)) WIDTH)
         (make-invader (invader-x i) (invader-y i) (* -1 (invader-dx i)))]
        [(< (+ (invader-x i) (invader-dx i)) 0)
         (make-invader (invader-x i) (invader-y i) (* -1 (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;; Purpose: Loop through missile list 
(define (missilemover lom)
  (cond [(empty? lom) empty]
        [else
         (cons (missilemove (first lom))
               (missilemover (rest lom)))]))

;; Purpose: Move the missile in the y direction by MISSILE-SPEED
(define (missilemove m) 
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Purpose: Loop through missile list and remove missiles that are above HEIGHT
(define (removemissiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (above? (first lom))
             (removemissiles (rest lom))
             (cons (first lom) (removemissiles (rest lom))))]))

;; Purpose: Return true if y for the missile is above HEIGHT
(define (above? m)
  (< (missile-y m) 0)) 

;; Purpose: move the tank in tank-dir by TANK-SPEED
(define (tankmove t)
  (cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 300)
         (make-tank 300 (tank-dir t))]
        [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0)
         (make-tank 0 (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))])) 


;; Check if any invaders and missles have matching positions and remove if they do
(define (filterhits s)
  (make-game (m-hit-i (game-invaders s) (game-missiles s))
             (i-hit-m (game-invaders s) (game-missiles s))
             (game-tank s)))

;;Purpose: get list of invaders after checking for collisons
(define (m-hit-i loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (hit? (first loi) lom)
             (m-hit-i (rest loi) lom)
             (cons (first loi) (m-hit-i (rest loi) lom)))]))

;; check for collisons
(check-expect (hit? I1 (cons (make-missile 140 110) empty)) true)
(check-expect (hit? I1 (cons (make-missile 170 110) empty)) false)

(define (hit? i lom)
  (cond [(empty? lom) false]
        [else
         (if (inrange? i (first lom))
             true
             (hit? i (rest lom)))]))

;; Determine if invaders are in range of missiles
(check-expect (inrange? I1 (make-missile 170 110)) false)
(check-expect (inrange? I1 (make-missile 140 110)) true)

(define (inrange? i m)
  (and
   (<= (abs (- (invader-x i) (missile-x m))) 10)
   (<= (abs (- (invader-y i) (missile-y m))) 10)))

;; Determine if missiles are in range of invaders
(define (i-hit-m loi lom)
  (cond [(empty? lom) empty]
        [else
         (if (hitm? (first lom) loi)
             (i-hit-m loi (rest lom))
             (cons (first lom) (i-hit-m loi (rest lom))))]))

;; cycle through invaders to check for ones in range
(define (hitm? m loi)
  (cond [(empty? loi) false]
        [else
         (if (inrange? (first loi) m)
             true
             (hitm? m (rest loi)))]))

;; Purpose: when the left or right key are held down, the tank is moved left or right respectively at some rate (this might require some rate on it????)
;; - If the tank reaches either side of the wall then it will no longer move further in that direction (change speed to 0)
;; So two functions occuring on one list
(define (handle-key s ke)
  (make-game (game-invaders s)
             (shoot (game-missiles s) ke (game-tank s))
             (tankdirection (game-tank s) ke)))

;; Purpose: Go throught the game missles list

(define (shoot lom ke t)
    (cond [(key=? ke " ") (cons (make-missile (tank-x t) (- 500 TANK-HEIGHT/2)) lom)]
          [else lom]))

;; Purpose: chenge direction of the tank  
(define (tankdirection t ke)
  (cond [(key=? ke "left")
         (if (= (tank-dir t) 1)
         (make-tank (tank-x t) (* -1 (tank-dir t)))
         t)]
        [(key=? ke "right")
         (if (= (tank-dir t) -1)
         (make-tank (tank-x t) (* -1 (tank-dir t)))
         t)]          
        [else t]))

;; Purpose: Place all new images in program (will require compound functions, one function for the addition of each image type)
;; !!!
(define (allimages s)
  (invaderimage (game-invaders s) (game-missiles s) (game-tank s)))

;; List and Image -> Image
;; Purpose: cycle through list to render all invaders

(define (invaderimage loi lom t)
  (cond [(empty? loi) (missileimage lom t)]
        [else
         (invaderplacer (first loi) (invaderimage (rest loi) lom t))]))

(define (invaderplacer i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

(define (missileimage lom t)
  (cond [(empty? lom) (tankimage t)]
        [else
         (missileplacer (first lom) (missileimage (rest lom) t))]))

(define (missileplacer i img)
  (place-image MISSILE (missile-x i) (missile-y i) img))

(define (tankimage t)
  (place-image TANK (tank-x t) (- 500 TANK-HEIGHT/2) BACKGROUND))


;; Stop the game when an invader hits the ground (y = 0)
;; This needs to return a boolean!!!
;; !!!

(define (invader-bottom s)
  (bottom? (game-invaders s)))

(define (bottom? loi)  
  (cond [(empty? loi) false]
        [else
         (if (bottomi? (first loi))
             true
             (bottom? (rest loi)))]))

(define (bottomi? i)
  (> (invader-y i) 500))





















