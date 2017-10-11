; Ziyu Qiu (ziyuqiu@brandeis.edu) 2016-01-31
; PS1-Lunar Lander

;;;THIS IS THE GIVEN CODE
;; this is the code for problem set -- Lunar Lander
;random
(#%require (only racket/base random))

;update
;See modification in problem1
;(define (update ship-state fuel-burn-rate)
;  (make-ship-state
;   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
;   (+ (velocity ship-state)
;      (* (- (* engine-strength fuel-burn-rate) gravity)
;         dt))                                           ; velocity
;   (- (fuel ship-state) (* fuel-burn-rate dt))))        ; fuel
  

;lander-loop
;See modification in problem2
;(define (lander-loop ship-state)
;  (show-ship-state ship-state)
;  (if (landed? ship-state)
;      (end-game ship-state)
;      (lander-loop (update ship-state (get-burn-rate)))))

;show-ship-state
;input:ship-state
;output:display list of height, velocity and fuel
(define (show-ship-state ship-state)
  (write-line 
    (list 'height (height ship-state)
          'velocity (velocity ship-state)
          'fuel (fuel ship-state))))
;landed?
;input:ship-state
;output:yes if height<=0, else no
(define (landed? ship-state)
  (<= (height ship-state) 0))

;end-game
;input:ship-state
;output: if final-velocity>=safe-velocity then print"good landing", else print "you crashed"
(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
       (write-line final-velocity)
       (cond ((>= final-velocity safe-velocity)
               (write-line "good landing")
               'game-over)
             (else
               (write-line "you crashed!")
               'game-over))))
;get-burn-rate
;input:nothing
;output:if player-input=burn-key then return 1, else return 0
(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

(define (play) (lander-loop (initial-ship-state)))

(define (initial-ship-state)
  (make-ship-state 50       ; 50 km high
                   0        ; not moving (0 km/sec)
                   20))     ; 20 kg of fuel left

(define dt 1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define (player-input) 
  (char->integer (prompt-for-command-char " action: "))) 

(define burn-key 32)   ;space key

; You'll learn about the stuff below here in Chapter 2.
; For now, think of make-ship-state, height, velocity, and fuel
; as primitive procedures built in to Scheme.

;make-ship-state
;input:height, velocity, fuel
;output:list of height, velocity, fuel
(define (make-ship-state height velocity fuel)
  (list 'HEIGHT   height
        'VELOCITY velocity
        'FUEL     fuel))

(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))

(define (second l) (cadr l))
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))

; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...

; for input and output

(define (write-line x)
  (display x)
  (newline))

(define (get-one-key)
  (let ((x (read-char)))
    (if (eq? x #\newline)
        x
        (empty-buffer x))))

(define (empty-buffer x)
  (if (eq? (read-char) #\newline)
      x
      (empty-buffer x)))

(define (prompt-for-command-char prompt)
  (display prompt)
  (get-one-key)) 

; for random number generation

(#%require (only racket/base random))

; a ridiculous addendum  (you'll need this for the exercises)

(define (1+ x) (+ 1 x))

;;;HERE STARTS MY CODE
;;;Problem 1
;solve out of fuel problem
;Modification in /update/ attached below
;update, updates the ship's position and velocity
;input:ship-state fuel-burn-rate
;output:new state of the ship(result of make-ship-state)
(define (update ship-state fuel-burn-rate)
  (let((fuel-burn-rate
       (cond   ((> fuel-burn-rate 1) 1) ;line added for problem 10
               ((> fuel-burn-rate (/ (fuel ship-state) dt))
                      (/ (fuel ship-state) dt))
               (fuel-burn-rate)
  )))
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)
      (* (- (* engine-strength fuel-burn-rate) gravity)
         dt))                                           ; velocity
   (- (fuel ship-state) (* fuel-burn-rate dt))))); fuel

;;;Problem 2
;allow choosing mode at the beginning
;Modification in /play/ and /lander-loop/ is attached below
(define (full-burn ship-state) 1)
(define (no-burn ship-state) 0)
(define (ask-user ship-state)(get-burn-rate))

;lander-loop
;input:ship-state
;output:if landed, output end-game word depending on velosity, else update
(define (lander-loop ship-state strategy);add parameter burn-rate
  (show-ship-state ship-state)
  (if (landed? ship-state)
      (end-game ship-state)
      (if(number? (strategy ship-state))
         (lander-loop (update ship-state (strategy ship-state)) strategy)
         (lander-loop (update ship-state (get-burn-rate)) strategy) 
      )))
;play
;input:burn-rate
;output: list of height, velocity, fuel
(define (play fuel-burn-rate) (lander-loop (initial-ship-state) fuel-burn-rate));add parameter burn-rate


;;;Problem 3
;randomly choose a mode to play
;(define (random-choice choice1 choice2)
;  (lambda (ship-state)
;    (if (= (random 2) 0)
;      (choice1 ship-state)
;      (choice2 ship-state))))

;;;Problem 4
;play choice1 when the height>marginal-height, else play choice2
;(define (height-choice choice1 choice2 marginal-height)
;  (lambda(ship-state)
;    (if(> (height ship-state) marginal-height)
;     (choice1 ship-state)
;     (choice2 ship-state))))

;;;Problem 5
;general height-choice
(define (choice strategy-1 strategy-2 deciding)
  (lambda(ship-state)
  (if(deciding ship-state)
  (strategy-1 ship-state)
  (strategy-2 ship-state))))
(define(random-choice strategy-1 strategy-2)
  (choice strategy-1
          strategy-2
          (lambda(ship-state) (= (random 2) 0))))
(define (height-choice strategy-1 strategy-2 marginal-height)
  (choice strategy-1
          strategy-2
          (lambda(ship-state)(> (height ship-state) marginal-height))))

;;;Problem 6
(define (compound ship-state)
    (if(> (height ship-state) 40)
       (no-burn ship-state)
       (random-choice full-burn ask-user)))

;;;Problem 7
;see write-up

;;;Problem 8
(define (square x) (* x x))
(define (acc ship-state)(/ (square (velocity ship-state)) (* 2 (height ship-state))))
(define (constant-acc ship-state)
 (/ (+ (acc ship-state) gravity) engine-strength))

;;;Problem 9
;see-write-up

;;;Problem 10
;See modification in /update/

;;;Problem 11
(define (optimal-constant-acc optimal-height)
  (height-choice no-burn constant-acc optimal-height))

;;;Problem 12&13
;See write-up
