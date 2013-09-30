;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |connect four starter -- testing|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")))))
;; Jesse Earisman & Zachary Robbins

(require 2htdp/image)
(require 2htdp/universe)

;; do not modify the constants, except for experimentation with ROWS and COLUMNS
(define RED 1) 
(define BLACK 2)
(define BLANK 0)
(define ROWS 9)  ;; can vary
(define COLUMNS 8) ;; can vary
;; Heuristic Values
(define ALLY-SCORE 2)
(define BLANK-SCORE 2)
(define ENEMY-SCORE 0)
(define MINIMAX-DEPTH 3)
;; Board is defined as as list of columns
;; each column is ROW units high
;; the list is COLUMN units big.

;; a coordinate structure, makes things easier, probably
;; x is column position and should not exceed COLUMNS
;; y is row position and should not exceed ROMS
(define-struct coor (x y))

;; coor world-state -> number
;; retrieves the value at the given coordinate
;; works just like piece at, but uses coor struct, because it simplifies much of the code, esp. functions that take a list of coor
(check-expect (get-checker (make-coor 5 5) start-state) 0)
(check-expect (get-checker (make-coor 4 7) test-state) 2)
(check-expect (get-checker (make-coor 4 8) test-state) 1)

(define (get-checker c state)
  (get-nth (coor-y c) (get-nth (coor-x c) (world-state-position state))))

;; coor -> Boolean
;; checks to see if the given coordinate is valid for our board
;; check-expects are in order of cond statement in function, all pass
(check-expect (valid? (make-coor -1 0)) false)
(check-expect (valid? (make-coor 0 -1)) false)
(check-expect (valid? (make-coor COLUMNS 0)) false)
(check-expect (valid? (make-coor 0 ROWS)) false)
(check-expect (valid? (make-coor 7 8)) true)
(define (valid? c)
  (cond 
    [(< (coor-x c) 0) false]
    [(< (coor-y c) 0) false]
    [(= (coor-x c) COLUMNS) false]
    [(= (coor-y c) ROWS) false]
    [else true]))

;; state --> Number.  
;; evaluates how good a state is.  larger values are better for red (human) 
;; while smaller values are better for black (computer)
(check-expect (evaluation-function start-state) 0)
(check-expect (evaluation-function test-state) 5)
(check-expect (evaluation-function (make-world-state
                                    (list
                                     (list 0 0 0 0 0 0 0 0 0)
                                     (list 0 0 0 0 0 0 0 0 0)
                                     (list 0 0 0 0 0 0 0 0 1)
                                     (list 0 0 0 0 0 0 0 0 1)
                                     (list 0 0 0 0 0 0 0 0 1)
                                     (list 0 0 0 0 0 0 0 0 1)
                                     (list 0 0 0 0 0 0 0 0 0)
                                     (list 0 0 0 0 0 0 0 0 0))
                                    RED
                                    5
                                    empty)) (+ (* 4 999999) (* 12 1)))

(define (evaluation-function state)
  (local
    [(define (eval-coor c)  ;; coor -> number
       (local
         [(define checker (get-checker c state))
          (define (eval-direction c2 x y maximum) ;; evaluates the score of this coordinate, reletive to the focal coordinate
            (if (not (valid? c2))
                (list maximum false)
                (local
                  [(define other-checker (get-checker c2 state))]
                  (cond
                    ; I didn't want to make a new structure, so i committed original sin and made a multi-type list.
                    ; May god have mercy on my soul
                    ; the first element of the list is number of checkers in a row, the second is a boolean that is true if the first non-ally square is empty
                    [(= other-checker BLANK) (list maximum true)]
                    [(= checker other-checker)
                     (eval-direction (next-coor c2 x y) x y (add1 maximum))]
                    [else (list maximum false)]))))
          
          ;; makes a new coor by incrementing the given coordinate by the given x and y values
          (define (next-coor c1 x y)
            (make-coor (+ (coor-x c1) x) (+ (coor-y c1) y)))
          
          ;; changes one direction two directions, on either side of the checker, moves up the ladder
          (define (eval-dir-helper x y) 
            (local
              [(define one-dir (eval-direction (next-coor c x y) x y 0))
               (define other-dir  (eval-direction (next-coor c (- x) (- y)) (- x) (- y) 0))]
              (score (+ (first one-dir) (first other-dir) 1) (second one-dir) (second other-dir))))
          
          ;; helper function, changes a direcion [0,3] to x and y values [-1,1]
          ;; then moves up the ladder of functions on the way to evaluating a coordinate
          (define (dir-to-xy dir)
            (cond
              [(= 0 dir) (eval-dir-helper 1 0)]
              [(= 1 dir) (eval-dir-helper 1 1)]
              [(= 2 dir) (eval-dir-helper 0 1)]
              [(= 3 dir) (eval-dir-helper -1 1)]))
          
          ;; scores the line of checkers that extends on both sides of the focal checker
          ;; maximum is the number of checkers in that row.
          ;; The two booleans reflect if the endpoints of the line are empty spaces (true) or unavailable (false)
          (define (score maximum one-side-empty? other-side-empty?)
            (cond 
              [(and one-side-empty? other-side-empty?) (* 2 maximum)]
              [(or one-side-empty? other-side-empty?) maximum]
              [else 0]))]
         
         (if (= checker BLANK) 0
             (* (foldr + 0 (map dir-to-xy (list 0 1 2 3))) (if (= checker RED) 1 -1)))))
     
     ;; generates a list of all valid coordinates on the board
     (define (get-all-coor x y)
       (cond
         [(= y ROWS) empty]
         [(= x COLUMNS) (get-all-coor 0 (add1 y))]
         [else (cons (make-coor x y) (get-all-coor (add1 x) y))]))]
    (foldr + 0 (map eval-coor (get-all-coor 0 0)))))


;; state --> state
;; you will implement this function to make a move
(check-expect (computer-moves test-state)  ;; tests to see if the computer will block a red victory
              (make-world-state
               (list
                (list 0 0 0 0 0 0 0 0 0) 
                (list 0 0 0 0 0 0 0 0 0) 
                (list 0 0 0 0 0 0 0 0 0) 
                (list 0 0 0 0 0 0 2 1 2)
                (list 0 0 0 0 0 0 0 2 1)
                (list 0 0 0 0 0 0 0 0 1)
                (list 0 0 0 0 0 0 0 0 1)
                (list 0 0 0 0 0 0 0 0 2))
               RED
               5 empty))

(check-expect (computer-moves  ;; tests to see if the computer will take a victory over blocking red.
               (make-world-state
                (list
                 (list 0 0 0 0 0 0 0 0 2)
                 (list 0 0 0 0 0 0 0 0 2)
                 (list 0 0 0 0 0 0 0 0 2) 
                 (list 0 0 0 0 0 0 0 0 0)  ; <- this is the winning move
                 (list 0 0 0 0 0 0 0 0 0)  ; <- this is the blocking move
                 (list 0 0 0 0 0 0 0 0 1)
                 (list 0 0 0 0 0 0 0 0 1)
                 (list 0 0 0 0 0 0 0 1 1))
                BLACK
                5 empty))
              (make-world-state
               (list
                (list 0 0 0 0 0 0 0 0 2)
                (list 0 0 0 0 0 0 0 0 2)
                (list 0 0 0 0 0 0 0 0 2) 
                (list 0 0 0 0 0 0 0 0 2)
                (list 0 0 0 0 0 0 0 0 0)
                (list 0 0 0 0 0 0 0 0 1)
                (list 0 0 0 0 0 0 0 0 1)
                (list 0 0 0 0 0 0 0 1 1))
               RED
               5 empty)) 



(define (computer-moves state)
  (make-move state (find-best (legal-next-moves state) MINIMAX-DEPTH false state)))

(define (eval-move move depth max? state)      ;; move -> score
  (local
    [(define next-state (make-move state move))]
    (if (or (= 0 depth) (check-win? next-state))  ; if we have reached the depth, it returns the value at the bottom of the tree
        (evaluation-function next-state)          ; if the last move ended the game, we don't need to look farther, because the game ends
        (evaluation-function (make-move next-state (find-best (legal-next-moves next-state) (sub1 depth) (not max?) next-state)))))) 
        ; if we aren't there yet, evaluate the result of the find-best function

;; lom -> move
;; takes the best move of the list
(define (find-best lom depth max? state)
  (cond
    [(empty? (rest lom)) (first lom)]
    [else
     (local
       [(define this-move-score (eval-move (first lom) depth max? state))
        (define rbest (find-best (rest lom) depth max? state))
        (define rbest-score (eval-move rbest depth max? state))]
       (cond
         [(and max? (> this-move-score rbest-score)) (first lom)]
         [(and (not max?) (< this-move-score rbest-score)) (first lom)]
         [else rbest]))]))



;; you must implement the above two functions as part of the asignment, but may create additional
;; helper functions

;; ===========================================
;; you should not modify code below this point
;; ===========================================

;; the representation of the world.  you should not be modifying this structure or its contents
;; to conduct the search, you can create instances of this structure as your representation

;; *** the settings part of world-state is now defined as time (a Number > 0)
;; *** as well as other-info, which is user-defined and can include anything you like
;; *** you can define a structure and add it to the state representation, or
;; *** just store a number.  the provided code will not make use of it
(define-struct world-state (position whose-turn settings other-info))
;;                                                         ^^^^


;; returns the checker color (RED, BLACK, or BLANK) of the specified position
;; on the board
(define (piece-at board row column)
  (get-nth row (get-nth column board)))


;; Natural List --> Element
;; returns the nth element of a list
(define (get-nth n alist)
  (local [(define (nth-helper n current alist)
            (cond
              [(= n current)(first alist)]
              [else
               (nth-helper n (+ 1 current) (rest alist))]))]
    (nth-helper n 0 alist)))

(define (main state)
  (local 
    [(define board 
       (make-list COLUMNS
                  (make-list ROWS 0)))
     
     (define PIECE-SIZE 30)
     
     (define RED-CHECKER (circle PIECE-SIZE "solid" "red"))
     (define BLACK-CHECKER (circle PIECE-SIZE "solid" "black"))
     (define BLANK-CHECKER (circle PIECE-SIZE "solid" "white"))
     
     (define OFFSET (/ PIECE-SIZE .66))
     (define WIDTH
       (+ (* COLUMNS 2.5 PIECE-SIZE) (* 0.5 PIECE-SIZE)))
     (define HEIGHT
       (+ (* ROWS 2.5 PIECE-SIZE) (* 0.5 PIECE-SIZE)))
     
     (define MTS 
       (rectangle WIDTH HEIGHT "solid" "yellow"))
     (define (place-checker state x y mouse-event)
       (local
         [(define move (map-coordinates x y))
          (define next-state (make-move state move))]
         (cond
           [(and (string=? mouse-event "button-down")
                 (member move (legal-next-moves state)))
            (if (check-win? next-state)  
                (cond
                  [(= (world-state-whose-turn state) RED)
                   "RED WINS"]
                  [(= (world-state-whose-turn state) BLACK)
                   "BLACK WINS"])
                (local [(define result (computer-moves next-state))]
                  (if (check-win? result)
                      (cond
                        [(= (world-state-whose-turn next-state) RED)
                         "RED WINS"]
                        [(= (world-state-whose-turn next-state) BLACK)
                         "BLACK WINS"])
                      result)))]
           [else state])))
     (define (display-column2 column x-offset y-offset image)
       x-offset)
     (define (display-column column x-offset y-offset image)
       (cond
         [(empty? column) image]
         [else
          (place-image
           (cond 
             [(= (first column) RED) RED-CHECKER]
             [(= (first column) BLACK) BLACK-CHECKER]
             [(= (first column) BLANK) BLANK-CHECKER])
           x-offset y-offset 
           (display-column (rest column) x-offset (+ y-offset (* 2.5 PIECE-SIZE)) image))]))
     
     (define (display-board-helper position x-offset image)
       (cond 
         [(empty? position) image]
         [else
          (display-board-helper
           (rest position)
           (+ x-offset (* 2.5 PIECE-SIZE))
           (display-column (first position)
                           x-offset
                           OFFSET image))]))
     
     (define (display-board position)
       (display-board-helper position OFFSET MTS))
     
     (define (render state)
       (if (string? state)
           (text state 40 'black)
           (display-board (world-state-position state))))
     
     (define (map-coordinate lower upper click pos)
       (cond
         [(and (> click lower) (< click upper)) pos]
         [(> pos (max ROWS COLUMNS)) -1]
         [else
          (map-coordinate (+ lower (* 2.5 PIECE-SIZE)) (+ upper (* 2.5 PIECE-SIZE)) click (+ 1 pos))]))
     
     (define (map-coordinates x y) 
       (list (map-coordinate (/ PIECE-SIZE 2) (+  (/ PIECE-SIZE 2) (* 2 PIECE-SIZE)) x 0)
             (map-coordinate (/ PIECE-SIZE 2) (+  (/ PIECE-SIZE 2) (* 2 PIECE-SIZE)) y 0)))]
    
    (big-bang state 
              (on-mouse place-checker)
              (stop-when string?)
              (to-draw render))))

;; *** this function permits you to make both legal and illegal moves
;; *** you do not need to use this function and probably should not.  someone thought of a reason
;; *** for it to exist and so i included it.  to be clear, your program is only permitted to 
;; *** make legal moves.
(define (make-hypothetical-move state move)
  (local [(define (update-column turn column current move)
            (cond
              [(empty? column) empty]
              [else
               (cons
                (cond
                  [(= current move)
                   turn]
                  [else (first column)])
                (update-column turn (rest column) (+ 1 current) move))]))
          
          (define (do-move board turn move-x move-y current-x)
            (cond
              [(empty? board) empty]
              [else
               (cons
                (cond
                  [(= move-x current-x) (update-column turn (first board) 0 move-y)]
                  [else (first board)])
                (do-move (rest board) turn move-x move-y (+ 1 current-x)))]))]
    (make-world-state
     (do-move (world-state-position state)
              (world-state-whose-turn state) 
              (first move) (second move) 0)
     (cond
       [(= (world-state-whose-turn state) RED) BLACK]
       [(= (world-state-whose-turn state) BLACK) RED])
     (world-state-settings state)
     (world-state-other-info state))))

;; you will use this function.  it takes as input the move you will make, represented as a list of X Y coordinates
(define (make-move state move) 
  (if (member move (legal-next-moves state))
      (make-hypothetical-move state move)
      state))

;; world-state --> list
;; returns all of the legal moves for the current position
(define (legal-next-moves state)
  (local [
          (define (first-blank column pos)
            (cond
              [(empty? column) (- pos 1)]
              [(not (= (first column) BLANK))
               (- pos 1)]
              [else (first-blank (rest column) (+ 1 pos))]))
          (define (get-moves board-state column)
            (cond
              [(empty? board-state) empty]
              [else
               (local [(define blank (first-blank (first board-state) 0))]
                 (if (< blank 0)
                     (get-moves (rest board-state) (+ 1 column))
                     (cons
                      (list column (first-blank (first board-state) 0))
                      (get-moves (rest board-state) (+ 1 column)))))]))]
    (get-moves (world-state-position state)
               0)))

;; check-win:  world-state --> boolean
;; determines whether the game has ended with a victory for whoever just moved
(define (check-win? state)
  (local [    
          (define (up-column board color x y)
            (if (< y 3)
                false
                (local [(define column (get-nth x board))]
                  (= (get-nth  (- y 1) column)
                     (get-nth  (- y 2) column)
                     (get-nth  (- y 3) column)
                     color))))
          
          (define (right-row board color x y)
            (if (>= x (- COLUMNS 3))
                false
                (= (get-nth y (get-nth (+ 1 x) board))
                   (get-nth y (get-nth (+ 2 x) board))
                   (get-nth y (get-nth (+ 3 x) board))
                   color)))
          
          (define (up-right board color x y)
            (if (or (< y 3)
                    (>= x (- COLUMNS 3)))
                false
                (= (get-nth (- y 1) (get-nth ( + x 1) board))
                   (get-nth (- y 2) (get-nth ( + x 2) board))
                   (get-nth (- y 3) (get-nth ( + x 3) board))
                   color)))
          
          (define (down-right board color x y)
            (if (or (>= y (- ROWS 3))
                    (>= x (- COLUMNS 3)))
                false
                (= (get-nth (+ y 1) (get-nth ( + x 1) board))
                   (get-nth (+ y 2) (get-nth ( + x 2) board))
                   (get-nth (+ y 3) (get-nth ( + x 3) board))
                   color)))
          
          (define (victory? board x y)
            (let
                ([color (get-nth y (get-nth x board))])
              (if (= color BLANK)
                  false
                  (or
                   (up-column board color x y)
                   (right-row board color x y)
                   (up-right board color x y)
                   (down-right board color x y)))))
          
          (define (walk-column board col row)
            (cond
              [(= row ROWS) false]
              [else
               (or
                (victory? board col row)
                (walk-column board col (+ 1 row)))]))
          
          (define (walk-board board col)
            (cond
              [(= col COLUMNS) false]
              [else
               (or (walk-column board col 0)
                   (walk-board board (+ 1 col)))]))]
    (walk-board (world-state-position state) 0)))


(define START-BOARD
  (make-list COLUMNS
             (make-list ROWS BLANK)))
(define start-state
  (make-world-state START-BOARD RED 5 empty))

(define test-state
  (make-world-state
   (list
    (list 0 0 0 0 0 0 0 0 0)  ;; column 0
    (list 0 0 0 0 0 0 0 0 0)  ;; column 1
    (list 0 0 0 0 0 0 0 0 0)  ;; etc
    (list 0 0 0 0 0 0 2 1 2)
    (list 0 0 0 0 0 0 0 2 1)
    (list 0 0 0 0 0 0 0 0 1)
    (list 0 0 0 0 0 0 0 0 1)
    (list 0 0 0 0 0 0 0 0 0))
   BLACK
   5 empty))






