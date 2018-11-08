;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname implementing-rckt) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;These are the invaders definitions
(define INVADER (rectangle 25 25 "solid" "red"))
(define invader-length (image-width INVADER))





;;These are the score,ticks and lives base definitions
(define text-size 28)
(define tick-10 10)





;;These are the columns and rows
(define number-of-columns 9)
(define number-of-rows 4)
(define distance-between-rows (+ invader-length 10))





;;These are the main definitions
(define H 600)
(define W (* 2 invader-length number-of-columns))
(define BG (empty-scene W H))
(define per-invader 5)





;;These are the definitions for the ship
(define invader-move-unit 1)
(define move-ship-unit 5)
(define Y-ship (- H 20))
(define SHIP (rectangle 20 10 "solid" "purple"))






;;These are the main definitions for the bullet
(define bullet-radius 3)
(define BulletSpeed 5)
(define max-bullet 3)
(define BULLET (circle bullet-radius "solid" "black"))





;;These are the world definitions
(define-struct world [ship bullet invader score lives ticks])





;;This is the world render
(define (render w)
  (render-ship (world-ship w)
   (render-bullet (world-bullet w)
    (render-invader (world-invader w)
     (render-score (world-score w)
      (render-lives (world-lives w)
     BG)))))) 





;;This is the score render
(define top-center (make-posn (/ W 2) 20))
(define (render-score score bg)
  (place-image
   (text (number->string score) text-size "black")
   (posn-x top-center)
   (posn-y top-center)
   bg))





;;This is the lives render
(define top-right (make-posn (- W 50)  20))
(define (render-lives lives bg)
  (place-image
   (beside (text "lives:" text-size "black") (text (number->string lives) text-size "black"))
   (posn-x top-right)
   (posn-y top-right)
   bg))





;;This is the first x position for each row for the invaders
(define first-x
  (/ (- W (* 2 (- number-of-columns 1) invader-length)) 2))





;;This is going to be the list creator for the invaders column
(define (create-loi-column x y column)
  (cond
    [(equal? 0 column) empty]
    [else (cons (make-posn x y) (create-loi-column (+ x (* 2 invader-length)) y (- column 1)))]))





;;This is going to be the list creator for the invaders row and column
(define (create-loi-row y row column)
  (cond
    [(equal? 0 row) empty]
    [else (append
           (create-loi-column first-x y column)
           (create-loi-row (+ y distance-between-rows) (- row 1) column))]))





;;This is the list with the posns for invaders
(define list1 (create-loi-row 100 number-of-rows number-of-columns))





;;This is the ship render
(define (render-ship ship-x bg)
  (place-image SHIP ship-x Y-ship bg))





;;This is the bullet
(define (render-bullet lob bg)
  (cond
    [(empty? lob) bg]
    [else (place-image BULLET (posn-x (first lob)) (posn-y (first lob))
                       (render-bullet (rest lob) bg))]))





;;This is the invaders render
(define (render-invader loi bg)
  (cond
    [(empty? loi) bg]
    [else (place-image INVADER (posn-x (first loi)) (posn-y (first loi))
                       (render-invader (rest loi) bg))]))





;;This is the world tick
(define (tick w)
  (make-world (world-ship w)
              (move-barrage (remove-bullet-out (remove-bullet (world-bullet w) (world-invader w))))
              (move-invader (remove-invader (world-invader w) (world-bullet w)) w)
              (update-score (world-score w) (world-invader w) (world-bullet w))
              (world-lives w)
              (world-ticks w)))





;;Thi is the change tick function
(define (change-tick tick w)
  (+ tick 1))





;;This is the move-invaders function
(define (move-invader loi w)
  (cond
    [(empty? loi) loi]
    [(equal? 0 (remainder (world-ticks w) 70)) (cons (make-posn (posn-x (first loi)) (+ (posn-y (first loi)) invader-move-unit)) (move-invader (rest loi) w))]
    [else (move-invader loi w)]))





;;This is the update score function
(define (update-score score loi lob)
  (cond
    [(empty? loi) score]
    [(bullet-hit-invader? (first loi) lob) (update-score (+ score per-invader) (rest loi) lob)]
    [else (update-score score (rest loi) lob)]))





;;This is the move barrage
(define (move-barrage lob)
  (cond
    [(empty? lob) empty]
    [else (cons (make-posn (posn-x (first lob)) (- (posn-y (first lob)) BulletSpeed))
          (move-barrage (rest lob)))]))





;;This is the key event
(define (KE w ke)
  (cond
    [(and (key=? "right" ke) (<  (world-ship w) (- W (/ invader-length 2)))) (make-world (+ (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-score w) (world-lives w) (world-ticks w))]
    [(and (key=? "left" ke) (>  (world-ship w) (/ invader-length 2))) (make-world (- (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-score w) (world-lives w) (world-ticks w))]
    [(key=? " " ke) (make-world (world-ship w) (add-bullet (world-ship w) (world-bullet w)) (world-invader w) (world-score w) (world-lives w) (world-ticks w))]
    [else w])) 





;;This is the add bullet
(define (add-bullet posx lob)
  (cons (make-posn posx Y-ship) lob))





;;This next function will remove from the list the invaders that were hit by the bullets
(define (remove-invader loi lob)
  (cond
    [(empty? loi) empty]
    [else (cond
            [(bullet-hit-invader? (first loi) lob) (remove-invader (rest loi) lob)]
            [else (cons (first loi) (remove-invader (rest loi) lob))])]))





;;This next function will remove the bullets out of bound
(define (remove-bullet-out lob)
  (cond
    [(empty? lob) lob]
    [else (cond
            [(<= (posn-y (first lob)) 0) (remove-bullet-out (rest lob))]
            [else (cons (first lob) (remove-bullet-out (rest lob)))])])) 





;;This is the remove bullet function if it hits an invader
(define (remove-bullet lob loi)
  (cond
    [(empty? lob) empty]
    [else (cond
            [(bullet-hit-invader1? (first lob) loi) (remove-bullet (rest lob) loi)]
            [else (cons (first lob) (remove-bullet (rest lob) loi))])]))





;;This next function will check if the bullet hit the invader
(define (bullet-hit-invader1? bullet loi)
  (cond
    [(empty? loi) #f]
    [else (or (hit1? bullet (first loi)) (bullet-hit-invader1? bullet (rest loi)))]))





;;This next function will check if the bullet hit the invader but this one is used to return the loi after removal
(define (bullet-hit-invader? invader lob)
  (cond
    [(empty? lob) #f]
    [else (or (hit? invader (first lob)) (bullet-hit-invader? invader (rest lob)))]))





;;This is the hit1? function
(define (hit1? bullet invader)
  (cond
    [(<= (distance bullet invader) (/ invader-length 2)) #t]
    [else #f]))





;;This is the hit? function
(define (hit? invader bullet)
  (cond
    [(<= (distance bullet invader) (/ invader-length 2)) #t]
    [else #f]))





;;This is the distance function
(define (distance p1 p2)
  (sqrt
   (+
    (sqr(-(posn-x p1) (posn-x p2)))
    (sqr(-(posn-y p1) (posn-y p2))))))





;;This function will check if the invader reached the bottom of the canvas
(define (invader-reach-bottom? loi)
  (cond
    [(empty? loi) #f]
    [(>= (posn-y (first loi)) (- H (/ invader-length 2))) #t]
    [else (invader-reach-bottom? (rest loi))]))





;;This is the end-when? function
(define (end? w)
  (cond
    [(or (equal? 180 (world-score w)) (invader-reach-bottom? (world-invader w))) #t]
    [else #f]))





;;This is the world-end
(define (world-end w)
  (overlay (text "Game Over" 50 "black") (render (make-world
                                          (/ W 2)
                                          empty
                                          (world-invader w)
                                          (world-score w)
                                          0
                                          (world-ticks w))))) 





;;This is the big bang
(big-bang (make-world (/ W 2) empty list1 0 3 0)
  (to-draw render)
  (on-tick tick)
  (on-key KE)
  (stop-when end? world-end))