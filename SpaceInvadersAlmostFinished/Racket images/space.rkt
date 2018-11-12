;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;These are the invaders definitions
(define INVADER (scale 0.65 (bitmap "images/Invader.png") )) 
(define invader-length (* 0.65 (image-width  INVADER)))





;;These are the score,ticks and lives base definitions
(define text-size 28)
(define tick-10 10)





;;These are the columns and rows
(define number-of-columns 9)
(define number-of-rows 4)
(define distance-between-rows (+ invader-length 10))





;;These are the main definitions
(define H  (image-height (scale 0.45 (bitmap "images/Background.jpg"))))
(define W (+ (* 4 invader-length) (* 2 invader-length number-of-columns)))
(define per-invader 5)
(define per-mothership 20)
(define chance-of-bullet 3)
(define BG (scale 0.45 (bitmap "images/Background.jpg")))




;;These are the definitions for the ship
(define invader-move-unit 2)
(define move-ship-unit 5)
(define SHIP (scale 0.75 (bitmap "images/Spaceship.png")))
(define Y-ship (- H (image-height SHIP))) 





;;These are the main definitions for the mothership
(define MOTHERSHIP (scale 0.65 (bitmap "images/Mothership.png")))
(define mothership-width (image-width MOTHERSHIP))
(define mothership-height (image-height MOTHERSHIP))
(define mothership-y (+ 10 mothership-height))
(define mothership-move-unit 5)





;;These are the main definitions for the bullet
(define BulletSpeed 5)
(define max-bullet 3)
(define BULLET (scale 0.5 (bitmap "images/Shot.png")))
(define INVADER-BULLET (circle 3 "solid" "red"))
(define bullet-radius (image-width BULLET))



;;These are the world definitions
(define-struct world [ship bullet invader score lives ticks invdir mothership])
(define-struct invader [position direction])





;;This is the world render
(define (render w)
 (render-ship (world-ship w)
  (render-bullet (world-bullet w)
   (render-invader (world-invader w)
  ;;(render-ibullet (world-ibullet w)
     (render-score (world-score w)
      (render-lives (world-lives w)
       (render-mothership (world-mothership w)
       BG))))))) 






;;This is the score render
(define top-center (make-posn (/ W 2) 20))
(define (render-score score bg)
  (place-image
   (text (number->string score) text-size "white")
   (posn-x top-center)
   (posn-y top-center)
   bg))





;;This is the lives render
(define top-right (make-posn (- W 50)  20))
(define (render-lives lives bg)
  (place-image
   (beside (text "lives:" text-size "white") (text (number->string lives) text-size "white"))
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





;;This is the render mothership
(define (render-mothership mothership-x bg)
  (place-image MOTHERSHIP mothership-x mothership-y bg))





;;This is the render I-bullet function
;;(define (render-ibullet loib bg)
;;  (cond
;;    [(empty? loib) bg]
;;    [else (place-image INVADER-BULLET (posn-x (first loib)) (posn-y (first loib))
;;                       (render-ibullet (rest loib) bg))]))





;;This is the world on-tick function
(define (tick w)
  (make-world (world-ship w)
              (move-barrage (remove-bullet-mothership (remove-bullet-out (remove-bullet (world-bullet w) (world-invader w))) (world-mothership w)))
              (move-invader (remove-invader (world-invader w) (world-bullet w)) w)
            ;;(add-ibullet (world-invader w) (world-ibullet w))
              (update-score (world-score w) (world-invader w) (world-bullet w) (world-mothership w))
              (update-lives (world-lives w) (world-bullet w) (world-mothership w))
              (+ 1 (world-ticks w))
              (cond
                [(invader-hit-right? (world-invader w)) 'left]
                [(invader-hit-left? (world-invader w)) 'right]
                [else (world-invdir w)])
              (cond
                [(>= (world-ticks w) 75) (move-mothership (remove-mothership (world-mothership w) (world-bullet w)))]
                [else (world-mothership w)])))





;;This is the add invasor bullet function
;;(define (add-ibullet loi loib)
;;  (cond
;;    [(< (random 100) chance-of-bullet) (cons (random (length loi)) loib))]))





;;This is the move motehrship function
(define (move-mothership mothership-x)
  (cond
    [(<= mothership-x (* 2 W)) (+ mothership-x mothership-move-unit)]
    [(> mothership-x (* 2 W)) (* -1 mothership-width)]))





;;This next function will remove the bullet if it hit the mothership
(define (remove-bullet-mothership lob mothership-x)
  (cond
    [(empty? lob) lob]
    [(bullet-hit-mothership? lob mothership-x) (remove-bullet-mothership (rest lob) mothership-x)]
    [else (cons (first lob) (remove-bullet-mothership (rest lob) mothership-x))]))



 

;;This next function will take the mothership out of screen if it gets hit by a bullet
(define (remove-mothership mothership-x lob)
  (cond
    [(bullet-hit-mothership? lob mothership-x) (+ W mothership-width)]
    [else mothership-x]))





;;This function will check if any ship bullet hit the mothership
(define (bullet-hit-mothership? lob mothership-x)
  (cond
    [(empty? lob) #f]
    [else (or (mothership-hit? (first lob) mothership-x) (bullet-hit-mothership? (rest lob) mothership-x))])) 





;;This next function will check if a bullet hit the mothership
(define (mothership-hit? bullet mothership-x)
  (cond
    [(<= (distance-Mothership bullet mothership-x mothership-y) (/ mothership-width 2)) #t]
    [else #f]))
 




;;This function will calculate the distance between the bullet posn and the ship posn
(define (distance-Mothership bullet mship-x mship-y)
  (sqrt
   (+ (sqr
    (- (posn-x bullet) mship-x))
      (sqr
       (- (posn-y bullet) mship-y)))))





;;This is the move-invaders function
(define (move-invader loi w)
  (cond
    [(empty? loi) loi]
    [(and (symbol=? 'right (world-invdir w)) (equal? 0 (remainder (world-ticks w) 1)) (not (invader-hit-right? loi))) (cons (make-posn (+ invader-move-unit (posn-x (first loi))) (posn-y (first loi))) (move-invader (rest loi) w))]
    [(and (symbol=? 'left (world-invdir w)) (equal? 0 (remainder (world-ticks w) 1)) (not (invader-hit-left? loi))) (cons (make-posn (- (posn-x (first loi)) invader-move-unit) (posn-y (first loi))) (move-invader (rest loi) w))]
    [(invader-hit-corner? loi) (update-invaders loi)]
    [else loi]))

 



;;This function will update every single invader in the list once a invader hits a corner
(define (update-invaders loi)
  (cond
    [(empty? loi) loi]
    [else (cons (make-posn (posn-x (first loi)) (+ (posn-y (first loi)) (* 10 invader-move-unit))) (update-invaders (rest loi)))]))





;;This is the hit check for the invaders for the right corner
(define (invader-hit-right? loi)
  (cond
    [(empty? loi) #f]
    [(>= (posn-x (first loi)) (- W (/ invader-length 2))) #t]
    [else (invader-hit-right? (rest loi))]))





;;This is the hit check for the invaders for the left corner
(define (invader-hit-left? loi)
  (cond
    [(empty? loi) #f]
    [(<= (posn-x (first loi)) (/ invader-length 2)) #t]
    [else (invader-hit-left? (rest loi))]))





;;This function will check if any of the invaders hit any of the corners
(define (invader-hit-corner? loi)
  (or
   (invader-hit-right? loi)
   (invader-hit-left? loi)
   ))





;;This is the update score function
(define (update-score score loi lob mothership-x)
  (cond
    [(empty? loi) score]
    [(bullet-hit-mothership? lob mothership-x) (update-score (+ score per-mothership) (rest loi) lob mothership-x)]
    [(bullet-hit-invader? (first loi) lob) (update-score (+ score per-invader) (rest loi) lob mothership-x)]
    [else (update-score score (rest loi) lob mothership-x)]))





;;This next function will update the life increasing it by one if a bullet hit the mothership
(define (update-lives lives lob mothership-x)
  (cond
    [(bullet-hit-mothership? lob mothership-x) (+ 1 lives)]
    [else lives]))





;;This is the move barrage
(define (move-barrage lob)
  (cond
    [(empty? lob) lob]
    [else (cons (make-posn (posn-x (first lob)) (- (posn-y (first lob)) BulletSpeed))
                (move-barrage (rest lob)))]))





;;This is the key event
(define (KE w ke)
  (cond
    [(and (key=? "right" ke) (<  (world-ship w) (- W (/ invader-length 2)))) (make-world (+ (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-score w) (world-lives w) (world-ticks w) (world-invdir w) (world-mothership w))]
    [(and (key=? "left" ke) (>  (world-ship w) (/ invader-length 2))) (make-world (- (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-score w) (world-lives w) (world-ticks w) (world-invdir w) (world-mothership w))]
    [(and (key=? " " ke) (< (length (world-bullet w)) max-bullet)) (make-world (world-ship w) (add-bullet (world-ship w) (world-bullet w)) (world-invader w) (world-score w) (world-lives w) (world-ticks w) (world-invdir w) (world-mothership w))]
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
    [(>= (posn-y (first loi)) (- (- H (/ invader-length 2)) 20)) #t]
    [else (invader-reach-bottom? (rest loi))]))





;;This function will check if an invader hit the ship
(define (invader-hit-ship? loi ship-x)
  (cond
    [(empty? loi) #f]
    [(<= (distance-invader-ship (first loi) ship-x (-  Y-ship (/ (image-height SHIP) 2))) (/ invader-length 2)) #t]
    [else (invader-hit-ship? (rest loi) ship-x)]))





;;This next function will calculate the distance between the ship and a invader
(define (distance-invader-ship invader ship-x ship-y)
  (sqrt
   (+
    (sqr (- (posn-x invader) ship-x))
    (sqr (- (posn-y invader) ship-y)))))





;;This is the end-when? function
(define (end? w)
  (cond
    [(or (invader-hit-ship? (world-invader w) (world-ship w))(empty? (world-invader w)) (invader-reach-bottom? (world-invader w)) (equal? (world-lives w) 0)) #t]
    [else #f]))




 
;;This is the world-end
(define (world-end w)
  (overlay (underlay/xy (text "Game Over" 50 "white") -35 50(text "Press R to start a new game" 30 "white")) (render (make-world
                                                  (/ W 2)
                                                  empty
                                                  (world-invader w)
                                                  (world-score w)
                                                  0
                                                  (world-ticks w)
                                                  (world-invdir w)
                                                  (world-mothership w))))) 
  



;;This is the initial world
(define initial-world
  (make-world (/ W 2) empty list1 0 3 0 'right (* -1 mothership-width)))





;;This is the big bang
(big-bang initial-world
  (to-draw render)
  (on-tick tick)
  (on-key KE)
  (stop-when end? world-end))