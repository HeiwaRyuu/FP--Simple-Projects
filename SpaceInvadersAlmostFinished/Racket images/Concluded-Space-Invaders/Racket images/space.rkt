;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;These are the invaders definitions
(define INVADER (scale 0.65 (bitmap "images/Invader.png") )) 
(define invader-length (* 0.65 (image-width  INVADER)))
(define inv-speed-1 2)
(define inv-speed-2 4)
(define inv-speed-3 6)




;;These are the score,ticks and lives base definitions
(define text-size 28)
(define tick-10 10)





;;These are the columns and rows
(define number-of-columns 9)
(define number-of-rows 4)
(define distance-between-rows (+ invader-length 10))




 
;;This is the chance of ibullet
(define chance-of-ibullet 15)





;;These are the lifebar and platform images definitions
(define GreenLifebar (scale 0.3 (bitmap "images/GreenLifebar.png")))
(define OrangeLifebar (scale 0.3 (bitmap "images/OrangeLifebar.png")))
(define RedLifebar (scale 0.3 (bitmap "images/RedLifebar.png")))
(define EmptyLifebar (scale 0.3 (bitmap "images/EmptyLifebar.png")))
(define Platform (scale 0.5 (bitmap "images/Platform.png")))



;;These are the main definitions
(define H (image-height (scale 0.45 (bitmap "images/Background.jpg"))))
(define W (+ (* 4 invader-length) (* 2 invader-length number-of-columns)))
(define per-invader 5)
(define per-mothership 20)
(define BG (scale 0.45 (bitmap "images/Background.jpg")))
(define INVADER-BULLET (scale 0.5 (bitmap "images/Invadershot.png")))





;;These are the platform's definitions
(define PLATFORM Platform) 
(define platform-initial-dir 'right)
(define platform-initial-position (make-posn (/ W 2) (+ (/ H 2) 50)))
(define platform-move-unit 5)
(define platform-max-lives 3)





;;These are the definitions for the ship
(define move-ship-unit 5) 
(define SHIP (scale 0.75 (bitmap "images/Spaceship.png")))
(define Y-ship (- H (image-height SHIP)))
(define max-ship-lives 3)





;;These are the main definitions for the mothership
(define MOTHERSHIP (scale 0.65 (bitmap "images/Mothership.png")))
(define mothership-width (image-width MOTHERSHIP))
(define mothership-height (image-height MOTHERSHIP))
(define mothership-y (+ 10 mothership-height))
(define mothership-move-unit 5)





;;These are the main definitions for the bullet
(define BulletSpeed 5)
(define max-bullet 3)
(define BULLET (scale 0.5 (bitmap "images/Shipshot.png")))
(define bullet-radius (image-width BULLET))
 


;;These are the world definitions
(define-struct world [ship bullet invader ibullet score lives extra-lives ticks invdir platform platform-lives platform-dir mothership])
(define-struct invader [position direction])





;;This is the world render
(define (render w)
 (render-ship (world-ship w)
  (render-bullet (world-bullet w)
   (render-invader (world-invader w)
    (render-ibullet (world-ibullet w)
     (render-score (world-score w)
      (render-lives (world-lives w) w
       (render-extra-lives (world-extra-lives w) w
        (render-platform (world-platform w) (world-platform-lives w)                
         (render-mothership (world-mothership w)
          BG))))))))))



 
 

;;This is the score render
(define top-center (make-posn (/ W 2) 20))
(define (render-score score bg)
  (place-image
   (text (number->string score) text-size "white")
   (posn-x top-center)
   (posn-y top-center) 
   bg))





;;This is the lives render
(define top-right (make-posn (- W 100)  20))
(define (render-lives lives w bg)
  (place-image
    (beside (text "Your life:" 20 "white")(cond
     [(>= (world-lives w) 3) GreenLifebar]
     [(equal? 2 (world-lives w)) OrangeLifebar]
     [(equal? 1 (world-lives w)) RedLifebar]
     [(equal? 0 (world-lives w)) EmptyLifebar]))
   (posn-x top-right)
   (posn-y top-right)
   bg))





;;This next function will render the extra lifes the player has
  (define (render-extra-lives lives w bg)
  (place-image
    (beside (text "Extra-lives:" 20 "white")(cond
     [(>= (world-extra-lives w) 3) GreenLifebar]
     [(equal? 2 (world-extra-lives w)) OrangeLifebar]
     [(equal? 1 (world-extra-lives w)) RedLifebar]
     [(equal? 0 (world-extra-lives w)) EmptyLifebar]))
   (posn-x top-right)
   (+ (posn-y top-right) 30)
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





;;This is the platform render
(define (render-platform platform platform-lives bg)
  (cond
    [(<= platform-lives 0) (place-image PLATFORM -100 -100 bg)]
    [else (place-image (underlay/xy PLATFORM (+ 5 (* 0.5 (image-width (scale 0.5 PLATFORM)))) 15 (cond
                                      [(>= platform-lives 3) (scale 0.5 GreenLifebar)]
                                      [(equal? platform-lives 2) (scale 0.5 OrangeLifebar)]
                                      [(equal? platform-lives 1) (scale 0.5 RedLifebar)]
                                      [else (scale 0.5 EmptyLifebar)])) (posn-x platform) (posn-y platform) bg)]))


 

;;This is the render mothership
(define (render-mothership mothership-x bg)
  (place-image MOTHERSHIP mothership-x mothership-y bg))





;;This is the render I-bullet function
(define (render-ibullet loib bg)
  (cond
    [(empty? loib) bg]
    [else (place-image INVADER-BULLET (posn-x (first loib)) (posn-y (first loib))
                       (render-ibullet (rest loib) bg))]))



 

;;This is the world on-tick function
(define (tick w)
  (make-world (world-ship w)
              (move-barrage (remove-bullet-mothership (remove-bullet-out (remove-bullet (world-bullet w) (world-invader w))) (world-mothership w)))
              (move-invader (remove-invader (world-invader w) (world-bullet w)) w)
              (move-ibullet (remove-ibullet-ground (remove-bullet-platform (remove-ibullet-ship (add-bullet-loi (generate-posn (restrict-generation (world-invader w)) (random 9)) (world-ibullet w) (world-ticks w)) (world-ship w)) (world-platform w))))
              (update-score (world-score w) (world-invader w) (world-bullet w) (world-mothership w))
              (update-lives (world-lives w) (world-ibullet w) (world-ship w) (world-bullet w) (world-mothership w))
              (update-extra-lives (world-extra-lives w) (world-ibullet w) (world-ship w) (world-bullet w) (world-mothership w) w)
              (+ 1 (world-ticks w))
              (cond
                [(invader-hit-right? (world-invader w)) 'left]
                [(invader-hit-left? (world-invader w)) 'right]
                [else (world-invdir w)])
              (cond
                [(> (world-platform-lives w) 0) (move-platform (world-platform w) (world-platform-dir w))]
                [else (world-platform w)])
              (update-platform-lives (world-platform-lives w) (world-ibullet w) (world-platform w))
              (update-platform-dir (world-platform w) (world-platform-dir w))
              (cond 
                [(>= (world-ticks w) 75) (move-mothership (remove-mothership (world-mothership w) (world-bullet w)))]
                [else (world-mothership w)])))





;;This functions will remove the invader bullets that hit the platform
(define (remove-bullet-platform loib platform)
  (cond
    [(empty? loib) loib]
    [(bullet-hit-platform? (first loib) platform) (remove-bullet-platform (rest loib) platform)]
    [else (cons (first loib) (remove-bullet-platform (rest loib) platform))]))





;;This function will update the platform lives
(define (update-platform-lives P-lives loib platform)
  (cond
    [(platform-hit? loib platform) (- P-lives 1)]
    [else P-lives]))





;;This function will check if any ibullet hit the platform
(define (platform-hit? loib platform)
  (cond
    [(empty? loib) #f]
    [else (or (bullet-hit-platform? (first loib) platform) (platform-hit? (rest loib) platform))]))





;;This function will check if a single bullet hit the platform
(define (bullet-hit-platform? ibullet platform)
  (cond
    [(and (<= (dist-to-plat-y (posn-y ibullet) (posn-y platform)) (+ (/ (image-height PLATFORM) 2) (/ (image-height INVADER-BULLET) 2)))
           (<= (dist-to-plat-x (posn-x ibullet) (posn-x platform)) (/ (image-width PLATFORM) 2)))
     #t]
    [else #f]))





;;This is the dist to plat y
(define (dist-to-plat-y bullet-y platform-y)
  (cond
    [(< (- bullet-y platform-y) 0) (- platform-y bullet-y)]
    [else (- bullet-y platform-y)]))





;;This is the dist to plat x
(define (dist-to-plat-x bullet-x platform-x)
  (cond
    [(< (- bullet-x platform-x) 0) (- platform-x bullet-x)]
    [else (- bullet-x platform-x)]))





;;This function will update the platform moving-direction
(define (update-platform-dir platform plat-dir)
  (cond
    [(platform-hit-right? platform) 'left]
    [(platform-hit-left? platform) 'right]
    [else plat-dir]))





;;This is the move-platform function
(define (move-platform platform platform-dir)
  (cond
    [(and (symbol=? platform-dir 'right) (not (platform-hit-right? platform))) (make-posn (+ (posn-x platform) platform-move-unit) (posn-y platform))]
    [(and (symbol=? platform-dir 'left) (not (platform-hit-left? platform))) (make-posn (- (posn-x platform) platform-move-unit) (posn-y platform))]
    [else (make-posn (posn-x platform) (+ (posn-y platform) (* 5 platform-move-unit)))]))




 
;;These functions below will check if the platform hit the right or left corners
;;This is the platform hit right
(define (platform-hit-right? platform)
  (cond
    [(>= (posn-x platform) (- W (/ (image-width PLATFORM) 2))) #t]
    [else #f]))





;;This is the platform hit left
(define (platform-hit-left? platform)
  (cond
    [(<= (posn-x platform) (/ (image-width PLATFORM) 2)) #t]
    [else #f]))





;;This function will restrict from where a posn should be generated
(define (restrict-generation loi)
  (cond
    [(<= (length loi) 9) loi]
    [else (restrict-generation (rest loi))]))





;;This is the add invasor bullet function
(define (generate-posn loi index)
  (cond
    [(<= index (- 10 (length loi))) (first loi)]
    [else (generate-posn (rest loi) (- index 1))]))





;;This is the function adding a bullet to the loib
(define (add-bullet-loi posn loib ticks)
  (cond
    [(equal? (remainder ticks chance-of-ibullet) 0) (cons posn loib)]
    [else loib]))





;;This is the move ibullet function
(define (move-ibullet loib)
  (cond
    [(empty? loib) loib]
    [else (cons (make-posn (posn-x (first loib)) (+ (posn-y (first loib)) BulletSpeed))
                (move-ibullet (rest loib)))]))




  
;;This function will remove the ibullet from screen if it hits the ground
(define (remove-ibullet-ground loib)
  (cond
    [(empty? loib) loib]
    [(ibullet-hit-ground? (first loib)) (remove-ibullet-ground (rest loib))]
    [else (cons (first loib) (remove-ibullet-ground (rest loib)))]))




  
;;This function will remove the ibullet from screen if it hits the ship
(define (remove-ibullet-ship loib ship)
  (cond
    [(empty? loib) loib]
    [(ship-hit? (first loib) ship Y-ship) (remove-ibullet-ship (rest loib) ship)]
    [else (cons (first loib) (remove-ibullet-ship (rest loib) ship))]))  





;;This function will check if any ibullet hit ship
(define (any-ibullet-hit-ship? loib ship)
  (cond
    [(empty? loib) #f]
    [else (or (ship-hit? (first loib) ship Y-ship) (any-ibullet-hit-ship? (rest loib) ship))]))





;;This function will calculate the distance between the ibullet posn and the ship
(define (ship-hit? bullet ship-x ship-y)
  (cond
    [(and (<= (dist-y (posn-y bullet) ship-y) (/ (image-height SHIP) 2))
          (<= (dist-x (posn-x bullet) ship-x) (/ (image-width SHIP) 2))) #t]
    [else #f]))




  
;;This function will check if the ibullet hit the gorund
(define (ibullet-hit-ground? ibullet)
  (cond
    [(>= (posn-y ibullet) (+ H (/ (image-height INVADER-BULLET) 2))) #t]
    [else #f]))




  
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
    [else (or (mothership-hit? (first lob) mothership-x mothership-y) (bullet-hit-mothership? (rest lob) mothership-x))])) 
 




;;This function will calculate the distance between the bullet posn and the mothership
(define (mothership-hit? bullet mothership-x mothership-y)
  (cond
    [(and (<= (dist-y (posn-y bullet) mothership-y) (/ (image-height MOTHERSHIP) 2))
          (<= (dist-x (posn-x bullet) mothership-x) (/ (image-width MOTHERSHIP) 2))) #t]
    [else #f]))





;;This is the move-invaders function
(define (move-invader loi w)
  (cond
    [(empty? loi) loi]
    [(and (symbol=? 'right (world-invdir w)) (equal? 0 (remainder (world-ticks w) 1)) (not (invader-hit-right? loi))) (cons (make-posn (+ (cond
    [(and (>= (length (world-invader w)) 25) (<= (length (world-invader w)) 36)) inv-speed-1]
    [(and (>= (length (world-invader w)) 15) (<= (length (world-invader w)) 24)) inv-speed-2]
    [(and (>= (length (world-invader w)) 0) (<= (length (world-invader w)) 14)) inv-speed-3]) (posn-x (first loi))) (posn-y (first loi))) (move-invader (rest loi) w))]
    [(and (symbol=? 'left (world-invdir w)) (equal? 0 (remainder (world-ticks w) 1)) (not (invader-hit-left? loi))) (cons (make-posn (- (posn-x (first loi)) (cond
    [(and (>= (length (world-invader w)) 25) (<= (length (world-invader w)) 36)) inv-speed-1]
    [(and (>= (length (world-invader w)) 15) (<= (length (world-invader w)) 24)) inv-speed-2]
    [(and (>= (length (world-invader w)) 0) (<= (length (world-invader w)) 14)) inv-speed-3])) (posn-y (first loi))) (move-invader (rest loi) w))]
    [(invader-hit-corner? loi) (update-invaders loi)]
    [else loi]))

 



;;This function will update every single invader in the list once a invader hits a corner
(define (update-invaders loi)
  (cond
    [(empty? loi) loi]
    [else (cons (make-posn (posn-x (first loi)) (+ (posn-y (first loi)) (* 10 inv-speed-1))) (update-invaders (rest loi)))]))





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
(define (update-lives lives loib ship-x lob mothership-x)
  (cond
    [(bullet-hit-mothership? lob mothership-x) (+ 1 lives)]
    [(any-ibullet-hit-ship? loib ship-x) (- lives 1)]
    [else lives]))





;;This next function will update the amount of extra-lives the player has
(define (update-extra-lives lives loib ship-x lob mothership-x w)
  (cond
    [(and (>= (world-lives w) 3) (<= (world-extra-lives w) 3) (bullet-hit-mothership? lob mothership-x)) (+ 1 lives)]
    [(and (> lives 0) (any-ibullet-hit-ship? loib ship-x)) (- lives 1)]
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
    [(and (key=? "right" ke) (<  (world-ship w) (- W (/ invader-length 2)))) (make-world (+ (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-ibullet w) (world-score w) (world-lives w) (world-extra-lives w) (world-ticks w) (world-invdir w) (world-platform w) (world-platform-lives w) (world-platform-dir w) (world-mothership w))]
    [(and (key=? "left" ke) (>  (world-ship w) (/ invader-length 2))) (make-world (- (world-ship w) move-ship-unit) (world-bullet w) (world-invader w) (world-ibullet w) (world-score w) (world-lives w) (world-extra-lives w) (world-ticks w) (world-invdir w) (world-platform w) (world-platform-lives w) (world-platform-dir w) (world-mothership w))]
    [(and (key=? " " ke) (< (length (world-bullet w)) max-bullet)) (make-world (world-ship w) (add-bullet (world-ship w) (world-bullet w)) (world-invader w) (world-ibullet w) (world-score w) (world-lives w) (world-extra-lives w) (world-ticks w) (world-invdir w) (world-platform w) (world-platform-lives w) (world-platform-dir w) (world-mothership w))]
    [else w])) 





;;This is the add bullet
(define (add-bullet posx lob)
  (cons (make-posn posx Y-ship) lob))




 
;;This next function will remove from the list the invaders that were hit by the ship bullets
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
    [else (or (hit? bullet (first loi)) (bullet-hit-invader1? bullet (rest loi)))]))





;;This next function will check if the bullet hit the invader but this one is used to return the loi after removal
(define (bullet-hit-invader? invader lob)
  (cond
    [(empty? lob) #f]
    [else (or (hit? invader (first lob)) (bullet-hit-invader? invader (rest lob)))]))





;;This is going to be thw reformulation for the distance check (checking if a bullet hit an invader
(define (hit? bullet invader)
  (cond
    [(and (<= (dist-y (posn-y bullet) (posn-y invader)) (/ (image-height INVADER) 2))
          (<= (dist-x (posn-x bullet) (posn-x invader)) (/ (image-width INVADER) 2))) #t]
    [else #f]))





;;This is the definition for dist-y
(define (dist-y bullet-y invader-y)
  (cond
    [(< (- bullet-y invader-y) 0) (- invader-y bullet-y)]
    [else (- bullet-y invader-y)]))





;;This is the definition for dist-x
(define (dist-x bullet-x invader-x)
  (cond
    [(< (- bullet-x invader-x) 0) (- invader-x bullet-x)]
    [else (- bullet-x invader-x)]))





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
  (cond
    [(equal? 0 (length (world-invader w))) (overlay (underlay/xy (text "You Win!" 50 "white") -35 50(beside (text "Your score was :" 30 "white")
                                                                    (text (number->string (world-score w)) 30 "white")))
                                                (render (make-world
                                                  (/ W 2)
                                                  empty
                                                  (world-invader w)
                                                  empty
                                                  (world-score w)
                                                  (cond
                                                    [(or (invader-hit-ship? (world-invader w) (world-ship w)) (invader-reach-bottom? (world-invader w))) 0]
                                                    [else (world-lives w)])
                                                  (cond
                                                    [(or (invader-hit-ship? (world-invader w) (world-ship w)) (invader-reach-bottom? (world-invader w))) 0]
                                                    [else (world-extra-lives w)])
                                                  (world-ticks w)
                                                  (world-invdir w)
                                                  (world-platform w)
                                                  (world-platform-lives w)
                                                  (world-platform-dir w)
                                                  (world-mothership w))))]
    [else                                  (overlay (underlay/xy (text "Game Over" 50 "white") -35 50(beside (text "Your score was :" 30 "white")
                                                                    (text (number->string (world-score w)) 30 "white")))
                                               (render (make-world
                                                  (/ W 2)
                                                  empty
                                                  (world-invader w)
                                                  empty
                                                  (world-score w)
                                                  (cond
                                                    [(or (invader-hit-ship? (world-invader w) (world-ship w)) (invader-reach-bottom? (world-invader w))) 0]
                                                    [else (world-lives w)])
                                                  (cond
                                                    [(or (invader-hit-ship? (world-invader w) (world-ship w)) (invader-reach-bottom? (world-invader w))) 0]
                                                    [else (world-extra-lives w)])
                                                  (world-ticks w)
                                                  (world-invdir w)
                                                  (world-platform w)
                                                  (world-platform-lives w)
                                                  (world-platform-dir w)
                                                  (world-mothership w))))]))


;;This is the initial world
(define initial-world
  (make-world (/ W 2) empty list1 empty 0 max-ship-lives 0 0 'right platform-initial-position platform-max-lives platform-initial-dir (* -1 mothership-width)))





;;This is the big bang
(big-bang initial-world
  (to-draw render)
  (on-tick tick)
  (on-key KE)
  (stop-when end? world-end))