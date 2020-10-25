
;scheme program to approximate the value of Pi

;*1 distance-to-origin procedure: takes two variables that represent a point,
; and returns its distance to the origin

(define distance-to-origin
  (lambda (x y)
    (sqrt (+ (* x x) (* y y)))))

;*2 make-points procedure: takes a positive integer and returns n random points

(define make-points
  (lambda (n)
    (cond
      [(= n 0) '()]
      [else (cons (cons (random 1.0 ) (cons (random 1.0) '()))
                                                (make-points (- n 1)))])))


;*3 count-in-circle procedure: takes a list of points and returns 
; the number of points that are inside the first quadrant of the circle of radius 1

(define count-in-circle
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(<= (distance-to-origin (caar ls) (car (cdr (car ls)))) 1)
                                              (+ 1 (count-in-circle (cdr ls)))]
      [else (+ 0 (count-in-circle (cdr ls)))])))


;*4 calculate-pi procedure: n  checks to see if the randomly generated sets of points
; created from make-points is within the circle and adds one for each set. The value is 
; then multiplied by 4 to approximate the number of sets that would be in a full circle. 
; The procedure is then divided by n, because n is the total possible number of sets of 
; points that could be in a circle. The ratio created by this procedure approximates the
; value of pi.

(define calculate-pi
  (lambda(n)
    (/ (* 4.0 (count-in-circle (make-points n))) n)))