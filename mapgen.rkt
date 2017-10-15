#lang racket/gui


(define col-water (make-color 0 134 206))
(define col-land (make-color 195 153 111))
(define col-black (make-color 0 0 0))
(define col-white (make-color 255 255 255))
(define grid-res 80)
(define grid-type 'square)

(struct Point (x y edges) #:transparent)
(struct Edge (x1 y1 x2 y2) #:transparent)

(struct Space ([size #:mutable] [res #:mutable] [type #:mutable] 
                                [points #:mutable] [edges #:mutable]) #:transparent)
(define the-space (Space 0 0 'square (make-hash '()) (make-hash '())))

(define (make-point x y edges)
  (hash-ref! 
   (Space-points the-space) 
   (list x y)
   (Point x y edges)))
(define (make-edge x1 y1 x2 y2)
  (hash-ref! 
   (Space-edges the-space) 
   (list x1 y1 x2 y2)
   (Edge x1 y1 x2 y2)))

(define (rebuild-points s-res unit-size)
  (set-Space-points! the-space (make-hash '()))
  (set-Space-edges! the-space (make-hash '()))
  (apply append (build-list 
                 s-res 
                 (λ (n) 
                   (build-list 
                    s-res 
                    (λ (k) 
                      (make-point
                       (* (+ k 1/2) unit-size)
                       (* (+ n 1/2) unit-size)
                       (list
                        (make-edge 
                         (* k unit-size) (* n unit-size)
                         (* k unit-size) (* (+ n 1) unit-size))
                        (make-edge 
                         (* k unit-size) (* n unit-size)
                         (* (+ k 1) unit-size) (* n unit-size))
                        (make-edge 
                         (* k unit-size) (* (+ n 1) unit-size)
                         (* (+ k 1) unit-size) (* (+ n 1) unit-size))
                        (make-edge 
                         (* (+ k 1) unit-size) (* n unit-size)
                         (* (+ k 1) unit-size) (* (+ n 1) unit-size))
                        ))))))))
(define (place-square g-size g-res)
  (let* ([s-res (ceiling (inexact->exact (sqrt g-res)))]
         [unit-size (/ g-size s-res)])
    (if 
     (and (equal? g-size (Space-size the-space))
          (equal? g-res (Space-res the-space))
          (equal? grid-type (Space-type the-space)))
     (hash-values (Space-points the-space))
     (rebuild-points s-res unit-size)
     )))

(define (place-hex g-size g-res)
  (place-square g-size g-res)) ; ha ha

(define (place-gridpoints grid-size)
  (if (equal? grid-type 'square)
      (place-square grid-size grid-res)
      (place-hex grid-size grid-res)))


(define (draw-things dc)
  (let*-values
      ([(width height) (send dc get-size)] 
       [(smaller-bound) (if (< width height) width height)]
       [(diff-x) (if (< width height) 0 (/ (- width height) 2))]
       [(diff-y) (if (< height width) 0 (/ (- height width) 2))])
    (send dc set-background col-water)
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen col-black 2 'solid)
    (send dc set-brush col-water 'solid)
    (send dc draw-rectangle diff-x diff-y smaller-bound smaller-bound)
    (send dc set-pen col-white 2 'solid)
    (for-each 
     (λ (p) 
       (send dc draw-line 
             (+ diff-x (Point-x p)) (+ diff-y (Point-y p)) 
             (+ diff-x (Point-x p)) (+ diff-y (Point-y p)))
       )
     (place-gridpoints smaller-bound))
    (send dc set-pen col-black 1 'solid)
    (for-each 
     (λ (e) 
       (send dc draw-line 
             (+ diff-x (Edge-x1 e)) (+ diff-y (Edge-y1 e)) 
             (+ diff-x (Edge-x2 e)) (+ diff-y (Edge-y2 e)))
       )
     (hash-values (Space-edges the-space)))))

(define base-frame (new frame%
                        [label "Map Generator"]
                        [width 800]
                        [height 600]))
(send base-frame create-status-line)
(define (set-status str)
  (send base-frame set-status-text str))

(define base-panel (new vertical-panel% [parent base-frame]))
(define settings-panel (new horizontal-panel% [parent base-panel] [min-height 100] [stretchable-height #f]))
(define canvas (new canvas% [parent base-panel] [min-width 200] [min-height 200]
                    [paint-callback
                     (λ (canvas dc)
                       (draw-things dc))]))

(send base-frame show #t)
