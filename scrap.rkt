;;; This is where I put functions that I don't need in my main file
;;; I'm too scared to fully get rid of them should I ever come to need them

;;; real-modulo: Stolen graciously from Racket's GitHub
(define (real-modulo [x real?] [y real?]) real?
  (cond [(not (real? x)) (raise-type-error 'real-modulo "real number" 0 x y)]
        [(not (real? y)) (raise-type-error 'real-modulo "real number" 1 x y)]
        [else (- x (* y (floor (/ x y))))]))

;;; This megalith of a function was my first attempt at properly listing scales
;;; This is just for reference, it does not work
; (define (whole-note-scale root int)
;   (let* ([rootletter (substring root 0 1)]
;          [pre-scale (append (map (lambda (x) (string-append x "1"))
;                                  (append (list root) (cdr (filter (lambda (x) (= 1 (string-length x)))
;                                                                   (better-take (member rootletter
;                                                                                        (append (sharp-or-flat ; root)
;                                                                                                (sharp-or-flat ; root)))
;                                                                                12)))))
;                             (string-append root "2"))])
;     (let loop ([step 0]
;                [pre-scale pre-scale]
;                [note1 (list-ref pre-scale 0)]
;                [note2 (list-ref pre-scale 1)])
;       (cond
;        ((= (length int) step) #f)
;        (else
;         (cond
;          ((zero? (- (list-ref int step) (distance note1 note2)))
;           (cons (substring note2 0 1)
;                 (loop (add1 step)
;                       pre-scale
;                       (list-ref pre-scale (add1 step))
;                       (list-ref pre-scale (+ 2 step)))))
;          ((= 1 (- (list-ref int step) (distance note1 note2)))
;           (cons (string-append (substring note2 0 1) "#")
;                 (loop (add1 step)
;                       pre-scale
;                       (list-ref pre-scale (add1 step))
;                       (list-ref pre-scale (+ 2 step)))))
;          ((= -1 (- (list-ref int step) (distance note1 note2)))
;           (cons (string-append (substring note2 0 1) "b")
;                 (loop (add1 step)
;                       pre-scale
;                       (list-ref pre-scale (add1 step))
;                       (list-ref pre-scale (+ 2 step)))))
;          (else (loop step
;                      pre-scale
;                      note1
;                      (list-ref pre-scale (+ 2 step))))
;          ))))
;     #t))

(define (draw-dot-color num rgb)
  (overlay
   (text
    (if (string->number num) (string->number num) num) 10 "black")
   (circle 10 "solid" (apply make-color (map round rgb)))))

;;; hsv-to-rgb: Inputs h s v, returns (r g b). Graciously stolen from Racket's GitHub
(define (hsv->rgb h s v)
  (define c (* v s))
  (let ([h (/ (real-modulo h 360) 60)])
    (define x (* c (- 1 (abs (- (real-modulo h 2) 1)))))
    (define-values (r g b)
      (cond [(and (0 . <= . h) (h . < . 1)) (values c x 0)]
            [(and (1 . <= . h) (h . < . 2)) (values x c 0)]
            [(and (2 . <= . h) (h . < . 3)) (values 0 c x)]
            [(and (3 . <= . h) (h . < . 4)) (values 0 x c)]
            [(and (4 . <= . h) (h . < . 5)) (values x 0 c)]
            [(and (5 . <= . h) (h . < . 6)) (values c 0 x)]))
    (define m (- v c))
    (list (* 255 (+ r m))
          (* 255 (+ g m))
          (* 255 (+ b m)))))

(define (rainbow-draw-on-scene pict)
  (cond
   ((empty? pict) (draw-scene))
   (else
    (place-image (draw-dot-color (first (car pict)) (last (car pict)))
                 (place-at-fret (third (car pict)))
                 (place-at-string (second (car pict)))
                 (rainbow-draw-on-scene (cdr pict))))))

(define (place-scale note scale base)
  (map (lambda (note fret) (list note fret)) (list-scale note scale) (for/list ([i (in-range (length scale))]) (+ base (apply + (take scale i))))))

(define (rainbow-draw-scales root int strings [fretmax 19] [fretmin 0])
  (for*/list ([s (in-range 1 (add1 (length strings)))]
              [f (in-range fretmin (add1 fretmax))]
              #:when (is-in-scale? (get-note-at s f strings) (list-scale root int)))
    (list (get-note-at s f strings) s f (rainbow-note root int (retrieve-note (get-note-at s f strings))))))

(define (rainbow-note root int note)
  (let ([leap (/ 360 (length int))])
    (hsv->rgb (* leap (get-position (list-scale root int) note)) 1 1)))

;;; To place something at a specific fret/string
(define (place-at string fret)
  (list (place-at-string string) (place-at-fret fret)))
