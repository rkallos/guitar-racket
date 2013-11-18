#lang racket

(require "notes.rkt")
(require "misc.rkt")

(define keys (list '("Cb" "ab")
                   '("Gb" "eb")
                   '("Db" "bb")
                   '("Ab" "f")
                   '("Eb" "c")
                   '("Bb" "g")
                   '("F" "d")
                   '("C" "a")
                   '("G" "e")
                   '("D" "b")
                   '("A" "f#")
                   '("E" "c#")
                   '("B" "g#")
                   '("F#" "d#")
                   '("C#" "a#")))

(define major-keys
  (for/list ([i (in-range (length keys))])
    (list-ref (list-ref keys i) 0)))

(define minor-keys
  (for/list ([i (in-range (length keys))])
    (list-ref (list-ref keys i) 1)))

(define base-scale (list "C" "D" "E" "F" "G" "A" "B"))

(define (is-valid-key? key)
  (cond
   ((member key major-keys) #t)
   ((member key minor-keys) #t)
   (else (error "is-valid-key?: Invalid key entered"))))

(define (get-key [accidentals 0])
  (if (and (< accidentals 7)
           (> accidentals -7))
      (list-ref keys (+ 7 accidentals))
      (error "get-key: Unable to retrieve key")))

(define (get-key-signature [key "C"])
  (if (is-valid-key? key)
      (cond
       ((char-upper-case? (car (string->list key))) (- (get-position major-keys key) 7))
       ((char-lower-case? (car (string->list key))) (- (get-position minor-keys key) 7))
       (else
        (error (format "get-key-signature: Could not find signature for %s" key))))
      (error "get-key-signature: Invalid key entered")))

(define (get-key-signature-accidentals [key "C"])
  (let ([accidentals (get-key-signature key)])
    (cond
     ((< 0 accidentals) (map (lambda (x) (string-append x "#")) (take fifths accidentals)))
     ((> 0 accidentals) (map (lambda (x) (string-append x "b")) (take (reverse fifths) (- accidentals))))
     (else '()))))

(define (get-notes [key "C"])
  (let ([altered-notes (map (lambda (x) (string (car (string->list x)))) (get-key-signature-accidentals key))]
        [symbol (if (positive? (get-key-signature key)) "#" "b")]
        [raw-tonic-position (get-position base-scale (string (char-upcase (car (string->list key)))))])
    (for/list ([note (better-take (member (list-ref base-scale raw-tonic-position) (double base-scale)) 7)])
      (if (member note altered-notes) (string-append note symbol) (string-append note "")))))

(define (relative-major key [lst keys])
  (cond
    ((empty? lst) (error (format "relative-major: ~s is not a minor key." key)))
    ((equal? (cadar lst) key) (caar lst))
    (else
     (relative-major key (cdr lst)))))

(define (relative-minor key [lst keys])
  (cond
    ((empty? lst) (error (format "relative-minor: ~s is not a major key." key)))
    ((equal? (caar lst) key) (cadar lst))
    (else
     (relative-minor key (cdr lst)))))

(provide (all-defined-out))