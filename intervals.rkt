#lang racket

(require "notes.rkt")
(require "keys.rkt")
(require "misc.rkt")

(define (interval key start-note interval)
  (define (loop n lst inc)
    (cond
      ((empty? lst) #f)
      ((equal? (car lst) n) inc)
      (else
       (loop n (cdr lst) (add1 inc)))))
  (cond
    ((not (is-valid-note? start-note)) (error (format "interval: ~s is not a valid note." start-note)))
    (else
     (list-ref (get-notes key) (remainder (+ interval (loop start-note (get-notes key) 0)) 7)))))

(define (int-unison note [key note])
  (interval key note 0))

(define (int-second note key)
  (interval key note 1))

(define (int-third note key)
  (interval key note 2))

(define (int-fourth note key)
  (interval key note 3))

(define (int-fifth note key)
  (interval key note 4))

(define (int-sixth note key)
  (interval key note 5))

(define (int-seventh note key)
  (interval key note 6))

(define (int-minor-unison note)
  (diminish note))

(define (int-major-unison note)
  (augment note))

(define (int-minor-second note)
  (aug-or-dim note (int-second (strip-note note) "C") 1))

(define (int-major-second note)
  (aug-or-dim note (int-second (strip-note note) "C") 2))

(define (int-minor-third note)
  (aug-or-dim note (int-third (strip-note note) "C") 3))

(define (int-major-third note)
  (aug-or-dim note (int-third (strip-note note) "C") 4))

(define (int-minor-fourth note)
  (aug-or-dim note (int-fourth (strip-note note) "C") 4))

(define (int-major-fourth note)
  (aug-or-dim note (int-fourth (strip-note note) "C") 5))

(define (int-perfect-fourth note)
  (int-major-fourth note))

(define (int-minor-fifth note)
  (aug-or-dim note (int-fifth (strip-note note) "C") 6))

(define (int-major-fifth note)
  (aug-or-dim note (int-fifth (strip-note note) "C") 7))

(define (int-perfect-fifth note)
  (int-major-fifth note))

(define (int-minor-sixth note)
  (aug-or-dim note (int-sixth (strip-note note) "C") 8))

(define (int-major-sixth note)
  (aug-or-dim note (int-sixth (strip-note note) "C") 9))

(define (int-minor-seventh note)
  (aug-or-dim note (int-seventh (strip-note note) "C") 10))

(define (int-major-seventh note)
  (aug-or-dim note (int-seventh (strip-note note) "C") 11))

(define (measure note1 note2)
  (let* ([n1 (note-to-int note1)]
         [n2 (note-to-int note2)]
         [res (- n2 n1)])
    (if (negative? res) (- 12 (- res)) res)))

(define (aug-or-dim note1 note2 interval)
  (let loop ([n1 note1]
             [n2 note2]
             [cur (measure note1 note2)])
    (cond
      ((equal? cur interval) n2)
      ((> cur interval) (loop n1 (diminish n2) (sub1 cur)))
      ((< cur interval) (loop n1 (augment n2) (add1 cur))))))

(define (invert interval)
  (reverse interval))

(define (determine-interval note1 note2 [shorthand #f])
  (define fifths-steps '(("unison" "1" 0)
                       ("fifth" "5" 7)
                       ("second" "2" 2)
                       ("sixth" "6" 9)
                       ("third" "3" 4)
                       ("seventh" "7" 11)
                       ("fourth" "4" 5)))
  (define (get-fifths n1 n2)
    (let ([notes-in-fifths
           (map (lambda (x)
                  (get-position fifths
                                (strip-note x)))
                (list n1 n2))])
      (if (< (apply - (reverse notes-in-fifths)) 0)
          (+ (- 7 (first notes-in-fifths)) (second notes-in-fifths))
          (apply - (reverse notes-in-fifths)))))
  (define (corner-case-unisons)
    (let* ([checkfunc (lambda (x)
                        (- (count (checker #\#) (cdr (string->list x)))
                           (count (checker #\b) (cdr (string->list x)))))]
           [n1val (checkfunc note1)]
           [n2val (checkfunc note2)])
      (cond
        ((equal? n1val n2val) (if (not shorthand) "major unison" "1"))
        ((< n1val n2val) (if (not shorthand) "augmented unison" "#1"))
        ((= 1 (- n1val n2val)) (if (not shorthand) "minor unison" "b1"))
        (else (if (not shorthand) "diminished unison" "bb1")))))
  (let* ([int-dist (measure note1 note2)]
         [number-of-fifths (get-fifths note1 note2)]
         [current (better-list-ref fifths-steps number-of-fifths)]
         [maj (list-ref current 2)])
    (cond
      ((apply equal? (map strip-note (list note1 note2)))
       (corner-case-unisons))
      ((equal? maj int-dist)
       (cond
         ((equal? (list-ref current 0) "fifth")
          (if (not shorthand) "perfect fifth" "5"))
         ((equal? (list-ref current 0) "fourth")
          (if (not shorthand) "perfect fourth" "4"))
         (else
          (if (not shorthand)
              (string-append "major "
                             (list-ref current 0))
              (list-ref current 1)))))
      ((<= (add1 maj) int-dist)
       (if (not shorthand)
           (string-append "augmented "
                          (list-ref current 0))
           (string-append (make-string (- int-dist maj) #\#)
                          (list-ref current 1))))
      ((equal? (sub1 maj) int-dist)
       (if (not shorthand)
           (string-append "minor "
                          (list-ref current 0))
           (string-append "b" (list-ref current 1))))
      ((>= (- maj 2) int-dist)
       (if (not shorthand)
           (string-append "diminished "
                          (list-ref current 0))
           (string-append (make-string (- maj int-dist) #\b)
                          (list-ref current 1))))
      (else (error "determine-interval: failed")))))

(define (interval-from-shorthand note int [up #t])
  (define shorthand-lookup `((1 ,int-major-unison ,int-major-unison)
                             (2 ,int-major-second ,int-minor-seventh)
                             (3 ,int-major-third ,int-minor-sixth)
                             (4 ,int-major-fourth ,int-major-fifth)
                             (5 ,int-major-fifth ,int-major-fourth)
                             (6 ,int-major-sixth ,int-minor-third)
                             (7 ,int-major-seventh ,int-minor-second)))
  (cond
    ((not (is-valid-note? note)) #f)
    (else
     ((apply compose1
       (map
        (lambda (x)
          (cond
            ((equal? #\# x) (if up augment diminish))
            ((equal? #\b x) (if up diminish augment))
            (else
             (for/first ([int shorthand-lookup]
                         #:when (get-position int ((compose1 string->number string) x)))
               (if up 
                   (list-ref int 1)
                   (list-ref int 2))))))
        (string->list int)))
      note))))

(define (is-perfect-consonant note1 note2 [include-fourths #t])
  (let ([dhalf (measure note1 note2)])
    (or (if (member dhalf '(0 7)) #t #f)
        (and include-fourths
             (equal? dhalf 5)))))

(define (is-imperfect-consonant note1 note2)
  (let ([dhalf (measure note1 note2)])
    (or (if (member dhalf '(3 4 8 9)) #t #f)
        #f)))

(define (is-consonant note1 note2 [include-fourths #t])
  (or (is-perfect-consonant note1 note2 include-fourths)
      (is-imperfect-consonant note1 note2)))

(define (is-dissonant note1 note2 [include-fourths #f])
  (not (is-consonant note1 note2 (not include-fourths))))

(provide (all-defined-out))
