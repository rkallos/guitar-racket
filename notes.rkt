#lang racket

(require "misc.rkt")
(require rackunit)

(define note-hash #hash(("C" . 0)
                        ("D" . 2)
                        ("E" . 4)
                        ("F" . 5)
                        ("G" . 7)
                        ("A" . 9)
                        ("B" . 11)))
(define fifths (list "F" "C" "G" "D" "A" "E" "B"))

(define sharpnotes (list "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(define flatnotes (list "C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))

;; Note -> Note
;; Returns note without accidentals
(define (strip-note note)
  (string (car (string->list note))))

;; Character -> Character -> Boolean
;; Checks if sym is equal to x
(define (checker sym)
  (lambda (x)
    (equal? x sym)))

;; Number -> Note
;; Produces note from note-int, which represents semitones away from C.
(define (int-to-note note-int [accidentals "#"])
  (cond
    ((<= 12 note-int) (int-to-note (remainder note-int 12) accidentals))
    ((> 0 note-int) (int-to-note (+ 12 note-int) accidentals))
    ((equal? accidentals "#") (list-ref sharpnotes note-int))
    ((equal? accidentals "b") (list-ref flatnotes note-int))
    (else
     (error (format "int-to-note: %s is not a valid accidental." accidentals)))))

;; Note + Note -> Boolean
;; Checks if two notes are enharmonic equivalents.
(define (is-enharmonic? note1 note2)
  (equal? (note-to-int note1) (note-to-int note2)))

;; String -> Boolean
;; Checks if String is a valid note.
(define (is-valid-note? note)
  (regexp-match? #rx"[A-G][#b]*" note))

;; Note -> Integer
;; Turns Note into a number representing semitones above C
(define (note-to-int note)
  (if (is-valid-note? note)
      (let* ([flats (count (checker #\b) (cdr (string->list note)))]
             [sharps (count (checker #\#) (cdr (string->list note)))])
        (if (positive? (- (+ (hash-ref note-hash (strip-note note))
                             sharps)
                          flats))
            (- (+ (hash-ref note-hash (strip-note note))
                  sharps)
               flats)
            (- 12
               (+ (hash-ref note-hash (strip-note note)) sharps)
               flats)))
      (error "note-to-int: Input not a valid note.")))

;; Note -> Note
;; Returns enharmonic equivalent without accidentals
(define (reduce-accidentals note)
  (let* ([letter (strip-note note)]
         [flats (count (checker #\b) (cdr (string->list note)))]
         [sharps (count (checker #\#) (cdr (string->list note)))]
         [val (+ (note-to-int letter) (- sharps flats))])
    (if (>= val 12)
        (int-to-note (remainder val 12))
        (int-to-note (remainder val 12) "b"))))

;; Note -> Note
;; Removes redundant accidentals from note.
;; This differs from reduce-accidentals in that it will not change the note letter.
(define (reduce-redundant-accidentals note)
  (let* ([letter (strip-note note)]
         [flats (count (checker #\b) (cdr (string->list note)))]
         [sharps (count (checker #\#) (cdr (string->list note)))])
    (let loop ([val (- sharps flats)]
               [out letter])
      (cond
       ((zero? val) out)
       ((positive? val) (loop (sub1 val) (augment out)))
       (else
        (loop (add1 val) (diminish out)))))))

;; Note -> Note
;; Returns augmented note by removing a "b" or adding a "#"
(define (augment note)
  (if (not (equal? (last (string->list note)) #\b))
      (string-append note "#")
      (list->string (drop-right (string->list note) 1))))

;; Note -> Note
;; Returns diminished note by removing a "#" or adding a "b"
(define (diminish note)
  (if (not (equal? (last (string->list note)) #\#))
      (string-append note "b")
      (list->string (drop-right (string->list note) 1))))

(provide (all-defined-out))
