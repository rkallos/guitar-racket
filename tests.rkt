#lang racket
(require rackunit)
(require "misc.rkt")
(require "notes.rkt")

;; strip-note
(check-equal? (strip-note "C##") "C")
(check-equal? (strip-note "Bbb") "B")

;; int-to-note
(check-equal? (int-to-note 0) "C")
(check-equal? (int-to-note 7) "G")
(check-equal? (int-to-note 10) "A#")
(check-equal? (int-to-note 10 #:accidentals "b") "Bb")

;; is-enharmonic?
(check-true (is-enharmonic? "C#" "Db"))
(check-true (is-enharmonic? "Cbbb" "A"))
(check-false (is-enharmonic? "Cbb" "B"))

;; note?
(check-true (note? "C"))
(check-true (note? "C###"))
(check-false (note? "H"))

;; reduce-accidentals
(check-equal? (reduce-accidentals "C##") "D")
(check-equal? (reduce-accidentals "Dbb") "C")
(check-equal? (reduce-accidentals "Cbbb") "A")

;; reduce-redundant-accidentals
(check-equal? (reduce-redundant-accidentals "C#") "C#")
(check-equal? (reduce-redundant-accidentals "C#b")  "C")
(check-equal? (reduce-redundant-accidentals "C###bbbb") "Cb")

;; augment
(check-equal? (augment "C") "C#")
(check-equal? (augment "C#") "C##")
(check-equal? (augment "Cb") "C")

;; diminish
(check-equal? (diminish "C") "Cb")
(check-equal? (diminish "Cb") "Cbb")
(check-equal? (diminish "C#") "C")
