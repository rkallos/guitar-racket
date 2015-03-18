#lang racket/base

(require rackunit
         "notes.rkt")

(define notes-tests
  (test-suite
   "Tests for notes.rkt"
   
   (check-equal? (strip-note "C#") "C" "Stripping accidentals")))
