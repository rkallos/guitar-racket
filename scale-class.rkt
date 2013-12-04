#lang racket

(require "misc.rkt"
         "intervals.rkt"
         "notes.rkt")

;; (make-mscale String List List)
(define-struct mscale
  (root
   ascending
   descending
   label)
  #:transparent)

;; mscale -> Listof Listof String
;; prints (list (mscale-ascending mscale) (mscale-descending mscale))
(define (show-mscale mscale)
  (list (mscale-ascending mscale)
        (mscale-descending mscale)))

;; mscale -> Boolean
;; Returns whether or not scale is the same ascending or descending
(define (mscale-same-up-down? mscale)
  (equal? (mscale-ascending mscale)
          (mscale-descending mscale)))

;; String -> mscale
;; Create mscale struct of major mscale of root
(define (major-mscale root)
  (let ([up (list root
                  (int-major-second root)
                  (int-major-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  (int-major-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Major")))

(define (dorian-mscale root)
  (let ([up (list root
                  (int-major-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Dorian")))

(define (phrygian-mscale root)
  (let ([up (list root
                  (int-minor-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-minor-sixth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Phrygian")))

(define (lydian-mscale root)
  (let ([up (list root
                  (int-major-second root)
                  (int-major-third root)
                  (augment (int-perfect-fourth root))
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  (int-major-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Lydian")))

(define (mixolydian-mscale root)
  (let ([up (list root
                  (int-major-second root)
                  (int-major-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Mixolydian")))

;; String -> mscale
;; produces natural minor mscale of root
(define (minor-mscale root)
  (let ([up (list root
                  (int-major-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-minor-sixth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Minor/Aolean")))

(define (locrian-mscale root)
  (let ([up (list root
                  (int-minor-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-minor-fifth root)
                  (int-minor-sixth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Locrian")))

;; String -> mscale
;; produces harmonic minor mscale of root
(define (harmonic-minor root)
  (let ([up (list root
                  (int-major-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-minor-sixth root)
                  (int-major-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Harmonic Minor")))

;; String -> mscale
;; produces melodic minor mscale of root
(define (melodic-minor root)
  (let ([up (list root
                  (int-major-second root)
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  (int-major-seventh root)
                  root)]
        [rl (list root)])
    (make-mscale root up
                (swap-at (swap-at (reverse up)
                                  (int-major-seventh root)
                                  (int-minor-seventh root))
                         (int-major-sixth root)
                         (int-minor-sixth root))
                "Melodic minor")))
                
(define (major-penta root)
  (let ([up (list root
                  (int-major-second root)
                  (int-major-third root)
                  (int-perfect-fifth root)
                  (int-major-sixth root)
                  root)])
    (make-mscale root up (reverse up) "Major Pentatonic")))

(define (minor-penta root)
  (let ([up (list root
                  (int-minor-third root)
                  (int-perfect-fourth root)
                  (int-perfect-fifth root)
                  (int-minor-seventh root)
                  root)])
    (make-mscale root up (reverse up) "Minor Pentatonic")))

(provide (all-defined-out))
