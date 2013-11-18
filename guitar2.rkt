#lang racket/gui

(require 2htdp/image
         (only-in mrlib/image-core render-image))
(require "misc.rkt"
         "scale-class.rkt"
         "notes.rkt")
;;
;; Default Variables
;;

(define TUNING '("E" "B" "G" "D" "A" "E"))
(define INTTUNING '(4 11 7 2 9 4))
(define STRINGS (length TUNING))
(define FRETS 12)
(define WIDTH 700)
(define HEIGHT 120)
(define DOTSIZE 20)
(define PADDING 20)
(define BASE (rectangle WIDTH HEIGHT "outline" "black"))

(define scale-types
  (list major-mscale
        dorian-mscale
        phrygian-mscale
        lydian-mscale
        mixolydian-mscale
        minor-mscale
        locrian-mscale
        harmonic-minor
        melodic-minor
        major-penta
        minor-penta))

(define (make-scales-and-labels [lst scale-types])
  (let loop ([out (make-hash)]
             [types lst])
    (cond ((empty? types) out)
          (else
           (hash-set! out (mscale-label ((car types) "C")) (car types))
           (loop out (cdr types))))))

(define scales-and-labels (make-scales-and-labels))
#;
(define scales-and-labels (hash "Major" major-mscale
                                "Dorian" dorian-mscale
                                "Phrygian" phrygian-mscale
                                "Lydian" lydian-mscale
                                "Mixolydian" mixolydian-mscale
                                "Minor/Aolean" minor-mscale
                                "Locrian" locrian-mscale
                                "Harmonic Minor" harmonic-minor
                                "Melodic Minor" melodic-minor
                                "Major Pentatonic" major-penta
                                "Minor Pentatonic" minor-penta))

;;
;; Data Definitions
;;

(define-struct fretboard
  (width height strings frets tuning dots)
  #:transparent)
;; Fretboard is (make-fretboard Integer Integer Integer Integer (Listof Note) (Listof Dot))
;; Represents fretboard of instrument with any number of strings and frets

(define-struct guitar-posn
  (string fret)
  #:transparent)
;; Guitar-posn is (make-guitar-posn Integer Integer)
;; Represents string and fret location on fretboard

(define-struct dot
  (txt color gposn)
  #:transparent)
;; Dot is (make-dot String String Guitar-posn)
;; Represents a dot on a fretboard with text and color at guitar-posn

;; Default color scheme for coloring the fretboard
(define default-colors (list "tomato" "orange" "gold" "limegreen" "DeepSkyBlue" "plum" "mediumpurple"))

;;
;; Function Definitions
;;

;; Fretboard -> Fretboard
;; Recalculates height of fretboard based on number of strings
(define (recalc-fretboard fb)
  (make-fretboard (fretboard-width fb)
                  (* DOTSIZE (fretboard-strings fb))
                  (fretboard-strings fb)
                  (fretboard-frets fb)
                  (fretboard-tuning fb)
                  (fretboard-dots fb)))

;; Scale -> Boolean
;; Determines if scale contains same notes ascending and descending
(define (mscale-same-up-down? sc)
  (equal? (mscale-ascending sc)
          (reverse (mscale-descending sc))))

;; Guitar-Posn [Listof Int] -> String
;; Return int on string at fret with tuning
;; int is essentially the scale degree of a chromatic scale starting on "C",
;; zero-indexed.
(define (get-int-at gposn [tuning INTTUNING])
  (let ([open-string (list-ref tuning (sub1 (guitar-posn-string gposn)))])
    (remainder (+ open-string (guitar-posn-fret gposn)) 12)))

;; Guitar-Posn String -> String
;; Return given note on string at fret, notated with the proper accidental.
(define (get-note-at gposn [sharp "#"])
  (int-to-note (get-int-at gposn (map note-to-int chosen-tuning)) sharp))

;; String Scale -> Boolean
;; Determines whether given note is in given scale.
(define (is-in-scale? note scale)
  (if (or (member note (mscale-ascending scale))
          (member note (mscale-descending scale))) #t #f))

;; Scale Fretboard -> Fretboard
(define (scale-dots scale fb [dir mscale-ascending])
  (define int-list (map note-to-int (dir scale)))
  (define (recur-frets f mf string)
    (let ([note (note-to-int (get-note-at (guitar-posn string f)))])
      (cond ((> f mf) '())
            ((member note int-list)
             (let ([pos (get-position int-list note)])
               (cons (make-dot (list-ref (dir scale) pos)
                               (list-ref default-colors pos)
                               (make-guitar-posn string f))
                     (recur-frets (add1 f) mf string))))
            (else (recur-frets (add1 f) mf string)))))
  (define (recur-strings s ms)
    (cond ((> s ms) '())
          (else (cons (recur-frets 0 (fretboard-frets fb) s)
                      (recur-strings (add1 s) ms)))))
  (make-fretboard
   (fretboard-width fb)
   (fretboard-height fb)
   (fretboard-strings fb)
   (fretboard-frets fb)
   (fretboard-tuning fb)
   (flatten (recur-strings 1 (fretboard-strings fb)))))

;;
;; Drawing Functions
;;

;; Guitar-posn Fretboard -> Number
;; Given size of fretboard, return position of fret in pixels
(define (locate-fret gposn fb)
  (define step (/ (fretboard-width fb) (fretboard-frets fb)))
  (+ PADDING (/ step 2) (* step (guitar-posn-fret gposn))))

;; Guitar-posn Fretboard -> Number
;; Given size of fretboard, return position of string in pixels
(define (locate-string gposn fb)
  (define nfb (recalc-fretboard fb))
  (define step (/ (fretboard-height nfb) (sub1 (fretboard-strings nfb))))
  (+ PADDING (* step (sub1 (guitar-posn-string gposn)))))

;; Dot -> Image
;; Create image of dot to be placed on fretboard
(define (draw-dot dt)
  (overlay
   (text
    (dot-txt dt)
    10
    "black")
   (circle 10 "solid" (dot-color dt))))

;; Dot Fretboard -> Fretboard
;; Creates fretboard with Dot at Guitar-Posn
(define (place-dot dt [fb DEFFRETBOARD])
  (make-fretboard (fretboard-width fb)
                  (fretboard-height fb)
                  (fretboard-strings fb)
                  (fretboard-frets fb)
                  (fretboard-tuning fb)
                  (cons dt (fretboard-dots fb))))

;; Number, Number, Image -> Image
;; Adds table of equally-sized cells on image
(define (draw-table rows cols base)
  (draw-cols cols (draw-rows rows base)))

;; Number, Image -> Image
;; Adds equally-spaced rows to base image
(define (draw-rows rows base)
  (let loop ([row rows])
    (cond ((= row 0) base)
          (else (place-image (line (image-width base) 0 "black")
                             (/ (image-width base) 2)
                             (* (image-height base) (/ row rows))
                             (loop (sub1 row)))))))

;; Number, Image -> Image
;; Adds equally-spaced columns to base image
(define (draw-cols cols base)
  (let loop ([col cols])
    (cond ((= col 0) base)
          (else (place-image (line 0 (image-height base) "black")
                             (* (image-width base) (/ col cols))
                             (/ (image-height base) 2)
                             (loop (sub1 col)))))))

;; Fretboard -> Image
(define (draw-fretboard fb)
  (define nfb (recalc-fretboard fb))
  (define base (image-pad PADDING
                          PADDING
                          PADDING
                          (+ PADDING (/ (fretboard-width nfb)
                                        (fretboard-frets nfb)))
                          (outline (draw-table (sub1 (fretboard-strings nfb))
                                               (fretboard-frets nfb)
                                               (rectangle (fretboard-width nfb)
                                                          (fretboard-height nfb)
                                                          "solid"
                                                          "white"))
                                   "black")))
  (let loop ([dots (fretboard-dots nfb)])
    (cond ((empty? dots) base)
          (else (place-image (draw-dot (car dots))
                             (locate-fret (dot-gposn (car dots)) nfb)
                             (locate-string (dot-gposn (car dots)) nfb)
                             (loop (cdr dots)))))))

(define TESTDOTS (list (make-dot "1" "red" (make-guitar-posn 1 0))
                       (make-dot "2" "orange" (make-guitar-posn 2 1))
                       (make-dot "3" "yellow" (make-guitar-posn 3 2))
                       (make-dot "4" "green" (make-guitar-posn 4 3))
                       (make-dot "5" "cyan" (make-guitar-posn 5 4))
                       (make-dot "6" "magenta" (make-guitar-posn 6 5))
                       (make-dot "2" "orange" (make-guitar-posn 2 1))
                       (make-dot "3" "yellow" (make-guitar-posn 3 1))
                       (make-dot "4" "green" (make-guitar-posn 4 1))
                       (make-dot "5" "cyan" (make-guitar-posn 5 1))
                       (make-dot "6" "magenta" (make-guitar-posn 6 1))))
(define TESTFRETBOARD (make-fretboard WIDTH 120 8 FRETS TUNING TESTDOTS))

;; Integer, Integer, Integer, Integer, Image -> Image
;; Adds empty space to top, right, bottom and left of image
(define (image-pad t r b l base)
  (let ([h (image-height base)]
        [w (image-width base)])
    (place-image base
                 (+ (/ w 2) l)
                 (+ (/ h 2) t)
                 (rectangle (+ r l w)
                            (+ t b h)
                            "solid"
                            "white"))))

;; Image String -> Image
;; Outlines image with rectangle of Color
(define (outline img color)
  (overlay (rectangle (image-width img)
                      (image-height img)
                      "outline"
                      color)
           img))

(define DEFFRETBOARD
  (make-fretboard
   WIDTH HEIGHT
   STRINGS FRETS
   TUNING
   '()))

(define VIOLIN
  (make-fretboard
   WIDTH
   HEIGHT
   4 12
   (list "G" "D" "A" "E")
   '()))
;;
;; GUI Definitions
;;

;; Basic GUI state
(define chosen-note "C")
(define chosen-scale-type major-mscale)
(define (chosen-scale) (chosen-scale-type chosen-note))
;(define chosen-fretboard DEFFRETBOARD)
(define chosen-direction mscale-ascending)
(define string-choices (take (mscale-ascending (major-mscale "C")) 7))

(define chosen-strings STRINGS)
(define chosen-frets FRETS)
(define chosen-tuning TUNING)

(define (chosen-fretboard)
  (make-fretboard
   WIDTH HEIGHT
   chosen-strings
   chosen-frets
   chosen-tuning
   '()))

;; Basic window frame. Contains main panel
(define gui (new frame%
                 [label "Virtual Fretboard"]
                 [width 800]
                 [height 400]
                 [stretchable-width 900]
                 [stretchable-height 250]))

;; Main panel containing all other UI elements
(define main-panel (new vertical-panel% [parent gui]))

;; Paint callback for fretboard-canvas
(define (paint! a-canvas my-drawing-context)
  (define my-new-scene (draw-fretboard
                        (scale-dots
                         (chosen-scale)
                         (chosen-fretboard)
                         chosen-direction)))
  (render-image my-new-scene my-drawing-context 0 0))

;; Main canvas for drawing fretboards
(define fretboard-canvas (new canvas%
                              [parent main-panel]
                              [min-width WIDTH]
                              [min-height HEIGHT]
                              [paint-callback paint!]))

(define controls-panel (new vertical-panel%
                            [parent main-panel]
                            [min-width 0]))

(define scale-and-tuning-panel (new horizontal-panel%
                                    [parent controls-panel]))

(define scale-type-panel (new group-box-panel%
                              [parent scale-and-tuning-panel]
                              [label "Scale Controls"]))

(define tuning-panel (new group-box-panel%
                          [parent scale-and-tuning-panel]
                          [label "Tuning Controls"]))

(define starting-note-field (new text-field%
                                 [parent scale-type-panel]
                                 [label "Note"]
                                 [min-width 100]
                                 [stretchable-width #f]
                                 [init-value "C"]))

(define scale-dir-choice (new choice%
                              [parent scale-type-panel]
                              [label "Direction"]
                              [choices (list "Ascending"
                                             "Descending")]
                              [enabled #f]
                              [callback (lambda (c e)
                                          (if (equal? (send scale-dir-choice get-string-selection)
                                                      "Descending")
                                              (set! chosen-direction mscale-descending)
                                              (set! chosen-direction mscale-ascending)))]))

(define scale-type-choice (new choice%
                               [parent scale-type-panel]
                               [label "Scale Type: "]
                               [choices (map (lambda (x)
                                               (mscale-label (x "C")))
                                             scale-types)]
                               [callback (lambda (c e)
                                           (begin (set! chosen-scale-type
                                                        (hash-ref scales-and-labels
                                                                  (send scale-type-choice get-string-selection)))
                                                  (if (mscale-same-up-down? (chosen-scale))
                                                      (send scale-dir-choice enable #f)
                                                      (send scale-dir-choice enable #t))))]))

(define tuning-box (new text-field%
                        [parent tuning-panel]
                        [label "Tuning"]
                        [min-width 75]
                        [init-value "E A D G B E"]))

(define tuning-radio (new radio-box%
                          [parent tuning-panel]
                          [label ""]
                          [choices (list "Low to high"
                                         "High to low")]))

(define msg (new message%
                 [parent controls-panel]
                 [label "Welcome!"]
                 [auto-resize #t]))

(define button (new button%
                    [parent controls-panel]
                    [label "Show"]
                    [callback
                     (lambda (_ event)
                       (let ([note-txt (send starting-note-field get-value)]
                             [scale-type (send scale-type-choice get-string-selection)]
                             [radio-choice (send tuning-radio get-selection)]
                             [split (string-split (send tuning-box get-value))])
                         (set! chosen-strings (length split))
                         (if (= 1 radio-choice)
                             (set! chosen-tuning split)
                             (set! chosen-tuning (reverse split)))
                         (cond
                          ((>= 1 chosen-strings)
                           (send msg set-label "Not enough strings"))
                          ((is-valid-note? note-txt)
                           (begin (set! chosen-note note-txt)
                                  (set! chosen-scale-type
                                        (hash-ref scales-and-labels scale-type))
                                  (send fretboard-canvas refresh)))
                          (else (send msg set-label "Not a valid note")))))]))
