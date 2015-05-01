#lang racket

(require "misc.rkt")
(require "notes.rkt")
(require "keys.rkt")
(require "intervals.rkt")
(require (only-in lang/htdp-advanced string-contains?))

;;; Shamelessly stolen from mingus. Python sorted the dict for me, though :-(
(define chord-shorthand-meaning
  #hash(("" . " major triad")
        ("add9" . " dominant ninth")
        ("m/M7" . " minor/major seventh")
        ("sus" . " suspended fourth triad")
        ("7b5" . " dominant flat five")
        ("m" . " minor triad")
        ("M7" . " major seventh")
        ("M6" . " major sixth")
        ("M7+5" . " augmented minor seventh")
        ("susb9" . " suspended fourth ninth")
        ("M9" . " major ninth")
        ("67" . " dominant sixth")
        ("hendrix" . " hendrix chord")
        ("6/9" . " sixth ninth")
        ("aug" . " augmented triad")
        ("6/7" . " dominant sixth")
        ("M" . " major triad")
        ("7#11" . " lydian dominant seventh")
        ("69" . " sixth ninth")
        ("7sus4" . " suspended seventh")
        ("7b12" . " hendrix chord")
        ("add13" . " dominant thirteenth")
        ("mM7" . " minor/major seventh")
        ("m7b5" . " half diminished seventh")
        ("M7+" . " augmented major seventh")
        ("+" . " augmented triad")
        ("m11" . " minor eleventh")
        ("dim" . " diminished triad")
        ("m13" . " minor thirteenth")
        ("m7+" . " augmented minor seventh")
        ("7#5" . " augmented minor seventh")
        ("7" . " dominant seventh")
        ("6" . " major sixth")
        ("7#9" . " dominant sharp ninth")
        ("7b9" . " dominant flat ninth")
        ("dom7" . " dominant seventh")
        ("13" . " dominant thirteenth")
        ("sus47" . " suspended seventh")
        ("M13" . " major thirteenth")
        ("dim7" . " diminished seventh")
        ("m7" . " minor seventh")
        ("m6" . " minor sixth")
        ("11" . " eleventh")
        ("5" . " perfect fifth")
        ("add11" . " eleventh")
        ("sus2" . " suspended second triad")
        ("7+" . " augmented major seventh")
        ("9" . " dominant ninth")
        ("sus4b9" . " suspended fourth ninth")
        ("m9" . " minor ninth")
        ("sus4" . " suspended fourth triad")))



;;; Section: Triad and Triads
(define (triad note key)
  (list note (int-third note key) (int-fifth note key)))

(define (triads key)
  (map (lambda (x) (triad x key)) (get-notes key)))

;;; Section: Chord functions
(define (major-triad note)
  (list note (int-major-third note) (int-perfect-fifth note)))

(define (minor-triad note)
  (list note (int-minor-third note) (int-perfect-fifth note)))

(define (diminished-triad note)
  (list note (int-minor-third note) (int-minor-fifth note)))

(define (augmented-triad note)
  (list note (int-major-third note) (augment (int-major-fifth note))))

(define (seventh-chord note key)
  (flatten (list (triad note key) (int-seventh note key))))

(define (sevenths key)
  (map (lambda (x) (seventh-chord x key)) (get-notes key)))

(define (major-seventh note)
  (flatten (list (major-triad note) (int-major-seventh note))))

(define (minor-seventh note)
  (flatten (list (minor-triad note) (int-minor-seventh note))))

(define (dominant-seventh note)
  (flatten (list (major-triad note) (int-minor-seventh note))))

(define (half-diminished-seventh note)
  (flatten (list (diminished-triad note) (int-minor-seventh note))))

(define (minor-seventh-flat-five note)
  (half-diminished-seventh note))

(define (diminished-seventh note)
  (flatten (list (diminished-triad note) (diminish (int-minor-seventh note)))))

(define (minor-major-seventh note)
  (flatten (list (minor-triad note) (int-major-seventh note))))

(define (minor-sixth note)
  (flatten (list (minor-triad note) (int-major-sixth note))))

(define (major-sixth note)
  (flatten (list (major-triad note) (int-major-sixth note))))

(define (dominant-sixth note)
  (flatten (list (major-sixth note) (int-minor-seventh note))))

(define (sixth-ninth note)
  (flatten (list (major-sixth note) (int-major-second note))))

(define (minor-ninth note)
  (flatten (list (minor-seventh note) (int-major-second note))))

(define (major-ninth note)
  (flatten (list (major-seventh note) (int-major-second note))))

(define (dominant-ninth note)
  (flatten (list (dominant-seventh note) (int-major-second))))

(define (dominant-flat-ninth note)
  (flatten (list (dominant-seventh note) (int-minor-second note))))

(define (dominant-sharp-ninth note)
  (flatten (list (dominant-seventh note) (augment (int-major-second note)))))

(define (eleventh-chord note)
  (list note (int-perfect-fifth note) (int-minor-seventh note) (int-perfect-fourth note)))

(define (minor-eleventh note)
  (flatten (list (minor-seventh note) (int-perfect-fourth note))))

(define (major-thirteenth note)
  (flatten (list (major-ninth note) (int-major-sixth note))))

(define (minor-thirteenth note)
  (flatten (list (minor-ninth note) (int-major-sixth note))))

(define (dominant-thirteenth note)
  (flatten (list (dominant-ninth note) (int-major-sixth note))))

(define (suspended-triad note)
  (suspended-fourth-triad note))

(define (suspended-fourth-triad note)
  (list note (int-perfect-fourth note) (int-perfect-fifth note)))

(define (suspended-second-triad note)
  (list note (int-major-second note) (int-perfect-fifth note)))

(define (suspended-seventh note)
  (flatten (list (suspended-triad note) (int-minor-seventh note))))

(define (suspended-fourth-ninth note)
  (flatten (list (suspended-triad note) (int-minor-second note))))

(define (augmented-major-seventh note)
  (flatten (list (augmented-triad note) (int-major-seventh note))))

(define (augmented-minor-seventh note)
  (flatten (list (augmented-triad note) (int-minor-seventh note))))

(define (dominant-flat-five note)
  (list note (int-major-third note) (int-minor-fifth note) (int-minor-seventh note)))

(define (lydian-dominant-seventh note)
  (flatten (list (dominant-seventh note) (augment (int-perfect-fourth note)))))

(define (hendrix-chord note)
  (flatten (list (dominant-seventh note) (int-minor-third note))))

;;; Section: Tonics

(define (get-role key pos [sevens #f])
  (list-ref ((if sevens sevenths triads) key) (sub1 pos)))

(define (tonic key)
  (get-role key 1))

(define (tonic7 key)
  (get-role key 1 #t))

(define (supertonic key)
  (get-role key 2))

(define (supertonic7 key)
  (get-role key 2 #t))

(define (mediant key)
  (get-role key 3))

(define (mediant7 key)
  (get-role key 3 #t))

(define (subdominant key)
  (get-role key 4))

(define (subdominant7 key)
  (get-role key 4 #t))

(define (dominant key)
  (get-role key 5))

(define (dominant7 key)
  (get-role key 5 #t))

(define (submediant key)
  (get-role key 6))

(define (submediant7 key)
  (get-role key 6 #t))

(define (subtonic key)
  (get-role key 7))

(define (subtonic7 key)
  (get-role key 7 #t))

(define I tonic)
(define I7 tonic7)
(define ii supertonic)
(define II supertonic)
(define ii7 supertonic7)
(define II7 supertonic7)
(define iii mediant)
(define III mediant)
(define iii7 mediant7)
(define III7 mediant7)
(define IV subdominant)
(define IV7 subdominant7)
(define V dominant)
(define V7 dominant7)
(define vi submediant)
(define VI submediant)
(define vi7 submediant7)
(define VI7 submediant7)
(define vii subtonic)
(define VII subtonic)
(define vii7 subtonic7)
(define VII7 subtonic7)

;;; Section: Inversions
(define (chord-invert chord)
  (flatten (list (drop chord 1) (car chord))))

(define (first-inversion chord)
  (chord-invert chord))

(define (second-inversion chord)
  (chord-invert (chord-invert chord)))

(define (third-inversion chord)
  (chord-invert (chord-invert (chord-invert chord))))

;;; Section: determining chords from shorthand or notes
;;; Helper functions

(define (get-slash chord)
    (cond
      ((ormap (lambda (x)
                 (string-contains? x chord))
               (list "m/M7" "6/9" "6/7"))
       (get-slash (substring chord (+ (get-position chord #\/)
                                      (if (string-contains? "m/M7" chord)
                                          2 1)))))
     ((member #\/ (string->list chord)) (apply string (drop (string->list chord) (add1 (get-position chord #\/)))))
     (else #f)))
(define (get-polychord chord)
    (cond
     ((member #\| (string->list chord)) (apply string (drop (string->list chord) (add1 (get-position chord #\|)))))
     (else #f)))

(define (chord-from-shorthand shorthand-string [slash #f])
  ;; How this works: Takes a chord written in shorthand, and returns the notes
  ;; Recognizes: Triads, sevenths, sixths, ninths, elevenths, thirteenths, slashed chords, and several altered chords

  ;; The second argument is only used for a recursive call when a slashed chord or polychord is found.

  ;; Recognized abbreviations: "m" and "M" are replacable with min/mi/- or maj/ma respectively
  ;; Triads: "m" "M" or "" "dim"
  ;; Sevenths: "m7" "M7" "7" "m7b5" "dim7" "m/M7" "mM7"
  ;; Augmented: "aug" or "+" "7#5" "M7#5" "M7+" "m7+" "7+"
  ;; Suspended: "sus4" "sus2" "sus47" or "7sus4" "sus" "11" "sus4b9" "susb9"
  ;; Sixths: "6" "m6" "M6" "6/7" or "67" "6/9" or "69"
  ;; Ninths: "9" "add9" "M9" "m9" "7b9" "7#9"
  ;; Elevenths: "11" or "add11" "7#11" "m11"
  ;; Thirteenths: "13" or "add13" "M13" "m13"
  ;; Altered: "7b5" "7b9" "7#9" "67" or "6/7"
  ;; Special: "5" "NC" "hendrix"

  ;; Matching minor: #rx"min|mi|-"
  ;; Matching major: #rx"maj|ma"
  
  (let* ([typedchord (regexp-replace #rx"min|mi|-" (regexp-replace #rx"maj|ma" shorthand-string "M") "m")]
         [notename (list->string (for/list ([i typedchord]
                                      #:break (not (or
                                                    (note? (string i))
                                                    (or
                                                     (equal? #\# i)
                                                     (equal? #\b i)))))
                             i))]
         [chordwonote (list->string (drop (string->list typedchord) (string-length notename)))])
    (cond
     ((get-polychord typedchord)
      (let ([pone (substring typedchord 0 (get-position typedchord #\|))]
            [ptwo (substring typedchord (add1 (get-position typedchord #\|)))])
        (chord-from-shorthand
         ptwo
         (chord-from-shorthand pone))))
     ((get-slash typedchord) (chord-from-shorthand
                              (substring typedchord 0 (sub1 (get-position
                                                             typedchord
                                                             (string-ref
                                                              (get-slash typedchord) 0))))
                              (get-slash typedchord)))
     (else
      (let ([res (if (hash-has-key? chord-shorthand chordwonote)
                     ((hash-ref chord-shorthand chordwonote) notename)
                     (error (format "chord-from-shorthand: unknown chord ~s" chordwonote)))])
        (cond
          ((if (values slash) #t #f)
           (cond
             ((string? slash)
              (if (note? slash)
                  (append (list slash) res)
                  (error (format "chord-from-shorthand: Unknown slash note ~s" slash))))
             ((list? slash)
              (flatten (list res
                             (for/list ([i slash]
                                        #:when (not (equal? (last res) i)))
                               i))))))
          (else (values res))))))))

(define chord-shorthand
  (hash "m" minor-triad
        "M" major-triad
        "" major-triad
        "dim" diminished-triad
        "aug" augmented-triad
        "+" augmented-triad
        "7#5" augmented-minor-seventh
        "M7+5" augmented-minor-seventh
        "M7+" augmented-major-seventh
        "m7+" augmented-minor-seventh
        "7+" augmented-major-seventh
        "sus47" suspended-seventh
        "sus4" suspended-fourth-triad
        "sus2" suspended-second-triad
        "sus" suspended-triad
        "11" eleventh-chord
        "sus4b9" suspended-fourth-ninth
        "susb9" suspended-fourth-ninth
        "m7" minor-seventh
        "M7" major-seventh
        "7" dominant-seventh
        "dom7" dominant-seventh
        "m7b5" minor-seventh-flat-five
        "dim7" diminished-seventh
        "m/M7" minor-major-seventh
        "mM7" minor-major-seventh
        "m6" minor-sixth
        "M6" major-sixth
        "6" major-sixth
        "6/7" dominant-sixth
        "67" dominant-sixth
        "6/9" sixth-ninth
        "69" sixth-ninth
        "9" dominant-ninth
        "7b9" dominant-flat-ninth
        "7#9" dominant-sharp-ninth
        "M9" major-ninth
        "m9" minor-ninth
        "7#11" lydian-dominant-seventh
        "m11" minor-eleventh
        "M13" major-thirteenth
        "m13" minor-thirteenth
        "13" dominant-thirteenth
        "7b5" dominant-flat-five
        "hendrix" hendrix-chord
        "7b12" hendrix-chord
        "5" (lambda (x) (list x (int-perfect-fifth x)))))
