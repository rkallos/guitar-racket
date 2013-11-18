#lang racket

;;; 
;;; Functions I need and can't find elsewhere
;;;
;;; get-position: Determines which get-position function to use, then uses it
;;; get-position-lst: Gets position of element in list
;;; get-position-str: Gets position of element in string
;;; double: Appends list to itself
;;; better-take: Doubles list to take more values than list provides
;;; better-list-ref: Allows for indexes greater than (length lst), and negative indexes
;;; swap-at: Swap old with new in lst

(define (get-position lst x)
  (cond
    ((string? lst)
     (if (member x (string->list lst)) (get-position-str lst x 0) #f))
    (else
     (if (member x lst) (get-position-lst lst x 0) #f))))

(define (get-position-lst lst x pos)
  (cond
    ((equal? x (list-ref lst pos)) pos)
    (else (get-position-lst lst x (add1 pos)))))

(define (get-position-str lst x pos)
  (cond
    ((equal? x (string-ref lst pos)) pos)
    (else (get-position-str lst x (add1 pos)))))

(define (double lst)
  (append lst lst))

(define (better-take lst n)
  (cond
    ((> n (length lst)) (better-take (double lst) n))
    (else
     (take lst n))))

(define (better-list-ref lst x)
  (cond
    ((positive? x)
     (if (>= x (length lst))
         (list-ref lst (remainder x (length lst)))
         (list-ref lst x)))
    (else
     (better-list-ref lst (+ (length lst) x)))))

(define (swap-at lst old new)
  (cond
    ((not (member old lst)) #f)
    (else
     ((lambda (x y)
        (append x (cons new (cdr y))))
      (take lst (get-position lst old))
      (drop lst (get-position lst old))))))

(define (multi-swap-at lst olds news)
  (define (recur lst olds news)
    (if (empty? olds)
        lst
        (recur (swap-at lst (car olds) (car news))
          (cdr olds)
          (cdr news))))
  (if (not (equal? (length olds)
                   (length news)))
      (error "Lists are not the same length")
      (recur lst olds news)))

(provide (all-defined-out))
