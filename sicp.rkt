#lang racket

;; Exercise 2.18

(define (my-reverse items)
  (define (my-reverse-iter lst acc)
    (if (null? lst)
        acc
        (my-reverse-iter (cdr lst) (cons (car lst) acc))))
  (my-reverse-iter items (quote ())))

;; Exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0)
         (no-more? coin-values)) 0)
    (else
     (+ (cc amount (except-first-denomination coin-values))
        (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

;; Exercise 2.20

(define (same-parity n . rest)
  (define (same-parity-iter lst acc)
    (if (null? lst)
        acc
        (if (or (and (odd? n) (odd? (car lst)))
                (and (even? n) (even? (car lst))))
            (same-parity-iter (cdr lst) (cons (car lst) acc))
            (same-parity-iter (cdr lst) acc))))
  (my-reverse (same-parity-iter rest (list n))))

(define (my-map func items)
  (if (null? items)
      (quote ())
      (cons (func (car items))
            (my-map func (cdr items)))))

(define (scale-list items factor)
  (my-map (lambda (x) (* x factor)) items))

;; Exercise 2.21

(define (square-list-1 items)
  (if (null? items)
      (quote ())
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (my-map (lambda (x) (* x x)) items))

;; Exercise 2.23

(define (for-each func items)
  (cond
    ((null? items) #t)
    (else
     (func (car items))
     (for-each func (cdr items)))))

(define (count-leaves x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x))))))

;; Exercise 2.24

; '(1 (2 (3 4)))

;; Exercise 2.25

; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
; (car (car '((7))))
; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; Exercise 2.26

; '(1 2 3 4 5 6)
; '((1 2 3) 4 5 6)
; '((1 2 3) (4 5 6))

