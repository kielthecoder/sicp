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
        