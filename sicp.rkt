#lang racket

;; Exercise 2.18

(define (my-reverse items)
  (define (my-reverse-iter lst acc)
    (if (null? lst)
        acc
        (my-reverse-iter (cdr lst) (cons (car lst) acc))))
  (my-reverse-iter items (quote ())))

