#lang racket

; use cps to chagnge how return works
; (pslit '(a b c d e f g)) => '((a c e g) ( b d f))

(define split-cps
  (lambda (lis return)
    (if (null? lis)
        (return '() '())
        (split-cps (cdr lis) (lambda (v1 v2) (return (cons (car lis) v2) v1))))))

; split-cps: (splitk 3 '(a b c d e f g) returncont) => '((a b c) (d e f g))
;(define splitk
;(lambda (k lis return)
;  (if (null? lis)
;      (return '() '() a)
;      (
      
; (flatten-cps '((a) ((((b) c) (d e))) (f))) (g)) (lambda (v) v)) => '(a b c d e f g)
;(define flatten-cps
;  (lambda (lis return)
;    (cond
;      ((null? lis) (return '()))
;      ((list? (car lis)) (flatten-cps (car lis) (lambda (v1) (flatten-cps (cdr lis) (lambda (v2) (append-cps v1 v2 return))))))
;      (else (flatten-cps (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; odd-even-cps: (odd-even-cps '(1 3 5 6 8 9) returncont) => '((1 3 5 9) (6 8))
(define odd-even-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      ((even? (car lis)) (odd-even-cps (cdr lis) (lambda (odd even) (return odd (cons (car lis) even)))))
      (else (odd-even-cps (cdr lis) (lambda (odd even) (return (cons (car lis) odd) even)))))))


; multiply: takes a list and multiplies all the values
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((zero? (car lis)) (break 0))
      (else (multiply-cps (cdr lis) (lambda (v) (return (* v (car lis)))) break)))))

; multiply with normal recursion using a break to jump out. Set the break with call/cc
(define multiply-break
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply-break (cdr lis) break))))))

(define multiply
  (lambda (lis)
    (call/cc
     (lambda (k)
       (multiply-break lis k)))))


; indexof: Given a list and an element, returns the "index" of the element
; in the list
; write this with normal recursion, but return immediately with -1 when you get to
; end of the list
(define indexof
  (lambda (a lis)
    (cond
      ((null? lis) 0)
      ((eq? a (car lis)) 0)
      (else (+ 1 (indexof a (cdr lis)))))))

(define indexof-break
  (lambda (a lis break)
    (cond
      ((null? lis) (break -1))
      ((eq? a (car lis))




