#lang racket
; (myreplaceall 'a 'A '(a b a c a)) => '(A b A c A)
(define myreplaceall
  (lambda (a b lis)
    (cond
      ((null? lis) '())
      ((eq? a (car lis)) (cons b (myreplaceall a b (cdr lis))))
      (else (cons (car lis) (myreplaceall a b (cdr lis)))))))

; (add-to-end 'x '(a b c d)) => '(a b c d x)
(define add-to-end
  (lambda (x lis)
    (cond
      ((null? lis) (cons x lis))
      (else (cons (car lis) (add-to-end x (cdr lis)))))))
      
; null? (id yhr list an empty list)
; car (returns the first ele)
; cdr (returns all but first ele)
; cons (fill the list)


; (myreverse '(a b c d)) => '(d c b a)
(define myreverse
  (lambda (lis)
    (cond
      ((null? lis) lis)
      (else (append (myreverse (cdr lis)) (cons (car lis) '()))))))
       
;(mymap (lambda (x) (* x x) '(1 2 3 4)) => '(1 4 9 16)
(define mymap
  (lambda (f lis)
    (cond
      ((null? lis) '())
      (else (cons (f (car lis)) (mymap f (cdr lis)))))))