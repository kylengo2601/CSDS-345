#lang racket

; remove all copies of an atom from a list that contains sublists
(define removeall*
  (lambda (x lis)
    (cond
      ((null? lis) lis)
      ((eq? x (car lis)) (removeall* x (cdr lis)))
      ((list? (car lis)) (cons (removeall* x (car lis)) (removeall* x (cdr lis))))
      (else (cons (car lis) (removeall* x (cdr lis)))))))

; is an atom anywhere in a list, where the list could contain sublists
(define member*?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((pair? (car lis)) (or (member*? a (car lis)) (member*? a (cdr lis))))
      ((eq? a (car lis)) #t)
      (else (member*? a (cdr lis))))))

; replaceall* takes two atoms and a list with sublists
; and it replaces every isntance of a with b
; (replaceall* 'x 'y '(a b ((x) f) ((g (x))))) => '(a b ((y) f) ((g (y))))
(define replaceall*
  (lambda (x y lis)
    (cond
      ((null? lis) lis)
      ((eq? x (car lis)) (cons y (replaceall* x y (cdr lis))))
      ((list? (car lis)) (cons (replaceall* x y (car lis)) (replaceall* x y (cdr lis))))
      (else (cons (car lis) (replaceall* x y (cdr lis)))))))
    

; sumnumbers*: take a list that can contain sublists
; and sums all the numbers inside it.
(define sumnumbers*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis)) (+ (car lis) (sumnumbers* (cdr lis))))
      ((pair? (car lis)) (+ (sumnumbers* (car lis)) (sumnumbers* (cdr lis))))
      (else (+ 0 (sumnumbers* (cdr lis)))))))


; emptyall: remove all (non-empty list) atoms from a list
; (flatten '(a b (c d ((e) f (g)))) => '(a b c d e f g)
(define atom?
  (lambda (x)
    (if (and (not (null? x)) (not (pair? x)))
        #t
        #f)))

(define flatten
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((atom? (car lis)) (cons (car lis) (flatten (cdr lis))))
      ((pair? (car lis)) (cons (























