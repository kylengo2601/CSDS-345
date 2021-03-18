#lang racket
; Kyle Ngo (ktn27)

; Q1: dotproduct
(define dotproduct
  (lambda (lis1 lis2 return)
    (cond
      ((or (null? lis1) (null? lis2)) (return 0))
      (else (dotproduct (cdr lis1) (cdr lis2) (lambda (v) (return (+ v (* (car lis1) (car lis2))))))))))


; Q2: removesubsequence
(define removesubsequence
  (lambda (lis1 lis2 return)
    (cond
      ((or (null? lis1) (null? lis2)) (return lis2))
      ((eq? (car lis1) (car lis2)) (removesubsequence (cdr lis1)  (cdr lis2) (lambda (v) (return v))))
      (else (removesubsequence lis1 (cdr lis2) (lambda (v) (return (cons (car lis2) v))))))))


; Q3: squareroot
(define squareroot
  (lambda (val i return)
    (cond
      ((or (zero? i) (zero? val)) (return val))
      (else (squareroot val (- i 1) (lambda (r) (return (- r (/ (- (* r r) val) (* 2 r))))))))))


; Q4: numatoms*
(define numatoms*
  (lambda (lis return)
    (cond
      ((null? lis) (return 0))
      ((list? (car lis)) (numatoms* (car lis) (lambda (v1) (numatoms* (cdr lis) (lambda (v2) (return (+ v1 v2)))))))
      (else (numatoms* (cdr lis) (lambda (v) (return (+ 1 v))))))))


; Q5: reverse*
(define reverse*
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((list? (car lis)) (reverse* (cdr lis) (lambda (v1) (reverse* (car lis) (lambda (v2) (return (append v1 (cons v2 '()))))))))
      (else (reverse* (cdr lis) (lambda (v) (return (append v (cons (car lis) '())))))))))


; Q6: vectormult
(define vectormult
  (lambda (vector matrix return)
    (if (null? (car matrix))
        (return '())
        (vectormult vector (mymap cdr matrix (lambda (m) m)) (lambda (v) (return (cons (dotproduct vector (mymap car matrix (lambda (m) m)) (lambda (x) x)) v)))))))

; apply f to every element of the list
(define mymap
  (lambda (f lis return)
    (if (null? lis)
        (return lis)
        (mymap f (cdr lis) (lambda (v) (return (cons (f (car lis)) v)))))))


; Q7: matrixmultiply
(define matrixmultiply
  (lambda (m1 m2 return)
    (if (null? m1)
        (return '())
        (matrixmultiply (cdr m1) m2 (lambda (v) (return (cons (vectormult (car m1) m2 (lambda (x) x)) v)))))))


; Q8: removesubsequence*
; Sample cmd: (removesubsequence* '(a b) '(w (x b) ((a) ((y z))) b) (lambda (l1 l2) l2))  --> '(w (x b) (() ((y z))))
(define removesubsequence*
  (lambda (lis1 lis2 return)
    (cond
      ((or (null? lis1) (null? lis2)) (return lis1 lis2))
      ((eq? (car lis1) (car lis2)) (removesubsequence* (cdr lis1) (cdr lis2) (lambda (l1 l2) (return l1 l2))))
      ((list? (car lis2)) (removesubsequence* lis1 (car lis2) (lambda (l1 l2) (removesubsequence* l1 (cdr lis2) (lambda (v1 v2) (return v1 (cons l2 v2)))))))
      (else (removesubsequence* lis1 (cdr lis2) (lambda (l1 l2) (return l1 (cons (car lis2) l2))))))))


; Q9: takes an atom and a list and returns a list containing
; all elements that occur after the last occurrence of the atom
(define suffix
  (lambda (a lis)
    (call/cc
     (lambda (k)
       (suffix-break a lis k)))))

(define suffix-break
  (lambda (a lis k)
    (cond
      ((null? lis) '())
      ((eq? a (car lis)) (k (suffix-break a (cdr lis) k)))
      (else (cons (car lis) (suffix-break a (cdr lis) k)))))) 


; Q10: xindex takes atom and list
; returns same list, but with list/sublist contained the atom transformed into list of atom's first occurence in that list
; (xindex 'x '((a b c) (d e x g) (((h i) x) j k ((l m x o)))))  => '((a b c) (3) ((2) j k ((3))))
(define xindex
  (lambda (a lis)
    (call/cc
     (lambda (k)
       (xindex-break a 0 lis k)))))

(define xindex-break
  (lambda (a p lis break)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (cons (call/cc (lambda (k) (xindex-break a 0 (car lis) k))) (xindex-break a (+ p 1) (cdr lis) break)))
      ((eq? a (car lis)) (break (list (+ p 1))))
      (else (cons (car lis) (xindex-break a (+ p 1) (cdr lis) break))))))

















































