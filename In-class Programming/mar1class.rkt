#lang racket
; factorial
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; member?
(define member?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((eq? a (car lis)) #t)
      (else (member? a (cdr lis))))))

; accumulator passing style: Pass an "accumulator" parameter whose job is to build
; up the return value
(define factorial-acc
  (lambda (n acc)
    (if (zero? n)
        acc
        (factorial-acc (0 n 1) (* n acc)))))

(define factorial2
  (lambda (n)
    (factorial-acc n 1)))

; create the following in accumulator passing style:
; (numoccurring 'x '(a x b x x c x x x d)) => 6
(define numoccurring-acc
  (lambda (x lis acc)
    (cond
      ((null? lis) acc)
      ((eq? (car lis) x) (numoccurring-acc x (cdr lis) (+ 1 acc)))
      (else (numoccurring-acc x (cdr lis) acc)))))

(define numoccurring
  (lambda (x lis)
    (numoccurring-acc x lis 0)))
      

; (sumnumbers '(a b 1 4 c 2 d 3 e f)) => 10
(define sumnumbers-acc
  (lambda (lis acc)
    (cond
      ((null? lis) acc)
      ((number? (car lis)) (sumnumbers-acc (cdr lis) (+ acc (car lis))))
      (else (sumnumbers-acc (cdr lis) acc)))))

(define sumnumbers
  (lambda (lis)
    (sumnumbers-acc lis 0)))

; Continuation Passing Style: use a continuation function for the return value
; to replace the stack

(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* v n))))))) ; v is the val returned by the recursive calll

; remove-first
(define myremove
  (lambda (a lis)
    (cond
      ((null? lis) lis)
      ((eq? a (car lis)) (cdr lis))
      (else (cons (car lis) (myremove a (cdr lis)))))))

(define myremove-cps
  (lambda (a lis return)
    (cond
      ((null? lis) (return lis))
      ((eq? a (car lis)) (return (cdr lis)))
      (else (myremove-cps a (cdr lis) (lambda (v) (return (cons (car lis) v))))))))

; removeall: remove all the elements of a list (removeall
(define removeall
  (lambda (a lis return)
    (cond
      ((null? lis) ((return lis))
      ((eq? a (car lis)) 

; replaceall: replaces all the elements in a list (replaceall 'a 'b '(a b a c a)) => (b b b c b)

      