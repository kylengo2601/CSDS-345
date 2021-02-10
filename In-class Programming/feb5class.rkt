#lang racket
; a func that takes an atom and a list and returns true if atom in list and f otherwise

(define member?
  (lambda (a lis)
    (if (null? lis)
        #f
        (if (eq? a (car lis))
            #t
            (member? a (cdr lis))))))

(define member2?
  (lambda (a lis)
    (cond
      ((null? lis)       #f)
      ((eq? a (car lis)) #t)
      (else              (member2? a (cdr lis))))))

; (numocurring 'x '(a x b x c x)) => 3 the number of time the given atom is in the list
(define numoccurring
  (lambda (x lis)
    (cond
      ((null? list) 0)
      ((eq? x (car lis)) (+ 1 (numoccurring x (cdr lis))))
      (else (numoccurring x (cdr lis))))))

; (sumnumbers '(a 3 b c 4 d 5)) => 12 adds the numbers in the list. number? returns #t if the operand is a number
(define sumnumbers
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis))  (+ (car lis) (sumnumbers (cdr lis))))
      (else (sumnumbers (cdr lis))))))


; myremove takes an atom and a list and removes the first occurence of the atom in the list
(define myremove
  (lambda (a lis)
    (cond
      ((null? lis) '())
      ((eq? a (car lis)) (cdr lis))
      (else (cons (car lis) (myremove a (cdr lis)))))))

(define myremoveall
  (lambda (a lis)
    (cond
      ((null? lis) '())
      ((eq? a (car lis)) (myremoveall a (cdr lis)))
      (else (cons (car lis) (myremoveall a (cdr lis)))))))

(define myappend
  (lambda (lis1 lis2)
    (if (null? lis1)
        lis2
        (cons (car lis1) (myappend (cdr lis1) lis2)))))

; rep 5 'a => lis of 5 a's
(define repeat
  (lambda (x a)
    (cond
      ((zero? x) '())
      (else (cons a (repeat (- x 1) a))))))

; squares
(define squares
  (lambda (lis)
    (if (null? lis)
        '()
        (cons (* (car lis) (car lis)) (squares (cdr lis))))))