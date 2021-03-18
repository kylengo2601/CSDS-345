#lang racket

; M-integer maps expressions to integer values
; (M-integer '(+ 3 5)) => 8
; (M-integer '(/ 8 3)) => 2
; (M-iinteger '(* (+ 4 3) (- 2 1))) => 7
; operators +,-,*,/,%


(define M-integer
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? (operator expression) '+) (+ (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '-) (- (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '*) (* (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      (else (error 'bad-operator)))))

; Operators & Operands Abstractions
(define operator (lambda (expression) (car expression)))
(define leftoperand cadr)
(define rightoperand (lambda (expression) (caddr expression)))


; M-boolean map expressions to boolean values
; (M-boolean '(< 1 2)) => #t
; (M-boolean '(<= 2 2)) => #t
; (M-boolean '(>= 3 2)) => #t
; (M-boolean '(> 1 29)) => #f

(define M-boolean
  (lambda (expression)
    (cond
      ((eq? (operator expression) '<) (< (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '<=) (or (< (M-integer (leftoperand expression)) (M-integer (rightoperand expression))) (eq? (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))))
      ((eq? (operator expression) '>) (> (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '>=) (or (> (M-integer (leftoperand expression)) (M-integer (rightoperand expression))) (eq? (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))))
      ((eq? (operator expression) '==) (eq? (M-integer (leftoperand expression)) (M-integer (rightoperand expression))))
      ((eq? (operator expression) '!=) (not (eq? (M-integer (leftoperand expression)) (M-integer (rightoperand expression)))))
      (else (error 'bad-operator)))))