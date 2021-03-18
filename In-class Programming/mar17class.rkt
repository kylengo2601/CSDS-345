#lang racket
; practice flow control by implementing break-type things
; create a function "sum-with-cut" that sums a list of numbers
;
; (sum-with-cut '(1 2 3 4 5)) => 15
; (sum-with-cut '(1 2 3 cut  4 5)) => 15 (because no closing end to the cut
; (sum-with-cut '(1 2 3 cut 4 5 end)) => 6
; (sum-with-cut '(1 2 cut 3 4 end 5)) => 8
; (sum-with-cut '(1 2 3 end 4 5)) => 15 (because no opening cut to the end so end is ignored)
(define sum-with-cut
  (lambda (lis)
    (sumwithcut lis (lambda (v) v))))

(define sumwithcut
  (lambda (lis cut)
    (cond
      ((null? lis) 0)
      ((eq? 'cut (car lis)) (call/cc (lambda (k) (sumwithcut (cdr lis) k))))  ; <- we want to mark this spot to be able to jump to it
      ((eq? 'end (car lis)) (cut (sumwithcut (cdr lis) (lambda (v) v))))  ; <- we want to jump to the cut "mark", call cut with the correct value
      ((eq? 'allow (car lis)) (sumwithcut (cdr lis) (lambda (v) (cut (+ (cadr lis) v)))))
      (else (+ (car lis) (sumwithcut (cdr lis) cut))))))
    


; allow inside a sum/cut allows the next value to be included
; allow outside a sum/cut is ignored
; (sum-with-cut '(1 2 cut 3 4 allow 5 6 allow  7 8 end 9)) => (1 + 2 + 5 + 7 + 9) = 24
