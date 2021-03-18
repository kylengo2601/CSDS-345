#lang racket
; Kyle Ngo (ktn27)


; Q1: inorder? takes a list of numbers
; and returns #t if the numbers are in non-decreasing order

(define inorder?
  (lambda (lis)
    (cond
      ((null? lis) #t)
      ((not (null? (cdr lis)))
          (if (> (car lis) (car (cdr lis)))
              #f
              (inorder? (cdr lis))
              ))
      (else (inorder? (cdr lis))))))


; Q2: dotproduct takes a two vectors (list of numbers) and computes the dot product of
; the vector. If one list is longer than the other, you can ignore the extra numbers
; of the longer list.

(define dotproduct
  (lambda (lis1 lis2)
    (cond
      ((or (null? lis1) (null? lis2)) 0)
      (else (+ (* (car lis1) (car lis2)) (dotproduct (cdr lis1) (cdr lis2)))))))


; Q3: squareroot takes two numbers, a value and an iteration. The iteration will be
; an int >= 0. The method will compute the squareroot of the value using iteration
; rounds of Newton's method, starting with an initial value equal to the input val.
; Newton's method is new = old - ((old * old) - value) / (2 * old)

(define newton
  (lambda (old value)
    (if (zero? old)
        0
        (- old (/ (- (* old old) value) (* 2 old))))))

(define sqrt
  (lambda (old val i)
    (cond
      ((zero? i) old)
      (else (sqrt (newton old val) val (- i 1))))))

(define squareroot
  (lambda (val i)
    (cond
      ((or (zero? val) (zero? i)) val)
      (else (sqrt val val i)))))

            
; Q4: removesubsequence takes two lists of atoms. The first list is a subsequence
; of the second list. The method should return the second list with the first occurence
; of the subsequence removed. So, if the first list is '(a b c), the first a if the
; second list if removed, the first b that appears after the removed a is removed,
; and the first c that appears after the removed b is removed.

(define removesubsequence
  (lambda (lis1 lis2)
    (cond
      ((or (null? lis1) (null? lis2)) lis2)
      ((eq? (car lis1) (car lis2)) (removesubsequence (cdr lis1) (cdr lis2)))
      (else (cons (car lis2) (removesubsequence lis1 (cdr lis2)))))))

; Q5: reverse* takes a nested list and reverses the contents of the list
; and all nested lists

(define reverse*
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((pair? (car lis)) (append (reverse* (cdr lis)) (cons (reverse* (car lis)) '())))
      (else (append (reverse* (cdr lis)) (cons (car lis) '()))))))


; Q6: first* takes a list of lists and returns the first (left most) atom
; that appears in the list, regardless of how nested it is
(define atom?
  (lambda (x)
    (if (and (not (null? x)) (not (pair? x)))
        #t
        #f)))

(define first*
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((atom? (car lis)) (car lis))
      (else (first* (car lis))))))


; Q7: last* takes a list of lists and returns the last (right most) atom
; that appears in the list, regardless of how nested it is. Give a simple solution
; that does not reverse the list.

(define last*
  (lambda (lis)
    (cond
      ((null? lis) lis)
      ((and (null? (cdr lis)) (atom? (car lis))) (car lis))
      ((and (null? (cdr lis)) (pair? (car lis))) (last* (car lis)))
      (else (last* (cdr lis))))))

; from prof
(define last-fix*
  (lambda (lis)
    (cond
      ((not (null? (cdr lis))) (last-fix* (cdr lis)))
      ((pair? (car lis)) (last* (car lis)))
      (else (car lis)))))


; Q8: numorder*? takes a possibly nested list of num, and returns #t if the values
; of the entries in the list and all sublists are in non-decreasing order. The value
; of a number. The value of a list is the sum of the values in the list.

(define subtotal
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((number? (car lis)) (+ (car lis) (subtotal (cdr lis))))
      (else (+ (subtotal (car lis)) (subtotal (cdr lis)))))))

(define flatten
  (lambda (lis)
    (cond
     ((null? lis) '())
     ((pair? (car lis)) (cons (subtotal (car lis)) (flatten (cdr lis))))
     (else (cons (car lis) (flatten (cdr lis)))))))

(define numorder*?
  (lambda (lis)
    (cond
      ((null? lis) #t)
      (else (inorder? (flatten lis))))))

(define numorder-fix?
  (lambda (lis)
    (cond
      ((null? lis) #t)
      ((and (list? (car lis)) (null? (cdr lis))) (numorder-fix? (car lis)))
      ((null? (cdr lis)) #t)
      ((> (valueof (car lis)) (valueof (cadr lis))) #f)
      ((list? (car lis)) (and (numorder-fix? (car lis)) (numorder-fix? (cdr lis))))
      (else (numorder-fix? (cdr lis))))))

(define valueof
  (lambda (x)
    (if (list? x)
        (valueof* x)
        x)))

(define valueof*
  (lambda (lis)
    (cond
      ((null? lis) 0)
      ((list? (car lis)) (+ (valueof* (car lis)) (valueof* (cdr lis))))
      (else (+ (car lis) (valueof* (cdr lis)))))))
      

; Q9: vectormult takes a row vector ( a lis of num) and matrix (a lis of lis of num)
; and multiplies the vector times the matrix. The result is a vector where the ith
; element of the result is the dotproduct of the input vector and the ith col of the
; matrix. You can assume that the length of the vector matches the number of rows
; of the matrix.

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

(define mytranspose
  (lambda (matrix transposematrix)
    (cond
      ((null? matrix) '())
      ((pair? (car matrix)) (mytranspose (mymap cdr matrix) (cons (mymap car matrix) transposematrix)))
      (else (myreverse transposematrix)))))

(define vectormatrix
  (lambda (vector matrix)
    (if (null? matrix)
        '()
        (cons (dotproduct vector (car matrix)) (vectormatrix vector (cdr matrix))))))

(define vectormult
  (lambda (vector matrix)
    (cond
      ((null? matrix) '())
      (else (vectormatrix vector (mytranspose matrix '()))))))


; Q10: matrixmultiply takes two matrices (a list of lists of numbers) and multiplies
; them. You can assume the number of columns of the first matrix is equal to the num
; of rows of the second matrix in the same the sublist.

(define matrixmatrix
  (lambda (mat1 mat2)
    (cond
      ((null? mat1) '())
      (else (cons (vectormatrix (car mat1) mat2) (matrixmatrix (cdr mat1) mat2))))))

(define matrixmultiply
  (lambda (mat1 mat2)
    (cond
      ((null? mat1) '())
      (else (matrixmatrix mat1 (mytranspose mat2 '()))))))
























        