#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define test "t")
(define ones (lambda () (cons 1 ones)))
(define a 2)

;(define (memoize f)
;  (let ((computed '()))
;    (lambda (x)
;      (let ((ans (assoc x computed)))
;        (if ans
;            (cdr ans)
;            (let ((result (f x)))
;              (begin (set! computed (cons (cons x result) computed))
;                     result)))))))
;
;(define memo-fib
;  (memoize
;   (lambda (x) (if (or (= x 0) (= x 1))
;                   x
;                   (+ (memo-fib (- x 1))
;                      (memo-fib (- x 2)))))))

;; Sorry but i love round paranthesis more than square bracket

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond ((null? xs) (error "list-nth-mod: empty list"))
        ((< n 0)    (error "list-nth-mod: negative number"))
        (else (let ((ans (list-tail xs (remainder n (length xs)))))
                (car ans)))))
                

;; Problem 4
(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (cons (car (stream))
            (stream-for-n-steps (cdr (stream)) (- n 1)))))

;; Problem 5
(define (funny-number-stream)
  (define (div-by-5? num)
    (if (= (remainder num 5) 0)
        (- num)
        num))
  (define (helper num)
    (lambda () (cons (div-by-5? num)
                     (helper (+ num 1)))))
  ((helper 1)))

;; Problem 6
(define dan-then-dog
  (lambda () (cons "dan.jpg"
                   (lambda () (cons "dog.jpg"
                                    dan-then-dog)))))

;; Problem 7
(define (stream-add-zero stream)
  (lambda () (cons (cons 0 (car (stream)))
                   (stream-add-zero (cdr (stream))))))

;; Problem 8
(define (cycle-lists xs ys)
  (define (helper num)
    (lambda () (cons (cons (list-nth-mod xs num) (list-nth-mod ys num))
                     (helper (+ num 1)))))
  (helper 0))

;; Problem 9
(define (vector-assoc v vec)
  (let ((vec-len (vector-length vec)))
    (define (loop pos)
      (cond ((>= pos vec-len) #f)
            ((and (pair? (vector-ref vec pos))
                  (equal? v (car (vector-ref vec pos)))) (vector-ref vec pos))
            (else (loop (+ pos 1)))))
    (loop 0)))

;; Problem 10
(define (cached-assoc xs n)
  (let ((vec (make-vector n #f))
        (cur-pos 0))
    (define (assoc-2 v)
      (let ((ans (vector-assoc v vec)))
        (if ans
            ans
            (let ((ans-2 (assoc v xs)))
              (vector-set! vec cur-pos ans-2)
              (set! cur-pos (remainder (+ cur-pos 1) n))
              ans-2))))
    assoc-2))

(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
     (let ((x-ans x))
       (letrec ((loop (lambda (ans)
                         (let ((y-ans (ans)))
                           (if (>= y-ans x-ans)
                               #t
                               (loop ans))))))
         (loop (lambda () y)))))))

; (while-less 7 do (begin (set! a (+ a 1)) a))
; (display a)