
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
;; (number, number, number) -> (list number)
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

;; 2
;; (list str, str) -> (list str)
(define (string-append-map xl suffix)
  (map (lambda (str) (string-append str suffix)) xl))

;; 3
;; (list number, number) -> (number)
(define (list-nth-mod xs n)
  (if (negative? n)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; 4
;; (stream s, number n) -> (list number)
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ((pr (s)))
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; 5
;; () -> Stream
(define funny-number-stream
  (letrec ([neg-five (lambda (x) (if (zero? (remainder x 5)) (- x) x))]
           [f (lambda (x) (cons (neg-five x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 6
;; () -> Stream
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
  dan))

;; 7
;; (stream s) -> (stream)
(define (stream-add-zero s)
  (letrec ( [f (lambda (x) (cons (cons 0 (car x)) (lambda() (f ((cdr x))))))] )
     (lambda () (f (s)))))

;; 8
;; (list xs, list ys) -> (stream)
(define (cycle-lists xs ys)
  (letrec ([pr (lambda (n) (cons (list-nth-mod xs n) (list-nth-mod ys n)))]
           [f (lambda (x) (cons (pr x) (lambda() (f (+ x 1)))))])
    (lambda() (f 0))))

;; 9
;; (number v, vector vec) -> (pair x y) or #f
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([n-th (vector-ref vec n)])
                      (if (and (pair? n-th) (equal? (car n-th) v))
                          n-th
                          (f (+ n 1))))))])
    (f 0)))

;; 10
;; (list xs, number n) -> assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin
                   (vector-set! memo pos new-ans)
                   (set! pos (remainder (+ pos 1) n))
                   new-ans)))))))

;; 11
;; () -> Macro
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([a e1]
              [f (lambda ()
                   (if (>= e2 a)
                       #t
                       (f)))])
       (f))]))