#lang racket

(define assert
  (lambda (x)
    (cond
      ((eq? x #t) 'ok)
      (else (error "assertion fail\n")))))

(define build (lambda (x y) (cons x (cons y '()))))
(define first (lambda (x) (car x)))
(define second (lambda (x) (car (cdr x))))

(define new-entry build)

(assert (eq? (first (build 'a 'b)) 'a))
(assert (eq? (second (build 'a 'b)) 'b))

;; (define lookup-in-entry
;;   (lambda (name entry)
;;     ))
