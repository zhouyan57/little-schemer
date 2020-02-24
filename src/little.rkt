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

(assert (eq? (first (build 'a 'b))
             'a))

(assert (eq? (second (build 'a 'b))
             'b))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car values))
      (else (lookup-in-entry-help
             name
             (cdr names)
             (cdr values)
             entry-f)))))

(define entry-1
  (new-entry
   '(appetizer entree beverage)
   '(food tastes good)))

(define identity (lambda (x) x))

(assert (eq? (lookup-in-entry 'entree entry-1 identity)
             'tastes))

(assert (eq? (lookup-in-entry 'dessert entry-1 identity)
             (identity 'dessert)))
