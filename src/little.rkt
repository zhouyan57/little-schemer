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
  '((appetizer entree beverage)
    (food tastes good)))

(define identity (lambda (x) x))

(assert (eq? (lookup-in-entry 'entree entry-1 identity)
             'tastes))

(assert (eq? (lookup-in-entry 'dessert entry-1 identity)
             (identity 'dessert)))

(define extend-table cons)

(define table-1
  '(((entree dessert)
     (spaghetti spu moni))
    ((appetizer entree beverage)
     (food tastes good))))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry
             name
             (car table)
             (lambda (name)
               (lookup-in-table name (cdr table) table-f)))))))

(assert (eq? (lookup-in-table 'entree table-1 identity)
             'spaghetti))

(assert (eq? (lookup-in-table 'beverage table-1 identity)
             'good))


;; (define value
;;   (lambda ()
;;     ...))

;; NOTE test interpreter.

(assert (eq? (value '(car (quote (a b c))))
             'a))

(assert (eq? (value '(quote (car (quote (a b c)))))
             '(car (quote (a b c)))))

(assert (eq? (value '(quote (car (quote (a b c)))))
             '(car (quote (a b c)))))

(assert (eq? (value '(add1 6))
             7))

(assert (eq? (value 6)
             7))

(assert (eq? (value '(quote nothing))
             'nothing))

(assert (eq? (value
              '((lambda (nothing)
                  (cons nothing (quote ())))
                (quote
                    (from nothing comes something))))
             '((from nothing comes something))))

(assert (eq? (value
              '((lambda (nothing)
                  (cond
                    (nothing (quote something))
                    (else (quote nothing))))
                #t))
             'something))
