#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

;; The interpreter.

;; Classification of expressions.

;; NOTE just like in lambda calculus,
;;   where we have the following classification of expressions.
;; exp := identifier
;;     | (lambda (identifier) exp) ;; lambda abstraction
;;     | (exp exp) ;; lambda application

;; We represent types by functions,
;;   and we call them "actions".

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    ))

(define *identifier
  (lambda (e table)
    ))

(define *lambda
  (lambda (e table)
    ))

(define *application
  (lambda (e table)
    ))

(define *cond
  (lambda (e table)
    ))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1?) *const)
      ((eq? e 'sub1?) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; NOTE Test the interpreter.

(assert (eq? (value '(car (quote (a b c))))
             'a))

(assert (eq? (value '(quote (car (quote (a b c)))))
             '(car (quote (a b c)))))

(assert (eq? (value '(quote (car (quote (a b c)))))
             '(car (quote (a b c)))))

(assert (eq? (value '(add1 6))
             7))

(assert (eq? (value 6)
             6))

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

(assert (eq? (value 'car)
             '(primitive car)))
