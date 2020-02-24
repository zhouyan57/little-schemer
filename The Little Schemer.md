# The Little Schemer

---

- The Ten Commandments

    ---

    - The First Commandment

        Always ask null? as the first question in expressing any function.

    - The Second Commandment

        Use cons to build lists.

    - The Third Commandment

        When building a list, describe the first typical ele­ ment, and then cons it onto the natural recursion.

        ![The%20Little%20Schemer/Untitled.png](The%20Little%20Schemer/Untitled.png)

    - The Fourth Commandment

        Always change at least one argument while recurring. It must be changed to be closer to termination. The changing argument must be tested in the termination condition:

        when using cdr, test termination with null?.

    - The Fifth Commandment

        When building a value with + , always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition.

        When building a value with x , always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication.

        When building a value with cons , always consider ( ) for the value of the terminating line.

    - The Sixth Commandment

        Simplify only after the function is correct.

    - The Seventh Commandment

        Recur on the subparts that are of the same nature:

        - On the sublists of a list.
        - On the subexpressions of an arithmetic expression.
    - The Eighth Commandment

        Use help functions to abstract from representations.

    - The Ninth Commandment

        Abstract common patterns with a new function.

- The Five Rules

    ---

    - The Law of Car

        The primitive car is defined only for non­ empty lists.

    - The Law of Cdr

        The primitive cdr is defined only for non­ empty lists. The cdrof any non-empty list is always another list.

    - The Law of Cons

        The primitive cons takes two arguments. The second argument to cons must be a list. The result is a list.

    - The Law of Null?

        The primitive null? is defined only for lists.

    - The Law of Eq?

        The primitive eq'l takes two arguments. Each must be a non-numeric atom.

## C1.Toys

---

- atom
- list
- S-expressions
- car
- cdr
- cons
    - (cons a l) → "cons the atom a onto the list l"
    - cons adds any S-expression to the front of a list.
    - The second argument is any list.
- null?
    - defined only for lists.
- (quote ())
    - a notation for the null list.
- (eq? a1 a2)
    - Are a1 and a2 the same non-numeric atom?

## C2. Do it, Do it again, and again...

---

- lat
    - a lat is a list of atoms.

        (define lat?
          (lambda (l)
            (cond
              ((null? l) #t)
              ((atom? (car l)) (lat? (cdr l)))
              (else #f))))

- Note
    - (cond . . . ) asks questions;
    - (lambda . . . ) creates a function;
    - (define . . . ) gives it a name.
- else
    - else asks if else is true → Yes, because the question else is always true.
- member?

        (define member?
          (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? a (car lat))
                        (member? a (cdr lat)))))))

## C3. Cons the Magnificent

---

- (rember a lat)

    (define rember
      (lambda (a lat)
        (cond
          ((null? lat) (quote ()))
          (else (cond
                  ((eq? (car lat) a) (cdr lat))
                  (else (cons (car lat)
                              (rember a (cdr lat)))))))))

- (firsts l)

    (define firsts
      (lambda (l)
        (cond
          ((null? l) (quote()))
          (else (cons (car (car l))
                      (firsts (cdr l)))))))

- (insertR new old lat)

    (define insertR
      (lambda (new old lat)
        (cond
          ((null? lat) (quote ()))
          (else
           (cond
             ((eq? (car lat) old)
              (cons old
                    (cons new (cdr lat))))
             (else (cons (car lat)
                         (insertR new old
                                  (cdr lat)))))))))

- The questions we ask about the lat
    - (null? lat)
    - else
- if (null? lat) is not true, lat has at least one element
    - (eq? (car lat) old)
    - else
- subst

    (define insertR
      (lambda (new old lat)
        (cond
          ((null? lat) (quote ()))
          (else
           (cond
             ((eq? (car lat) old)
              (cons old
                    (cons new (cdr lat))))
             (else (cons (car lat)
                         (insertR new old
                                  (cdr lat)))))))))

- subst2
    - replaces either the first occurrence of o1 or the first occurrence of o2 by new

    (define subst2
      (lambda (new o1 o2 lat)
        (cond
          ((null? lat)(quote ()))
          (else (cond
                  ((or (eq? (car lat) o1) (eq? (car lat) o2))
                   (cons new (cdr lat)))
                  (else (cons (car lat)
                              (subst2 new o1 o2
                                      (cdr lat)))))))))
    
    Replace the two eq� lines about the (car lat) by
    ((or (eq� (car lat) o1) (eq� (car lat) o2)) (cons new (cdr lat))).

- multirember

    (define multirember
      (lambda (a lat)
        (cond
          ((null? lat) (quote ()))
          (else
           (cond
             ((eq? (car lat) a)
              (multirember a (cdr lat)))
             (else (cons (car lat)
                         (multirember a
                                      (cdr lat)))))))))

- multiinsertR

    (define multiinsertR
      (lambda (new old lat)
        (cond
          ((null? lat) (quote ()))
          (else
           (cond
             ((eq? (car lat) old)
              (cons (car lat)
                    (cons new
                          (multiinsertR new old
                                        (cdr lat)))))
             (else (cons (car lat)
                         (multiinsertR new old
                                       (cdr lat)))))))))

- multiinsertL

        (define multiinsertL
          (lambda (new old lat)
            (cond
              ((null? lat)(quote ()))
              (else
               (cond
                 ((eq? (car lat) old)
                  (cons new
                        (cons old
                              (multiinsertL new old
                                            (cdr lat)))))
                 (else (cons (car lat)
                             (multiinsertL new old
                                           (cdr lat)))))))))

## C4.Numbers Games

---

- numbers
    - nonnegative integers
- (add1 n)
- (sub1 n)
- (zero? n)
- (+ n m)

        (define +
          (lambda (n m)
            (cond
              ((zero? m) n)
              (else (add1 (+ n (sub1 m)))))))

- (- n m)

        (define -
          (lambda (n m)
            (cond
              ((zero? m) n)
              (else (sub1 (- n (sub1 m)))))))

- tup → tupple
    - a list of numbers. ()✅
- addtup
    - It builds a number by totaling all the numbers in its argument.

        (define addtup
          (lambda (tup)
            (cond
              ((null? tup) 0)
              (else (+ (car tup) (addtup (cdr tup)))))))

- What is the natural way to build numbers from a list?
    - Use + in place of cons : + builds numbers in the same way as cons builds lists.
- ✖️

        (define x
          (lambda (n m)
            (cond
              ((zero? m) 0)
              (else (+ n (x n (sub1 m)))))))

- tup+          (tups with the same length)

        (define tup+
          (lambda (tup1 tup2)
            (cond
              ((and (null? tup1) (null? tup2))
               (quote ()))
              (else
                (cons (+ (car tup1) (car tup2))
                      (tup+
                       (cdr tup1) (cdr tup2)))))))

- tup+            (work for any two tups)

        (define tup+
          (lambda (tup1 tup2)
            (cond
              ((and (null? tup1) (null? tup2))
               (quote ()))
              ((null? tup1) tup2)
              ((null? tup2) tup1)
              (else
               (cons (+ (car tup1) (car tup2))
                     (tup+
                      (cdr tup1) (cdr tup2)))))))

- >

        (define >
          (lambda (n m)
            (cond
              ((zero? n) #f)
              ((zero? m) #t)
              (else (> (sub1 n) (sub1 m))))))

- =

        (define =
          (lambda (n m)
            (cond
              ((zero? m) (zero? n))
              ((zero? n) #f)
              (else (= (sub1 n) (sub1 m))))))

- ⬆️

    ![The%20Little%20Schemer/Untitled%201.png](The%20Little%20Schemer/Untitled%201.png)

- ➗

    ![The%20Little%20Schemer/Untitled%202.png](The%20Little%20Schemer/Untitled%202.png)

- length

        (define length
          (lambda (lat)
            (cond
              ((null? lat) 0)
              (else (add1 (length (cdr lat)))))))

- pick

        (define pick
          (lambda (n lat)
            (cond
              ((zero? (sub1 n)) (car lat))
              (else (pick (sub1 n) (cdr lat))))))

- rempick

        (define rempick
          (lambda (n lat)
            (cond
              ((zero? (sub1 n)) (cdr lat))
              (else (cons (car lat)
                          (rempick (sub1 n) (cdr lat)))))))

- no-nums

        (define no-nums
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              (else (cond
                      ((number? (car lat))
                       (no-nums (cdr lat)))
                      (else (cons (car lat)
                                  (no-nums
                                   (cdr lat)))))))))

- all-nums

        (define all-nums
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              (else
               (cond
                 ((number? (car lat))
                  (cons (car lat)
                        (all-nums (cdr lat))))
                 (else (all-nums (cdr lat))))))))

- eqan?
    - is true if its two arguments (a1 and a2) are the same atom. Remember to use = for numbers and eq? for all other atoms.

        (define eqan?
          (lambda (a1 a2)
            (cond
              ((and (number? a1) (number? a2))
               (= a1 a2))
              ((or (number? a1) (number? a2)) #f)
              (else (eq? a1 a2)))))

- occur
    - counts the number of times an atom a appears in a lat

        (define occur
          (lambda (a lat)
            (cond
              ((null? lat) 0)
              (else
               (cond
                 ((eq? (car lat) a)
                  (add1 (occur a (cdr lat))))
                 (else (occur a (cdr lat))))))))

- one?

        (define one?
          (lambda (n)
            (cond
              ((zero? n) #f)
              (else (zero? (sub1 n))))))
        
        (define one?
          (lambda (n)
            (= n 1)))

- rempick

        (define rempick
          (lambda (n lat)
            (cond
              ((one? n) (cdr lat))
              (else (cons (car lat)
                          (rempick (sub1 n) (cdr lat)))))))

## C5. Oh My Gawd*: It's Full of Stars

---

- rember*

        (define rember*
          (lambda (a l)
            (cond
              ((null? l) (quote ()))
              ((atom? (car l))
               (cond
                 ((eq? (car l) a)
                  (rember* a (cdr l)))
                 (else (cons (car l)
                             (rember* a (cdr l))))))
              (else (cons (rember* a (car l))
                          (rember* a (cdr l)))))))

- insertR*

        (define insertR*
          (lambda (new old l)
            (cond
              ((null? l) (quote ()))
              ((atom? (car l))
               (cond
                 ((eq? (car l) old)
                  (cons old
                        (cons new
                              (insertR* new old
                                        (cdr l)))))
                 (else (cons (car l)
                             (insertR* new old
                                       (cdr l))))))
              (else (cons (insertR* new old
                                    (car l))
                          (insertR* new old
                                    (cdr l)))))))

- How are all *-functions similar?
    - They all ask three questions and recur with the car as well as with the cdr, whenever the car is a list.
- occur*

        (define occur*
          (lambda (a l)
            (cond
              ((null? l) 0)
              ((atom? (car l))
               (cond
                 ((eq? (car l) a)
                  (add1 (occur* a (cdr l))))
                 (else (occur* a (cdr l)))))
              (else (+ (occur* a (car l))
                       (occur* a (cdr l)))))))

- subst*

        (define subst*
          (lambda (new old l)
            (cond
              ((null? l) (quote ()))
              ((atom? (car l))
               (cond
                 ((eq? (car l) old)
                  (cons new (subst* new old (cdr l))))
                 (else
                  (cons (car l) (subst* new old (cdr l))))))
               (else
                (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

- member*

        (define member*
          (lambda (a l)
            (cond
              ((null? l) #f)
              ((atom? (car l))
               (or (eq? (car l) a)
                   (member* a (cdr l))))
              (else (or (member* a (car l))
                        (member* a (cdr l)))))))

- leftmost

        (define leftmost
          (lambda (l)
            (cond
              ((atom? (car l)) (car l))
              (else (leftmost (car l))))))

- eqlist?

        (define eqlist?
          (lambda (l1 l2)
            (cond
              ((and (null? l1) (null? l2)) #t)
              ((or (null? l1) (null? l2)) #f)
              ((and (atom? (car l1)) (atom? (car l2)))
               (and (eqan? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2))))
              ((or (atom? (car l1)) (atom? (car l2))) #f)
              (else
               (and (eqlist? (car l1) (car l2))
                    (eqlist? (cdr l1) (cdr l2)))))))
        
        (define eqlist?
          (lambda (l1 l2)
            (cond
              ((and (null? l1) (null? l2)) #t)
              ((or (null? l1) (null? l2)) #f)
              (else
               (and (equal? (car l1) (car l2))
                    (eqlist (cdr l1) (cdr l2)))))))
        

- equal?
    - How many questions does equal? ask to determine whether two S-expressions are the same?
        - Four. The first argument may be an atom or a list of S-expressions at the same time as the second argument may be an atom or a list of S-expresssions.

        (define equal?
          (lambda (s1 s2)
            (cond
              ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
              ((or (atom? s1) (atom? s2)) #f)
              (else (eqlist? s1 s2)))))

- rember
    - Here is rember after we replace lat by a list l of S-expressions and a by any S-expression.

        (define rember
          (lambda (s l)
            (cond
              ((null? l) (quote ()))
              (else (cond
                      ((equal? (car l) s) (cdr l))
                      (else (cons (car l) (rember s (cdr l)))))))))
        
        (define rember
          (lambda (s l)
            (cond
              ((null? l) (quote ()))
              (equal? (car l) s) (cdr l))
            (else (cons (car l) (rember s (cdr l))))))

## C6.Shadows

---

- arithmetic expression
    - an arithmetic expression is either an atom (including numbers), or two arithmetic expressions combined by + , x , or ⬆️.
- numbered
    - It is a function that determines whether a representation of an arithmetic expression contains only numbers besides the +, x , and ↑.

        (define numbered?
          (lambda (aexp)
            (cond
              ((atom? aexp) (number? aexp))
              (else
               (and (numbered? (car aexp))
                    (numbered? (car (cdr (cdr aexp)))))))))

- value

        (define value
          (lambda (nexp)
            (cond
              ((atom? nexp) nexp)
              ((eq? (car (cdr nexp)) (quote +))
               (+ (value (car nexp))
                  (value (car (cdr (cdr nexp))))))
              ((eq? (car (cdr nexp)) (quote x))
               (x (value (car nexp))
                  (value (car (cdr (cdr nexp))))))
              (else
               (↑ (value (car nexp))
                  (value
                   (car (cdr (cdr nexp)))))))))

- rewrite value
    - 1st-sub-exp
    - 2nd-sub-exp
    - operator

        (define 1st-sub-exp
          (lambda (aexp)
            (car (cdr aexp))))
        
        (define 2nd-sub-exp
          (lambda (aexp)
            (car (cdr (cdr aexp)))))
        
        (define operator
          (lambda (aexp)
            (car aexp)))
        
        (define value
          (lambda (nexp)
            (cond
              ((atom? nexp) nexp)
              ((eq? (operator nexp) (quote +))
               (+ (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp))))
              ((eq? (operator nexp)
                    (quote x))
               (x (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp))))
              (else
               (↑ (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp)))))))

## C7. Friends and Relations

---

- set

        (define set?
          (lambda (lat)
            (cond
              ((null? lat) #t)
              ((member? (car lat) (cdr lat)) #f)
              (else (set? (cdr lat))))))

- makeset

        (define set?
          (lambda (lat)
            (cond
              ((null? lat) #t)
              ((member? (car lat) (cdr lat)) #f)
              (else (set? (cdr lat))))))

- make set  (using multirember)

        (define makeset
          (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              (else (cons (car lat)
                          (makeset (multirember (car lat)
                                                (cdr lat))))))))

- subset?

        (define subset?
          (lambda (set1 set2)
            (cond
              ((null? set1) #t)
              ((member? (car set1) set2)
               (subset? (cdr set1) set2))
              (else #f))))
        
        (define subset?
          (lambda (set1 set2)
            (cond
              ((null? set1) #t)
              (else
               (and (number? (car set1) set2)
                    (subset? (cdr set1) set2))))))

- eqset?

        (define eqset?
          (lambda (set1 set2)
            (subset? set2 set1)))

- intersect
    - at least one atom in set1 is in set2.

        (define intersect?
          (lambda (set1 set2)
           (cond
             ((null? set1) #f)
             (else (or (member? (car set1) set2)
                       (intersect? (cdr set1) set2))))))

- union

        (define union
          (lambda (set1 set2)
            (cond
              ((null? set1) set2)
              ((member? (car set1) set2)
               (union (cdr set1) set2))
              (else (cons (car set1)
                          (union (cdr set1) set2))))))

- intersectall

        (define intersectall
          (lambda (l-set)
            (cond
              ((null? (cdr l-set)) (car l-set))
              (else (intersect (car l-set)
                               (intersectall (cdr l-set)))))))

- a-pair?
    - it is a list with only two S-expressions.

        (define a-pair?
          (lambda (x)
            (cond
              ((atom? x) #f)
              ((null? x) #f)
              ((null? (cdr x)) #f)
              ((null? (cdr (cdr x))) #t)
              (else #f))))

- first
- second
- build

        (define first
          (lambda (p)
            (cond
              (else (car p)))))
        
        (define second
          (lambda (p)
            (cond
              (else (car (cdr p))))))
        
        (define build
          (lambda (s1 s2)
            (cond
              (else (cons s1
                          (cons s2 (quote ())))))))
        
        (define third
          (lambda (l)
            (car (cdr (cdr l)))))

- rel
    - a list of pairs
- fun →function
    - (fun? rel) #t → (firsts rel) is a set

        (define fun?
          (lambda (rel)
            (set? (firsts rel))))

- (revrel rel)

        (define revrel
          (lambda (rel)
            (cond
              ((null? rel) (quote ()))
              (else (cons (build
                           (second (car rel))
                           (first (car rel)))
                          (revrel (cdr rel)))))))
        ;;using revpair
        (define revrel
          (lambda (rel)
            (cond
              ((null? rel) (quote ()))
              (else (cons (revpair (car rel))
                          (revrel (cdr rel)))))))

- revpair
    - reversed the two components of a pair

        (define revpair
          (lambda (pair)
            (build (second pair) (first pair))))

- fullfun?

        (define fullfun?
          (lambda (fun)
            (set? (seconds fun))))

- one-to-one
    - another name for fullfun

        (define one-to-one?
          (lambda (fun)
            (fun? (revrel fun))))

## C8.Lambda the Ultimate

---

- rember-f

        (define rember-f
          (lambda (test? a l)
            (cond
              ((null? l) (quote ()))
              ((test? (car l) a) (cdr l))
              (else (cons (car l)
                          (rember-f test? a
                                    (cdr l)))))))
        
        (define rember-f
          (lambda (test?)
            (lambda (a l)
              (cond
                ((null? l) (quote ()))
                ((test? (car l) a) (cdr l))
                (else (cons (car l)
                            ((rember-f test?) a
                                              (cdr l))))))))

    ![The%20Little%20Schemer/Untitled%203.png](The%20Little%20Schemer/Untitled%203.png)

- insertL-f  insertR-f

        (define insertL-f
          (lambda (test?)
            (lambda (new old l)
              (cond
                ((null? l) (quote ()))
                ((test? (car l) old)
                 (cons new (cons old (cdr l))))
                (else (cons (car l)
                            ((insertL-f test?) new old (cdr l))))))))

- seqL seqR

        (define seqL
          (lambda (new old l)
            (cons new (cons old l))))
        
        (define seqR
          (lambda (new old l)
            (cons old (cons new l))))

- insert-g

        (define insert-g
          (lambda (seq)
            (lambda (new old l)
              (cond
                ((null? l) (quote ()))
                ((eq? (car l) old)
                 (seq new old (cdr l)))
                (else (cons (car l)
                            ((insert-g seq) new old
                                            (cdr l))))))))

- define **insertL** with insert-g

        (define insertL (insert-g seqL))

- Define insertL again with insert-g. Do not pass in seqL this time.

        (define insertL
          (insert-g
           (lambda (new old l)
             (cons new (cons old l)))))

- subst

        (define subst
          (lambda (new old lat)
            (cond
              ((null? lat) (quote ()))
              (else (cond
                      ((eq? (car lat) old)
                       (cons new (cdr lat)))
                      (else (cons (car lat)
                                  (subst new old (cdr lat)))))))))

    - SeqS  (like seqL or seqR)

            (define seqS
              (lambda (new old l)
                (cons new l)))

        - define substance using insert-g

                (define subst (insert-g seqS))

- seqrem

        (define seqrem
          (lambda (new old l) l))

    - rember

            (define rember
              (lambda (a l)
                ((insert-g seqrem) #f a l)))

- value   (C6.)

        (define value
          (lambda (nexp)
            (cond
              ((atom? nexp) nexp)
              ((eq? (operator nexp) (quote +))
               (+ (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp))))
              ((eq? (operator nexp)
                    (quote x))
               (x (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp))))
              (else
               (↑ (value (1st-sub-exp nexp))
                  (value (2nd-sub-exp nexp)))))))

    - atom-to-function

            (define atom-to-function
              (lambda (x)
                (cond
                  ((eq? x (quote +)) + )
                  ((eq? x (quote x)) x)
                  (else ↑))))

        - value   (using atom-to-function)

                (define value
                  (lambda (nexp)
                    (cond
                      ((atom? nexp) nexp)
                      (else
                       ((atom-to-function
                         (operator nexp))
                        (value (1st-sub-exp nexp))
                        (value (2nd-sub-exp nexp)))))))

- multirember-f

        (define multirember-f
          (lambda (test?)
            (lambda (a lat)
              (cond
                ((null? lat) (quote ()))
                ((test? a (car lat))
                 (multirember-f test?) a (cdr lat)))
              (else (cons (car lat)
                          ((multirember-f test?) a (cdr lat)))))))

- multirember T
    - test? is eq?-tuna

        (define multiremberT
          (lambda (test? lat)
            (cond
              ((null? lat) (quote ()))
              ((test? (car lat))
               (multiremberT **test?** (cdr lat)))
              (else (cons (car lat)
                          (multiremberT test?
                                        (cdr lat)))))))

## C9. ...and Again, and Again, and Again...

---

- What is a partial function?
    - Is align a partial function? No, it yields a value for every argument.
    - 从这句话可以看出来
    - A function is partial function, if it does not yield a value for some argument.
    - 每个函数都有定义域
    - 如果定义域中的所有值 这个函数作用上去之后 都能成功地返回一个结果
    - 那么就是 total function
    - 如果定义域中有一些值 这个函数作用上去之后 返回不了结果
    - 比如出错
    - 或者死循环
    - 那么就是 partial function
    - 假设 car 的定义域是 pair
    - 那 car 就是 total function
    - 假设 car 的定义域是 list, 那 car 就是 partial function
    - 因为 list 里面有 null
    - 它处理不了 会出错
- "unnatural" recursion does not recur on a part of lat.
- pora
    - pora 对于 align 来说应该是 a-pair*?
    - a-pair* is (atom) or (a-pair whose first and second are still (atom or a-pair*))

        (define a-pair*?
          (lambda (x)
            (cond
              ((atom? x) #t)
              ((a-pair? x) (and (a-pair*? (first x)) (a-pair*? (second x))))
              (else #f))))

- looking   (partial functions)

        (define looking
          (lambda (a lat)
            (keep-looking a (pick 1 lat) lat)))

- keep-looking

        (define keep-looking
          (lambda (a sorn lat)
            (cond
              ((number? sorn)
               (keep-looking a (pick sorn lat) lat))
              (else (eq? sorn a)))))

- shift

        (define shift
          (lambda (pair)
            (build (first (first pair))
                   (build (second (first pair))
                          (second pair)))))

- align

        (define align
          (lambda (pora)
            (cond
              ((atom? pora) pora)
              ((a-pair? (first pora))
               (align (shift pora)))
              (else (build (first pora)
                           (align (second pora)))))))

    - Why are we not guaranteed that align makes progress?
        - In the second cond-line shift creates an argument for align that is not a part of the original argument.
        - It violates the Seventh Commandment.
- length*

        (define length*
          (lambda (pora)
            (cond
              ((atom? pora) 1)
              (else
               (+ (length* (first pora))
                  (length* (second pora)))))))

    - (a (a b c))
    - (+ 1 (length* (a b c)))????
- weight*

        (define weight*
          (lambda (pora)
            (cond
              ((atom? pora) 1)
              (else
               (+ (x (weight* (first pora)) 2)
                  (weight* (second pora)))))))

1. 154 →Yes, the weight*'s of align's arguments become successively smaller.
- shuffle

        (define shuffle
          (lambda (pora)
            (cond
              ((atom? pora) pora)
              ((a-pair? (first pora))
               (shuffle (revpair pora)))
              (else (build (first pora)
                           (shuffle (second pora)))))))

- A

        (define A
          (lambda (n m)
            (cond
              ((zero? n) (add1 m))
              ((zero? m) (A (sub1 n) 1))
              (else (A (sub1 n)
                       (A n (sub1 m)))))))

- eternity

        (define eternity
          (lambda (x)
            (eternity x)))

- length

        (define length
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))

- length0

    It determines the length of the empty list and nothing else.

        (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (eternity (cdr l)))))))

- length ≤1

        (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length0 (cdr l)))))))

    - replace length0 by its definition.

            (lambda (l)
                (cond
                  ((null? l) 0)
                  (else (add1
                         ((lambda (l)
                            (cond
                              ((null? l) 0)
                              (else (add1
                                     (eternity (cdr l))))))
                          (cdr l))))))

- length≤2
    - replace eternity with the next version of length.

        (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1
                     ((lambda (l)
                        (cond
                          ((null? l) 0)
                          (else (add1
                                 ((lambda (l)
                                    (cond
                                      ((null? l) 0)
                                      (else
                                       (add1
                                        (eternity
                                         (cdr l))))))
                                  (cdr l))))))
                      (cdr l))))))

- All these programs contain a function that looks like length. Perhaps we should abstract out this function: see The Ninth Commandment .
    - We need a function that looks just like length but starts with (lambda (length) . . . ).
    - length0
        - 这是一个函数作用,不是函数定义,这个函数作用会返回一个函数,返回的函数可以用来计算list的length,但是只能算到0
        - 两层lambda,里层是被返回的函数,eternity 就是死循环函数

        ((lambda (length)
           (lambda (l)
             (cond
               ((null? l) 0)
               (else (add1 (length (cdr l)))))))
         eternity)

    - length≤1

        ((lambda (f)
           (lambda (l)
             (cond
               ((null? l) 0)
               (else (add1 (f (cdr l)))))))
         ((lambda (g)
            (lambda (l)
              (cond
                ((null? l) 0)
                (else (add1 (g (cdr l)))))))
          eternity))

    - length0  (make length)

            ((lambda (mk-length)
              (mk-length eternity))
            (lambda (length)
              (lambda (l)
                (cond
                  ((null? l) 0)
                  (else (add1 (length (cdr l))))))))

    - length≤1

            ((lambda (mk-length)
               (mk-length
                (mk-length eternity)))
             (lambda (length)
               (lambda (l)
                 (cond
                   ((null? l) 0)
                   (else (add1 (length (cdr l))))))))

    ## C10. What Is the Value of All of This?

    ---

    - entry
        - a pair of lists whose first list is a set. Also, the two lists must be of equal length.
    - table
        - A table (also called an environment) is a list of entries.
    - lookup-in-entry-help

            (define lookup-in-entry-help
              (lambda (name names values entry-f)
                (cond
                  ((null? names) (entry-f name))
                  ((eq? (car names) name)
                   (car values))
                  (else (lookup-in-entry-help name
                                              (cdr names)
                                              (cdr values)
                                              entry-f)))))

    - lookup-in-entry

            (define lookup-in-entry
              (lambda (name entry entry-f)
                (lookup-in-entry-help name
                                      (first entry)
                                      (second entry)
                                      entry-f)))