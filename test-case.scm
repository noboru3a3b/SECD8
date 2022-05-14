(test-start)
T

;;;;;;;;;;;;;;;;;;;;
;;; test1-1
(quote a)
A

(if true 'a 'b)
A

(if false 'a 'b)
B

(car '(a b c))
A

(cdr '(a b c))
(B C)

(cons 'a 'b)
(A . B)

(eq? 'a 'a)
TRUE

(eq? 'a 'b)
FALSE

(pair? '(a b c))
TRUE

(pair? 'a)
FALSE


;; test1-2
(define a 'b)
A

a
B

(lambda (x) x)
(CLOSURE (LD (0 . 0) RTN) NIL)

((lambda (x) x) 'a)
A

(list 'a 'b 'c 'd 'e)
(A B C D E)


;; test1-3
(define x 'a)
X

x
A

(define foo (lambda () x))
FOO

(foo)
A

(define bar (lambda (x) (foo)))
BAR

(bar 'b)
A

foo
(CLOSURE (LDG X RTN) NIL)

bar
(CLOSURE (ARGS 0 LDG FOO TAPP RTN) NIL)


;; test1-4
(define baz (lambda (x) (lambda (y) (cons x y))))
BAZ

(define baz-a (baz 'a))
BAZ-A

(baz-a 'b)
(A . B)

(baz-a 'c)
(A . C)

baz
(CLOSURE (LDF (LD (1 . 0) LD (0 . 0) ARGS 2 LDG CONS TAPP RTN) RTN)
         NIL)

baz-a
(CLOSURE (LD (1 . 0) LD (0 . 0) ARGS 2 LDG CONS TAPP RTN) ((A)))


;; test1-5
(append '(a b c) '(d e f))
(A B C D E F)

(append '((a b) (c d)) '(e f g))
((A B) (C D) E F G)

(reverse '(a b c d e))
(E D C B A)

(reverse '((a b) c (d e)))
((D E) C (A B))

(memq 'a '(a b c d e))
(A B C D E)

(memq 'c '(a b c d e))
(C D E)

(memq 'f '(a b c d e))
FALSE

(assq 'a '((a 1) (b 2) (c 3) (d 4) (e 5)))
(A 1)

(assq 'e '((a 1) (b 2) (c 3) (d 4) (e 5)))
(E 5)

(assq 'f '((a 1) (b 2) (c 3) (d 4) (e 5)))
FALSE

(map car '((a 1) (b 2) (c 3) (d 4) (e 5)))
(A B C D E)

(map cdr '((a 1) (b 2) (c 3) (d 4) (e 5)))
((1) (2) (3) (4) (5))

(map (lambda (x) (cons x x)) '(a b c d e))
((A . A) (B . B) (C . C) (D . D) (E . E))

(filter (lambda (x) (not (eq? x 'a))) '(a b c a b c a b c))
(B C B C B C)

(fold-left cons '() '(a b c d e))
(((((NIL . A) . B) . C) . D) . E)

(fold-right cons '() '(a b c d e))
(A B C D E)



;;; test 2-1
(define a '(1 2 3))
A

`(a b c)
(A B C)

`(,a b c)
((1 2 3) B C)

`(,@a b c)
(1 2 3 B C)

`(,(car a) b c)
(1 B C)

`(,(cdr a) b c)
((2 3) B C)

`(,@(cdr a) b c)
(2 3 B C)


;; test2-2
(define a 0)
A

(define b 1)
B

(let ((a 10) (b 20)) (cons a b))
(10 . 20)

a
0

b
1


;; test2-3
(and 1 2 3)
3

(and false 2 3)
FALSE

(and 1 false 3)
FALSE

(and 1 2 false)
FALSE

(or 1 2 3)
1

(or false 2 3)
2

(or false false 3)
3

(or false false false)
FALSE


;; test2-4
(let* ((a 10) (b 20) (c (cons a b))) c)
(10 . 20)

(reverse '(a b c d e))
(E D C B A)

(cons 1 (reverse '()))
(1)

(letrec ((a a)) a)
:UNDEF

(define reversei
  (lambda (ls)
    (let loop ((ls ls) (a '()))
      (if (null? ls)
          a
          (loop (cdr ls) (cons (car ls) a))))))
REVERSEI

(reversei '(a b c d e))
(E D C B A)

(cons 1 (reversei '()))
(1)

(begin)
:UNDEF

(begin 1 2 3 4 5)
5


;; test2-5
(define cond-test
  (lambda (x)
    (cond ((eq? x 'a) 1)
          ((eq? x 'b) 2)
          ((eq? x 'c) 3)
          (else 0))))
COND-TEST

(cond-test 'a)
1

(cond-test 'b)
2

(cond-test 'c)
3

(cond-test 'd)
0

(cond-test 'e)
0


;; test2-6
(define case-test
  (lambda (x)
    (case x
      ((a b c) 1)
      ((d e f) 2)
      ((g h i) 3)
      (else    0))))
CASE-TEST

(case-test 'a)
1

(case-test 'e)
2

(case-test 'i)
3

(case-test 'j)
0


;; test2-7
(define reverse-do
  (lambda (xs)
    (do ((ls xs (cdr ls)) (result '()))
        ((null? ls) result)
      (set! result (cons (car ls) result)))))
REVERSE-DO

(reverse-do '(a b c d e))
(E D C B A)



;;; test3-1
(apply cons '(1 2))
(1 . 2)

(apply cons 1 '(2))
(1 . 2)

(define a '(1 2 3 4 5))
A

(define foo (lambda (a b . c) (set! a 10) (set! b 20) (set! c 30)))
FOO

(apply foo a)
30

a
(1 2 3 4 5)


;; test3-2
(define a false)
A

(list 'a 'b (call/cc (lambda (k) (set! a k) 'c)) 'd)
(A B C D)

a
(CONTINUATION (B A) NIL (LDC D ARGS 4 LDG LIST APP STOP) NIL)

(a 'e)
(A B E D)

(a 'f)
(A B F D)


;; test3-3
(define bar1 (lambda (cont) (display "call bar1") (newline)))
BAR1

(define bar2 (lambda (cont) (display "call bar2") (newline) (cont false)))
BAR2

(define bar3 (lambda (cont) (display "call bar3") (newline)))
BAR3

(define test (lambda (cont) (bar1 cont) (bar2 cont) (bar3 cont)))
TEST

test
(CLOSURE
         (LD (0 . 0) ARGS 1 LDG BAR1 APP POP LD (0 . 0) ARGS 1 LDG BAR2 APP
          POP LD (0 . 0) ARGS 1 LDG BAR3 TAPP RTN)
         NIL)

(call/cc (lambda (cont) (test cont)))
; call bar1
; call bar2
FALSE


;; test3-4
(define find-do
  (lambda (fn ls)
    (call/cc
      (lambda (k)
        (do ((xs ls (cdr xs)))
            ((null? xs) false)
          (if (fn (car xs)) (k (car xs))))))))
FIND-DO

(find-do (lambda (x) (eq? 'c x)) '(a b c d e))
C

(find-do (lambda (x) (eq? 'c x)) '(a b d e f))
FALSE


;; test3-5
(define map-check (lambda (fn chk ls)
  (call/cc
    (lambda (k)
      (map (lambda (x) (if (chk x) (k '()) (fn x))) ls)))))
MAP-CHECK

(cons 1 (map-check (lambda (x) (cons x x)) (lambda (x) (eq? x 'e)) '(a b c d e f)))
(1)

(map-check (lambda (x) (cons x x)) (lambda (x) (eq? x 'e)) '(a b c d f))
((A . A) (B . B) (C . C) (D . D) (F . F))


;; test3-6
(define flatten (lambda (ls)
  (call/cc
    (lambda (cont)
      (letrec ((flatten-sub
                (lambda (ls)
                  (cond ((null? ls) '())
                        ((not (pair? ls)) (list ls))
                        ((null? (car ls)) (cont '()))
                        (else (append (flatten-sub (car ls))
                                      (flatten-sub (cdr ls))))))))
        (flatten-sub ls))))))
FLATTEN

(flatten '(a (b (c (d . e) f) g) h))
(A B C D E F G H)

(cons 1 (flatten '(a (b (c (d () . e) f) g) h)))
(1)


;; test3-7
(define a (make-iter for-each-tree '(a (b (c (d . e) f) g) h)))
A

;a
;(CLOSURE
;         (LDCT #1=(RTN) ARGS 1 LDF (LD (0 . 0) ARGS 1 LD (2 . 0) TAPP RTN) APP
;          . #1#)
;         #2=(((CLOSURE
;               (LDF
;                (LDCT #3=(LSET (1 . 0) RTN) ARGS 1 LDF
;                 (LD (0 . 0) LSET (3 . 0) POP LD (1 . 0) ARGS 1 LD (2 . 0)
;                  TAPP RTN)
;                 APP . #3#)
;                LD (2 . -2) ARGS-AP 2 LD (2 . 0) APP POP LDG FALSE ARGS 1 LD
;                (0 . 0) TAPP RTN)
;               #2#))
;             ((CLOSURE
;               (LDC :UNDEF ARGS 1 LDF
;                (LDF
;                 (LD (0 . 0) ARGS 1 LDG NULL? APP SEL
;                  (ARGS 0 LDF (LDC NIL RTN) APP JOIN)
;                  (LD (0 . 0) ARGS 1 LDG PAIR? APP SEL
;                   (ARGS 0 LDF
;                    (LD (1 . 0) ARGS 1 LDG CAR APP ARGS 1 LD (2 . 0) APP POP
;                     LD (1 . 0) ARGS 1 LDG CDR APP ARGS 1 LD (2 . 0) TAPP RTN)
;                    APP JOIN)
;                   (ARGS 0 LDF (LD (1 . 0) ARGS 1 LD (3 . 0) TAPP RTN) APP
;                    JOIN)
;                   JOIN)
;                  RTN)
;                 LSET (0 . 0) POP LD (1 . 1) ARGS 1 LD (0 . 0) TAPP RTN)
;                APP RTN)
;               NIL)
;              (A (B (C (D . E) F) G) H))))

(a)
A

(a)
B

(a)
C

(a)
D

(a)
E

(a)
F

(a)
G

(a)
H

(a)
FALSE

(a)
FALSE



;;; test4-1
(define sum
  (lambda (x)
    (if (= x 0)
        0
      (+ x (sum (- x 1))))))
SUM

(define sum1
  (lambda (x a)
    (if (= x 0)
        a
      (sum1 (- x 1) (+ a x)))))
SUM1

(sum 1000000)
500000500000

(sum1 1000000 0)
500000500000


;; test4-2
(tarai 10 5 0)
10

(tak 14 7 0)
7


;; test4-3
(define a (delay (+ 10 20)))
A

a
(CLOSURE
         (LD (1 . 0) ARGS 1 LDG NOT APP SEL
          (ARGS 0 LD (2 . 0) APP ARGS 1 LDF
           (LD (2 . 0) ARGS 1 LDG NOT APP SELR
            (ARGS 0 LDF
             (LDG TRUE LSET (3 . 0) POP LD (1 . 0) LSET (3 . 1) RTN) APP RTN)
            (LDC :UNDEF RTN))
           APP JOIN)
          (LDC :UNDEF JOIN) POP LD (1 . 1) RTN)
         ((FALSE FALSE) ((CLOSURE (LDC 10 LDC 20 ARGS 2 LDG + TAPP RTN) NIL))))

(force a)
30

(define b (delay (begin (display "oops! ") (+ 10 20))))
B

b
(CLOSURE
         (LD (1 . 0) ARGS 1 LDG NOT APP SEL
          (ARGS 0 LD (2 . 0) APP ARGS 1 LDF
           (LD (2 . 0) ARGS 1 LDG NOT APP SELR
            (ARGS 0 LDF
             (LDG TRUE LSET (3 . 0) POP LD (1 . 0) LSET (3 . 1) RTN) APP RTN)
            (LDC :UNDEF RTN))
           APP JOIN)
          (LDC :UNDEF JOIN) POP LD (1 . 1) RTN)
         ((FALSE FALSE)
          ((CLOSURE
            (ARGS 0 LDF
             (LDC "oops! " ARGS 1 LDG DISPLAY APP POP LDC 10 LDC 20 ARGS 2 LDG
              + TAPP RTN)
             APP RTN)
            NIL))))

(force b)
; oops!
30

(force b)
30

(tarai-delay 80 40 (delay 0))
80


;; test5-1
((lambda (x) (if x 10)) false)
:UNDEF



;;;;;;;;;;;;;;;;;;;
(test-end)
