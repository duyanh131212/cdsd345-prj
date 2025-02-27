#lang racket/base

;; CSDS 345 - Programming Language Concepts
;; Group 14: Anh Phan, Hieu Dang, My Le

(require "simpleParser.rkt" "utils.rkt")
(require racket/list)

;; helpers
(define (contains? atom lis)
  (cond
    [(null? lis) #f]
    [(eq? atom (car lis)) #t]
    [else (contains? atom (cdr lis))]))

(define (process-output output)
  (cond
    [(eq? #t output) 'true]
    [(eq? #f output) 'false]
    [(number? output) output]
    [else output]))

;; initial state: a pair of lists
(define ini_state (list '() '()))

;; state accessors
(define statevars car)
(define statevals cadr)
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;; getval: retrieve the value of a variable from state without using let
(define (getval name state)
  (if (null? (statevars state))
      (error "vars DNE")
      (if (eq? name (car (statevars state)))
          (if (list? (car (statevals state)))
              (error "not assigned")
              (car (statevals state)))
          (getval name (list (cdr (statevars state))
                             (cdr (statevals state)))))))

;; add a new variable binding
(define (add name val state)
  (if (contains? name (statevars state))
      (error "already existed")
      (list (cons name (statevars state))
            (cons val (statevals state)))))

;; remove a variable binding using a continuation style helper
(define (remove name state)
  (removecps name state (lambda (v) v)))

(define (removecps name state ret)
  (cond
    [(null? (statevars state))
     (ret ini_state)]
    [(eq? name (car (statevars state)))
     (ret (list (cdr (statevars state)) (cdr (statevals state))))]
    [else
     (removecps name (list (cdr (statevars state)) (cdr (statevals state)))
                 (lambda (s)
                   (ret (list (cons (car (statevars state)) (statevars s))
                              (cons (car (statevals state)) (statevals s))))))]))

;; declare and assign
(define (declare name state)
  (add name '() state))

(define (assign name val state)
  (if (contains? name (statevars state))
      (add name val (remove name state))
      (error "variable not declared yet")))

;; M_value: evaluate an expression (assignment, arithmetic, boolean, or atomic value)
(define (M_value expr state)
  (cond
    [(null? expr) (error "called M_value on a null value")]
    [(assign? expr) (M_expr_assign expr state)]
    [(arith? expr) (M_int expr state)]
    [(bool? expr) (M_boolean expr state)]
    [(not (list? expr))
     (cons (if (number? expr)
               expr
               (getval expr state))
           state)]))

(define (signed? expr) (eq? (len expr 0) 2))
(define (bool? expr)
  (or (contains? expr '(true false))
      (and (list? expr)
           (contains? (operator expr) '(== != < > <= >= || && !)))))
(define (arith? expr)
  (or (number? expr)
      (and (list? expr)
           (contains? (operator expr) '(+ - * / %)))))
(define (assign? expr)
  (and (list? expr)
       (eq? (operator expr) '=)))

(define (len lis n)
  (if (null? lis) n (len (cdr lis) (+ 1 n))))

;; abbreviations for accessing components of assignment statements
(define name cadr)
(define expr caddr)

;; M_expr_assign: evaluate an (possibly chained) assignment expression
(define (M_expr_assign stmt state)
  ((lambda (var-name)
     ((lambda (expr-part)
        (if (and (list? expr-part) (eq? (car expr-part) '=))
            ((lambda (pair1)
               ((lambda (rhs state1)
                  (cons rhs (assign var-name rhs state1)))
                (car pair1) (cdr pair1)))
             (M_expr_assign expr-part state))
            ((lambda (pair1)
               ((lambda (rhs state1)
                  (cons rhs (assign var-name rhs state1)))
                (car pair1) (cdr pair1)))
             (M_value expr-part state))))
      (expr stmt)))
   (name stmt)))

;; M_state_assign: process an assignment statement
(define (M_state_assign stmt state)
  ((lambda (pair)
     (cons #t (cdr pair)))
   (M_expr_assign stmt state)))

;; M_int: evaluate arithmetic expressions
(define (M_int expr state)
  (cond
    [(not (list? expr))
     (cons (if (number? expr) expr (getval expr state)) state)]
    [(and (eq? '+ (operator expr)) (not (signed? expr)))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              ((lambda (rval state2)
                 (cons (+ lval rval) state2))
               (car pair2) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(and (eq? '+ (operator expr)) (signed? expr))
     (M_value (leftoperand expr) state)]
    [(and (eq? '- (operator expr)) (not (signed? expr)))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              ((lambda (rval state2)
                 (cons (- lval rval) state2))
               (car pair2) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(and (eq? '- (operator expr)) (signed? expr))
     ((lambda (pair)
        (cons (- 0 (car pair)) (cdr pair)))
      (M_value (leftoperand expr) state))]
    [(eq? '* (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              ((lambda (rval state2)
                 (cons (* lval rval) state2))
               (car pair2) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '/ (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              ((lambda (rval state2)
                 (cons (quotient lval rval) state2))
               (car pair2) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '% (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              ((lambda (rval state2)
                 (cons (remainder lval rval) state2))
               (car pair2) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]))

;; M_boolean: evaluate boolean expressions
(define (M_boolean expr state)
  (cond
    [(null? expr) (error "passed null to M_boolean")]
    [(eq? 'true expr) (cons #t state)]
    [(eq? 'false expr) (cons #f state)]
    [(not (list? expr)) (cons (getval expr state) state)]
    [(eq? '! (operator expr))
     ((lambda (pair)
        (cons (not (car pair)) (cdr pair)))
      (M_value (leftoperand expr) state))]
    [(eq? '== (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (eq? lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '!= (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (not (eq? lval (car pair2))) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '< (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (< lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '> (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (> lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '<= (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (<= lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '>= (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (>= lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '|| (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (or lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]
    [(eq? '&& (operator expr))
     ((lambda (pair1)
        ((lambda (lval state1)
           ((lambda (pair2)
              (cons (and lval (car pair2)) (cdr pair2)))
            (M_value (rightoperand expr) state1)))
         (car pair1) (cdr pair1)))
      (M_value (leftoperand expr) state))]))

;; M_state_stmt_list: process a list of statements
(define (M_state_stmt_list stmt state)
  (if (null? stmt)
      (cons #t state)
      ((lambda (pair)
         (if (not (car pair))
             (cons #f (cdr pair))
             (M_state_stmt_list (cdr stmt) (cdr pair))))
       (M_state_stmt (car stmt) state))))

;; M_state_stmt: process a single statement
(define (M_state_stmt stmt state)
  (cond
    [(null? stmt) (cons #t state)]
    [(eq? 'var (car stmt)) (M_state_declare stmt state)]
    [(eq? '= (car stmt)) (M_state_assign stmt state)]
    [(eq? 'if (car stmt)) (M_state_if stmt state)]
    [(eq? 'while (car stmt)) (M_state_while stmt state)]
    [(eq? 'return (car stmt)) (M_state_return stmt state)]
    [else (cons #t state)]))

;; M_state_return: process a return statement
(define (M_state_return stmt state)
  (if (null? stmt)
      (error "cannot return null")
      ((lambda (pair)
         (cons #f (process-output (car pair))))
       (M_value (cadr stmt) state))))

;; M_state_declare: process a variable declaration statement
(define (M_state_declare stmt state)
  (if (= (len stmt 0) 2)
      (cons #t (declare (cadr stmt) state))
      ((lambda (declared-state)
         ((lambda (pair)
            (cons #t (cdr pair)))
          (M_expr_assign (list '= (cadr stmt) (caddr stmt)) declared-state)))
       (declare (cadr stmt) state))))

;; M_state_if: process an if statement
(define (M_state_if stmt state)
  ((lambda (pair)
     (if (car pair)
         (M_state_stmt (caddr stmt) (cdr pair))
         (if (= (len stmt 0) 4)
             (M_state_stmt (cadddr stmt) (cdr pair))
             (cons #t (cdr pair)))))
   (M_value (cadr stmt) state)))

;; M_state_while_loop: a helper for while loops (defined at top-level to avoid local define)
(define (M_state_while_loop stmt state-current)
  ((lambda (pair)
     (if (car pair)
         ((lambda (pair2)
            (if (car pair2)
                (M_state_while_loop stmt (cdr pair2))
                (cons #f (cdr pair2))))
          (M_state_stmt (caddr stmt) (cdr pair)))
         (cons #t (cdr pair))))
   (M_value (cadr stmt) state-current)))

;; M_state_while: process a while statement by calling the helper
(define (M_state_while stmt state)
  (M_state_while_loop stmt state))

;; interpret: parse a file and run the program
(define (interpret filename)
  ((lambda (parse-tree)
     (display parse-tree)
     (newline)
     ((lambda (pair)
        (display (cdr pair))
        (newline)
        (cdr pair))
      (M_state_stmt_list parse-tree ini_state)))
   (parser filename)))

;; run tests
(interpret "tests/1.txt")
(interpret "tests/2.txt")
(interpret "tests/3.txt")
(interpret "tests/4.txt")
(interpret "tests/5.txt")
(interpret "tests/6.txt")
(interpret "tests/7.txt")
(interpret "tests/8.txt")
(interpret "tests/9.txt")
(interpret "tests/10.txt")
;; (interpret "tests/11.txt")
;; (interpret "tests/12.txt")
;; (interpret "tests/13.txt")
;; (interpret "tests/14.txt")
(interpret "tests/15.txt")
(interpret "tests/16.txt")
(interpret "tests/17.txt")
(interpret "tests/18.txt")
(interpret "tests/19.txt")
(interpret "tests/20.txt")
(interpret "tests/21.txt")
(interpret "tests/22.txt")
(interpret "tests/23.txt")
(interpret "tests/24.txt")
(interpret "tests/25.txt")
(interpret "tests/26.txt")
(interpret "tests/27.txt")
(interpret "tests/28.txt")
