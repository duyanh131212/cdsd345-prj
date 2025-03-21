#lang racket/base

(require "simpleParser.rkt"
         "utils.rkt")
(require racket/list)

;; initial state (one layer containing a list for vars and a list for values)
(define ini_state (list (list '() '())))

(define (push-layer state)
  (cons '('() '()) state))

(define (pop-layer state)
  (if (null? state)
      (error "There is no layer to pop")
      (cdr state)))

;; Helper: returns the index of 'name' in list 'lst' or #f if not found.
(define (index-of-helper name lst i)
  (cond
    [(null? lst) #f]
    [(eq? name (car lst)) i]
    [else (index-of-helper name (cdr lst) (+ i 1))]))

(define (index-of name lst)
  (index-of-helper name lst 0))

;; Helper: returns a new list with the element at index idx replaced by val.
(define (list-set lst idx val)
  (if (zero? idx)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;; Searches for a binding in a single layer.
(define (find-binding name layer)
  ((lambda (idx)
     (if (number? idx)
         (list name (list-ref (cadr layer) idx))
         #f))
   (index-of name (car layer))))

;; Updates the binding in a single layer.
(define (update-binding name val layer)
  ((lambda (vars)
     ((lambda (vals)
        ((lambda (idx)
           (if (number? idx)
               (list vars (list-set vals idx val))
               (error "Variable not declared in layer:" name)))
         (index-of name vars)))
      (cadr layer)))
   (car layer)))

;; Looks up the value for a variable by searching layers top-down.
(define (getval name state)
  (cond
    [(null? state) (error "Variable not found:" name)]
    [else ((lambda (binding)
             (if binding
                 (cadr binding)
                 (getval name (cdr state))))
           (find-binding name (car state)))]))

;; Declares a new variable in the current (top) layer.
;; It adds the variable name and initializes its value to '().
(define (declare name state)
  ((lambda (layer)
     (if (index-of name (car layer))
         (error "Variable already declared:" name)
         (cons (list (cons name (car layer))
                     (cons '() (cadr layer)))
               (cdr state))))
   (car state)))

;; Assigns a value to a variable by searching the state layers.
(define (assign name val state)
  (cond
    [(null? state) (error "Variable not declared:" name)]
    [else ((lambda (layer)
             (if (number? (index-of name (car layer)))
                 (cons (update-binding name val layer) (cdr state))
                 (cons layer (assign name val (cdr state)))))
           (car state))]))

(define (contains? atom lis)
  (cond
    [(null? lis) #f]
    [(eq? atom (car lis)) #t]
    [else (contains? atom (cdr lis))]))

(define (len lis n)
  (if (null? lis)
      n
      (len (cdr lis) (+ 1 n))))

(define (signed? expr)
  (eq? (len expr 0) 2))

(define (bool? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)
      (and (list? expr)
           (contains? (car expr) '(== != < > <= >= || && !)))))

(define (arith? expr)
  (or (number? expr)
      (and (list? expr)
           (contains? (car expr) '(+ - * / %)))))

(define (assign? expr)
  (and (list? expr)
       (eq? (car expr) '=)))

(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

(define (process-output output)
  (cond
    [(eq? output #t) 'true]
    [(eq? output #f) 'false]
    [(number? output) output]
    [else output]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVALUATION OF EXPRESSIONS (M_value, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (M_value expr state)
  (cond
    [(null? expr) (error "called M_value on a null value")]
    [(assign? expr)
     (M_value (cadr expr) (M_state_assign expr state))]
    [(arith? expr) (M_int expr state)]
    [(and (list? expr)
          (contains? (car expr) '(== != < > <= >= || && !)))
     (M_boolean expr state)]
    [(bool? expr) (M_boolean expr state)]
    [(not (list? expr)) (getval expr state)]
    [(and (list? expr) (= (len expr 0) 2))
     (eq? (M_value (car expr) state)
          (M_value (cadr expr) state))]
    [else (error "Unknown expression type in M_value")]))
    
(define (M_int expr state)
  (cond
    [(not (list? expr))
     (if (number? expr)
         expr
         (getval expr state))]
    [(and (eq? '+ (operator expr))
          (not (signed? expr)))
     (+ (M_value (leftoperand expr) state)
        (M_value (rightoperand expr) state))]
    [(and (eq? '+ (operator expr))
          (signed? expr))
     (M_value (leftoperand expr) state)]
    [(and (eq? '- (operator expr))
          (not (signed? expr)))
     (- (M_value (leftoperand expr) state)
        (M_value (rightoperand expr) state))]
    [(and (eq? '- (operator expr))
          (signed? expr))
     (- 0 (M_value (leftoperand expr) state))]
    [(eq? '* (operator expr))
     (* (M_value (leftoperand expr) state)
        (M_value (rightoperand expr) state))]
    [(eq? '/ (operator expr))
     (quotient (M_value (leftoperand expr) state)
               (M_value (rightoperand expr) state))]
    [(eq? '% (operator expr))
     (remainder (M_value (leftoperand expr) state)
                (M_value (rightoperand expr) state))]
    [else (error "Unknown operator in M_int")]))
    
(define M_boolean
  (lambda (expr state)
    (cond
      [(null? expr) (error "passed null to M_boolean")]
      [(eq? 'true expr) #t]
      [(eq? 'false expr) #f]
      [(not (list? expr)) (getval expr state)]
      [(eq? '! (operator expr))
       (not (M_value (leftoperand expr) state))]
      [(eq? '== (operator expr))
       (eq? (M_value (leftoperand expr) state)
            (M_value (rightoperand expr) state))]
      [(eq? '!= (operator expr))
       (not (eq? (M_value (leftoperand expr) state)
                 (M_value (rightoperand expr) state)))]
      [(eq? '< (operator expr))
       (< (M_value (leftoperand expr) state)
          (M_value (rightoperand expr) state))]
      [(eq? '> (operator expr))
       (> (M_value (leftoperand expr) state)
          (M_value (rightoperand expr) state))]
      [(eq? '<= (operator expr))
       (<= (M_value (leftoperand expr) state)
           (M_value (rightoperand expr) state))]
      [(eq? '>= (operator expr))
       (>= (M_value (leftoperand expr) state)
           (M_value (rightoperand expr) state))]
      [(eq? '|| (operator expr))
       (or (M_value (leftoperand expr) state)
           (M_value (rightoperand expr) state))]
      [(eq? '&& (operator expr))
       (and (M_value (leftoperand expr) state)
            (M_value (rightoperand expr) state))]
      [else (error "Unknown operator in M_boolean")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATEMENT EVALUATION (M_state_*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M_state_stmt_list: evaluates a list of statements.
(define (M_state_stmt_list stmts state)
  (if (null? stmts)
      (list 'state state)
      (let ([result (M_state_stmt (car stmts) state)])
        (cond
          [(eq? (car result) 'return) result]
          [(or (eq? (car result) 'break) (eq? (car result) 'continue))
           result]
          [else (M_state_stmt_list (cdr stmts) (cadr result))]))))

;; M_state_stmt: process a single statement.
(define (M_state_stmt stmt state)
  (cond
    [(null? stmt)
     (list 'state state)]
    [(eq? 'var (car stmt))
     (list 'state (M_state_declare stmt state))]
    [(eq? '= (car stmt))
     (list 'state (M_state_assign stmt state))]
    [(eq? 'if (car stmt))
     (M_state_if stmt state)]
    [(eq? 'while (car stmt))
     (M_state_while stmt state)]
    [(eq? 'return (car stmt))
     (list 'return (process-output (M_value (cadr stmt) state)))]
    ;; --- Added handling for break and continue ---
    [(eq? 'break (car stmt))
     (list 'break state)]
    [(eq? 'continue (car stmt))
     (list 'continue state)]
    [(and (list? stmt) (eq? (car stmt) 'begin))
     (M_state_begin (cdr stmt) state)]
    [else
     (list 'state state)]))

;; M_state_return: processes a return statement.
(define (M_state_return stmt state)
  (if (null? stmt)
      (error "cannot return null")
      (process-output (M_value (cadr stmt) state))))
    
;; M_state_declare: processes a declaration.
(define (M_state_declare stmt state)
  (if (= (len stmt 0) 2)
      (declare (cadr stmt) state)
      (M_state_assign stmt (declare (cadr stmt) state))))
    
;; M_state_assign: processes an assignment.
(define (M_state_assign stmt state)
  (if (and (list? (caddr stmt))
           (eq? (car (caddr stmt)) '=))
      ((lambda (inner-state)
         (assign (cadr stmt)
                 (M_value (cadr (caddr stmt)) inner-state)
                 inner-state))
       (M_state_assign (caddr stmt) state))
      (assign (cadr stmt)
              (M_value (caddr stmt) state)
              state)))

;; M_state_if: processes an if statement.
(define (M_state_if stmt state)
  (if (M_value (cadr stmt) state)
      (M_state_stmt (caddr stmt) state)
      (if (= (len stmt 0) 4)
          (M_state_stmt (cadddr stmt) state)
          (list 'state state))))
    
;; M_state_while: process a while loop.
(define (M_state_while stmt state)
  (if (M_value (cadr stmt) state)
      (let ([result (M_state_stmt (caddr stmt) state)])
        (cond
          [(eq? (car result) 'return) result]
          [(eq? (car result) 'break) (list 'state (cadr result))]
          [(eq? (car result) 'continue) (M_state_while stmt (cadr result))]
          [else (M_state_while stmt (cadr result))]))
      (list 'state state)))

;; Handle begin blocks (named here M_state_begin)
(define (M_state_begin stmts state)
  (define new-state (push-layer state))
  (define result (M_state_stmt_list stmts new-state))
  (cond
    [(or (eq? (car result) 'return)
         (eq? (car result) 'break)
         (eq? (car result) 'continue))
     result]
    [else
     (list 'state (pop-layer (cadr result)))]))

;; interpret: entry point.
(define (interpret filename)
  (display (parser filename))
  (M_state_stmt_list (parser filename) ini_state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
;(interpret "tests/14.txt")
(interpret "tests/15.txt")
(interpret "tests/16.txt")
(interpret "tests/17.txt")
(interpret "tests/18.txt")
(interpret "tests/19.txt")
(interpret "tests/20.txt")

;; extra tests
(interpret "tests/21.txt") ; ok
(interpret "tests/22.txt")
;(interpret "tests/23.txt")
(interpret "tests/24.txt")
(interpret "tests/25.txt") ; ok
;(interpret "tests/26.txt")
;(interpret "tests/27.txt") ; infinite loop with global state update
;(interpret "tests/28.txt")

(interpret "tests2/1.txt")
(interpret "tests2/2.txt")
(interpret "tests2/3.txt")
(interpret "tests2/4.txt")
;(interpret "tests2/5.txt")
(interpret "tests2/6.txt")
(interpret "tests2/7.txt")
(interpret "tests2/8.txt")
(interpret "tests2/9.txt")
(interpret "tests2/10.txt")
;(interpret "tests2/11.txt")
;(interpret "tests2/12.txt")
;(interpret "tests2/13.txt")
(interpret "tests2/14.txt")
;(interpret "tests2/15.txt")
;(interpret "tests2/16.txt")
;(interpret "tests2/17.txt")
;(interpret "tests2/18.txt")
;(interpret "tests2/19.txt")
;(interpret "tests2/20.txt")
