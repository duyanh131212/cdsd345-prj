#lang racket/base

(require "simpleParser.rkt"
         "utils.rkt")
(require racket/list)


(define ini_state (list '()))

(define (push-layer state)
  (cons '() state))

(define (pop-layer state)
  (if (null? state)
      (error "There is no layer to pop")
      (cdr state)))

(define (find-binding name bindings)
  (cond
    [(null? bindings) #f]
    [(eq? name (car (car bindings))) (car bindings)]
    [else (find-binding name (cdr bindings))]))

(define (update-binding name val layer)
  (cond
    [(null? layer) (error "Variable not declared in layer:" name)]
    [(eq? name (car (car layer)))
     (cons (list name val) (cdr layer))]
    [else (cons (car layer) (update-binding name val (cdr layer)))]))

(define (getval name state)
  (cond
    [(null? state) (error "Variable not found:" name)]
    [else (if (find-binding name (car state))
              (cadr (find-binding name (car state)))
              (getval name (cdr state)))]))

(define (declare name state)
  (if (find-binding name (car state))
      (error "Variable already declared:" name)
      (cons (cons (list name '()) (car state)) (cdr state))))

(define (assign name val state)
  (cond
    [(null? state) (error "Variable not declared:" name)]
    [else (if (find-binding name (car state))
              (cons (update-binding name val (car state)) (cdr state))
              (cons (car state) (assign name val (cdr state))))]))

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

;; M_value: evaluates an expression to a value.
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
    
;; M_int: arithmetic expressions.
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
    
;; M_boolean: boolean expressions.
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

;; M_state_stmt_list: evaluates a list of statements.
(define (M_state_stmt_list stmt state)
  (if (null? stmt)
      state
      (M_state_stmt_list (cdr stmt) (M_state_stmt (car stmt) state))))
    
;; M_state_stmt: dispatches a single statement.
(define (M_state_stmt stmt state)
  (cond
    [(null? stmt) state]
    [(eq? 'var (car stmt)) (M_state_declare stmt state)]
    [(eq? '= (car stmt)) (M_state_assign stmt state)]
    [(eq? 'if (car stmt)) (M_state_if stmt state)]
    [(eq? 'while (car stmt)) (M_state_while stmt state)]
    [(eq? 'return (car stmt)) (M_state_return stmt state)]
    [(and (list? stmt) (eq? (car stmt) 'begin))
     (pop-layer (M_state_stmt_list (cdr stmt) (push-layer state)))]
    [else state]))
    
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
      (assign (cadr stmt)
              (M_value (cadr (caddr stmt))
                       (M_state_assign (caddr stmt) state))
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
          state)))
    
;; M_state_while: processes a while loop.
(define (M_state_while stmt state)
  (if (M_value (cadr stmt) state)
      (M_state_while stmt (M_state_stmt (caddr stmt) state))
      state))
    
;; interpret: entry point.
(define (interpret filename)
  (begin
    (display (parser filename))
    (M_state_stmt_list (parser filename) ini_state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;
;; (Assuming your test files are available as tests2/1.txt ... tests2/20.txt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;(interpret "tests2/14.txt")
(interpret "tests2/15.txt")
(interpret "tests2/16.txt")
(interpret "tests2/17.txt")
(interpret "tests2/18.txt")
(interpret "tests2/19.txt")
(interpret "tests2/20.txt")
