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
(define (index-of name lst)
  (let loop ((lst lst) (i 0))
    (cond
      [(null? lst) #f]
      [(eq? name (car lst)) i]
      [else (loop (cdr lst) (+ i 1))])))

;; Helper: returns a new list with the element at index idx replaced by val.
(define (list-set lst idx val)
  (if (zero? idx)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;; Searches for a binding in a single layer.
(define (find-binding name layer)
  (let ((idx (index-of name (car layer))))
    (if (number? idx)
        (list name (list-ref (cadr layer) idx))
        #f)))

;; Updates the binding in a single layer.
(define (update-binding name val layer)
  (let* ((vars (car layer))
         (vals (cadr layer))
         (idx (index-of name vars)))
    (if (number? idx)
        (list vars (list-set vals idx val))
        (error "Variable not declared in layer:" name))))

;; Looks up the value for a variable by searching layers top-down.
(define (getval name state)
  (cond
    [(null? state) (error "Variable not found:" name)]
    [else (let ((binding (find-binding name (car state))))
            (if binding
                (cadr binding)
                (getval name (cdr state))))]))

;; Declares a new variable in the current (top) layer.
;; It adds the variable name and initializes its value to '().
(define (declare name state)
  (let ((layer (car state)))
    (if (index-of name (car layer))
        (error "Variable already declared:" name)
        (cons (list (cons name (car layer))         ; add name to the vars list
                    (cons '() (cadr layer)))        ; add initial value '() to the values list
              (cdr state)))))

;; Assigns a value to a variable by searching the state layers.
(define (assign name val state)
  (cond
    [(null? state) (error "Variable not declared:" name)]
    [else (let ((layer (car state)))
            (if (number? (index-of name (car layer)))
                (cons (update-binding name val layer) (cdr state))
                (cons layer (assign name val (cdr state)))))]))


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
(define (M_state_stmt_list stmts state)
  (if (null? stmts)
      (list 'state state)
      (let ([result (M_state_stmt (car stmts) state)])
        (cond
          [(eq? (car result) 'return)
           result]
          [else
           (M_state_stmt_list (cdr stmts) (cadr result))]))))

    
;; M_state_stmt: process a single statement.
(define (M_state_stmt stmt state)
  (cond
    [(null? stmt)
    ;; Empty statement
    (list 'state state)]
    ;; Declaration statement
    [(eq? 'var (car stmt))
     (list 'state (M_state_declare stmt state))]
    ;; Assign statement
    [(eq? '= (car stmt))
     (list 'state (M_state_assign stmt state))]
    ;; If statement
    [(eq? 'if (car stmt))
     (M_state_if stmt state)]
    ;; While statement
    [(eq? 'while (car stmt))
     (M_state_while stmt state)]
    ;; Return encountered
    [(eq? 'return (car stmt))
     (list 'return (process-output (M_value (cadr stmt) state)))]
    ;; Begin-block -> push a layer, evaluate statements, then pop the layer.
    [(and (list? stmt) (eq? (car stmt) 'begin))
     (M_state_begin (cdr stmt) state)]
    
    [else
     ;; Any other statement
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
      ;; Condition is true -> evaluate then
      (M_state_stmt (caddr stmt) state)
      ;; Condition is false -> no else or process else branch
      (if (= (len stmt 0) 4)
          (M_state_stmt (cadddr stmt) state)
          (list 'state state))))
    
;; M_state_while: process a while loop.
(define (M_state_while stmt state)
  (if (M_value (cadr stmt) state)
      ;; Run the body once if the condition is true
      (let ([res (M_state_stmt (caddr stmt) state)])
        (cond
          [(eq? (car res) 'return)
           ;; Return encountered
           res]
          [else
           ;; Keep looping if return is not encountered
           (M_state_while stmt (cadr res))]))
      ;; False condition
      (list 'state state)))

;; Handle begin blocks
(define (M_state_begin stmts state)
  (define new-state (push-layer state))
  (define result (M_state_stmt_list stmts new-state))
  (cond
    [(eq? (car result) 'return)
     ;; Immediately stop if return is encountered
     result]
    [else
     ;; If the block did not return, pop the layer
     (list 'state (pop-layer (cadr result)))]))

;; interpret: entry point.
(define (interpret filename)
  (begin
    (display (parser filename))
    (M_state_stmt_list (parser filename) ini_state)))

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
(interpret "tests/21.txt");ok
(interpret "tests/22.txt")
;(interpret "tests/23.txt")
(interpret "tests/24.txt")
(interpret "tests/25.txt");ok
;(interpret "tests/26.txt")
;(interpret "tests/27.txt");infinite loop vi k update global state
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
;(interpret "tests2/10.txt")
;(interpret "tests2/11.txt")
;(interpret "tests2/12.txt")
;(interpret "tests2/13.txt")
;(interpret "tests2/14.txt")
;(interpret "tests2/15.txt")
;(interpret "tests2/16.txt")
;(interpret "tests2/17.txt")
;(interpret "tests2/18.txt")
;(interpret "tests2/19.txt")
;(interpret "tests2/20.txt")
