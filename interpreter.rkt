#lang racket/base

(require "simpleParser.rkt"
         "utils.rkt")
(require racket/list)

; helpers
(define (contains? atom lis)
  (cond
    [(null? lis) #f]
    [(eq? atom (car lis)) #t]
    [else (contains? atom (cdr lis))]))

; process-output
(define (process-output output)
  (cond
    [(eq? #t output) 'true]
    [(eq? #f output) 'false]
    [(number? output) output]))

; initial state
(define ini_state '(() ()))

; state vars
(define statevars car)

; state vals
(define statevals cadr)

; operator
(define operator car)

; leftoperand
(define leftoperand cadr)

; rightoperand
(define rightoperand caddr)

; getval from state
(define (getval name state)
  (let* ([vars (statevars state)]
         [vals (statevals state)])
        (cond
            [(null? vars)           (error "vars DNE")]
            [(eq? name (car vars))  (if (list? (car vals))
                                        (error "not assigned")
                                        (car vals))]
            [else                   (getval name (list (cdr vars) (cdr vals)))])))

; add
(define (add name val state)
  (cond
    [(contains? name (statevars state))     (error "alr existed")]
    [else                                   (list   (cons name (statevars state)) 
                                                    (cons val (statevals state)))]))
; remove
(define (remove name state)
  (removecps name state (lambda (v) v)))

(define (removecps name state ret)
  (cond
    [(null? (statevars state))              (ret ini_state)]
    [(eq? name (car (statevars state)))     (ret (list (cdr (statevars state)) (cdr (statevals state))))]
    [else                                   (removecps name 
                                                        (list   (cdr (statevars state))  
                                                                (cdr (statevals state)))
                                                        (lambda (s) 
                                                            (ret (list  (cons (car (statevars state)) (statevars s)) 
                                                                        (cons (car (statevals state)) (statevals s))))))]))
; declare
(define (declare name state) (add name '() state))

; assign
(define (assign name val state)
  (cond
    [(contains? name (statevars state)) (add name val (remove name state))]
    [else                               (error "have not declared yet")]))

; M_value
(define M_value 
    (lambda (expr state)
        (cond
        [(null? expr)       (error "called M_value on a null value")]
        [(assign? expr)     (M_value (cadr expr) (M_state_assign expr state))]
        [(arith? expr)      (M_int expr state)]
        [(bool? expr)       (M_boolean expr state)]
        [(not (list? expr)) (getval expr state)])))

(define (signed? expr) (eq? (len expr 0) 2))
(define (bool? expr)
  (or (contains? expr '(true false))
    (and    (list? expr) 
            (contains? (operator expr) '(== < > <= >= || &&)))))
(define (arith? expr) 
    (or (number? expr) 
        (and    (list? expr) 
                (contains? (operator expr) '(+ - * / %)))))
(define (assign? expr)
  (and (list? expr)
       (eq? (operator expr) '=)))

; M_int
(define (M_int expr state)
  (cond
    [(not (list? expr))             (if (number? expr)
                                        expr
                                        (getval expr state))]
    [(and   (eq? '+ (operator expr))
            (not (signed? expr)))   (+  (M_value (leftoperand expr) state)
                                        (M_value (rightoperand expr) state))]
    [(and   (eq? '+ (operator expr))
            (signed? expr))         (M_value (leftoperand expr) state)]
    [(and   (eq? '- (operator expr))
            (not (signed? expr)))   (-  (M_value (leftoperand expr) state)
                                        (M_value (rightoperand expr) state))]
    [(and   (eq? '- (operator expr))
            (signed? expr))         (-  0 
                                        (M_value (leftoperand expr) state))]
    [(eq? '* (operator expr))       (*  (M_value (leftoperand expr) state)
                                        (M_value (rightoperand expr) state))]
    [(eq? '/ (operator expr))       (quotient   (M_value (leftoperand expr) state)
                                                (M_value (rightoperand expr) state))]
    [(eq? '% (operator expr))       (remainder  (M_value (leftoperand expr) state)
                                                (M_value (rightoperand expr) state))]))
; M_boolean
(define M_boolean
  (lambda (expr state)
    (cond
      [(null? expr)                 (error "passed null to M_boolean")]
      [(eq? 'true expr)             #t]
      [(eq? 'false expr)            #f]
      [(not (list? expr))           (getval expr state)]
      [(eq? '! (operator expr))     (not (M_value (leftoperand expr) state))]
      [(eq? '== (operator expr))    (eq?    (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '< (operator expr))     (<      (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '> (operator expr))     (>      (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '<= (operator expr))    (<=     (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '>= (operator expr))    (>=     (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '|| (operator expr))    (or     (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      [(eq? '&& (operator expr))    (and    (M_value (leftoperand expr) state)
                                            (M_value (rightoperand expr) state))]
      )))

; M_state_stmt_list, entry point for all parse tree
(define M_state_stmt_list
  (lambda (stmt state)
    (cond
      [(null? stmt)         state]
      [(null? (cdr stmt))   (M_state_stmt (car stmt) state)]
      [else                 (M_state_stmt_list (cdr stmt) (M_state_stmt (car stmt) state))])))

; M_state_stmt
(define keyword car)
(define M_state_stmt
  (lambda (stmt state)
    (cond       
      [(null? stmt)                 state]
      [(eq? 'var (keyword stmt))    (M_state_declare stmt state)]
      [(eq? '= (keyword stmt))      (M_state_assign stmt state)]
      [(eq? 'if (keyword stmt))     (M_state_if stmt state)]
      [(eq? 'while (keyword stmt))  (M_state_while stmt state)]
      [(eq? 'return (keyword stmt)) (M_state_return stmt state)]
      [else                         state])))

; M_state_return
(define (M_state_return stmt state)
  (cond
    [(null? stmt)   (error "cannot return null")]
    [else           (process-output (M_value (cadr stmt) state))]))


(define name cadr)
(define expr caddr)
; M_state_declare
(define (M_state_declare stmt state)
  (cond
    [(eq? (len stmt 0) 2)   (declare (name stmt) state) ]
    [else                   (M_state_assign stmt (declare (name stmt) state))]))

; M_state_assign
(define (M_state_assign stmt state)
  (cond
    [(and   (list? (expr stmt)) 
            (eq? (car (expr stmt)) '=)) (assign (name stmt) 
                                                (M_value    (name (expr stmt)) 
                                                            (M_state_assign (expr stmt) state)) 
                                                (M_state_assign (expr stmt) state))]
    [else                               (assign (name stmt) 
                                                (M_value (expr stmt) state) 
                                                state)]))

(define (len lis n)
  (if (null? lis)
      n
      (len (cdr lis) (+ 1 n))))

; M_state_if
(define condition cadr)
(define stmt1 caddr)
(define stmt2 cadddr)
(define (M_state_if stmt state)
  (cond
    [(M_value (condition stmt) state)   (M_state_stmt (stmt1 stmt)
                                                      (M_state_stmt (condition stmt) state))]
    [(eq? (len stmt 0) 4)               (M_state_stmt (stmt2 stmt)
                                                      (M_state_stmt (condition stmt) state))]
    [else                               state]))

; M_state_while
(define loop_condition cadr)
(define loop_body caddr)
(define (M_state_while stmt state)
  (cond
    [(M_value (loop_condition stmt) state)    (M_state_while  stmt 
                                                                (M_state_stmt   (loop_body stmt) 
                                                                                (M_state_stmt (loop_condition stmt) state)))]
    [else                                       state]))

; interpret
(define (interpret filename) (begin
                               (display (parser filename))
                               (M_state_stmt_list (parser filename) ini_state)))

;; tests
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