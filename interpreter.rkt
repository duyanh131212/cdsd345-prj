#lang racket/base

(require "simpleParser.rkt"
         "utils.rkt")
(require racket/list)

;helpers
(define (contains? atom lis)
  (cond
    [(null? lis) #f]
    [(eq? atom (car lis)) #t]
    [else (contains? atom (cdr lis))]))

;process-output
(define (process-output output)
  (cond
    [(eq? #t output) 'true]
    [(eq? #f output) 'false]
    [(number? output) output]
    [else output]))

; initial state
(define ini_state (list '() '()))

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
      [(null? vars) (error "vars DNE")]
      [(eq? name (car vars))
       (if (list? (car vals))
           (error "not assigned")
           (car vals))]
      [else (getval name (list (cdr vars) (cdr vals)))])))

; add
(define (add name val state)
  (cond
    [(contains? name (statevars state)) (error "already existed")]
    [else (list (cons name (statevars state))
                (cons val (statevals state)))]))

; remove
(define (remove name state)
  (removecps name state (lambda (v) v)))

(define (removecps name state ret)
  (cond
    [(null? (statevars state)) (ret ini_state)]
    [(eq? name (car (statevars state)))
     (ret (list (cdr (statevars state)) (cdr (statevals state))))]
    [else
     (removecps name
                (list (cdr (statevars state)) (cdr (statevals state)))
                (lambda (s)
                  (ret (list (cons (car (statevars state)) (statevars s))
                             (cons (car (statevals state)) (statevals s))))))]))

; declare
(define (declare name state)
  (add name '() state))

; assign
(define (assign name val state)
  (cond
    [(contains? name (statevars state))
     (add name val (remove name state))]
    [else (error "variable not declared yet")]))

; M_value
(define (M_value expr state)
  (cond
    [(null? expr) (error "called M_value on a null value")]
    [(assign? expr) (M_expr_assign expr state)]
    [(arith? expr) (M_int expr state)]
    [(bool? expr) (M_boolean expr state)]
    [(not (list? expr))
     (cons (if (number? expr)
               expr
               (getval expr state)) state)]))

(define (signed? expr) (eq? (len expr 0) 2))
(define (bool? expr)
  (or (contains? expr '(true false))
      (and (list? expr)
           (contains? (operator expr)
                     '(== != < > <= >= || && !)))))
(define (arith? expr)
  (or (number? expr)
      (and (list? expr)
           (contains? (operator expr)
                     '(+ - * / %)))))
(define (assign? expr)
  (and (list? expr)
       (eq? (operator expr) '=)))

(define (len lis n)
  (if (null? lis)
      n
      (len (cdr lis) (+ 1 n))))

(define name cadr)
(define expr caddr)

;; M_expr_assign: Evaluates an assignment expression (possibly chained)
;; Returns a pair: (cons assigned-value new-state)
(define (M_expr_assign stmt state)
  (let ([var-name (name stmt)]
        [expr-part (expr stmt)])
    (if (and (list? expr-part) (eq? (car expr-part) '=))
        (let ([pair1 (M_expr_assign expr-part state)])
          (let ([rhs (car pair1)]
                [state1 (cdr pair1)])
            (let ([updated-state (assign var-name rhs state1)])
              (cons rhs updated-state))))
        (let ([pair1 (M_value expr-part state)])
          (let ([rhs (car pair1)]
                [state1 (cdr pair1)])
            (let ([updated-state (assign var-name rhs state1)])
              (cons rhs updated-state)))))))

; M_state_assign
(define (M_state_assign stmt state)
  (let ([pair (M_expr_assign stmt state)])
    (cons #t (cdr pair))))

; M_int
(define (M_int expr state)
  (cond
    [(not (list? expr))
     (cons (if (number? expr) expr (getval expr state))
           state)]
    [(and (eq? '+ (operator expr)) (not (signed? expr)))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (let ([rval (car pair2)]
                 [state2 (cdr pair2)])
             (cons (+ lval rval) state2)))))]
    [(and (eq? '+ (operator expr)) (signed? expr))
     (M_value (leftoperand expr) state)]
    [(and (eq? '- (operator expr)) (not (signed? expr)))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (let ([rval (car pair2)]
                 [state2 (cdr pair2)])
             (cons (- lval rval) state2)))))]
    [(and (eq? '- (operator expr)) (signed? expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (cons (- 0 (car pair1)) (cdr pair1)))]
    [(eq? '* (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (let ([rval (car pair2)]
                 [state2 (cdr pair2)])
             (cons (* lval rval) state2)))))]
    [(eq? '/ (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (let ([rval (car pair2)]
                 [state2 (cdr pair2)])
             (cons (quotient lval rval) state2)))))]
    [(eq? '% (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (let ([rval (car pair2)]
                 [state2 (cdr pair2)])
             (cons (remainder lval rval) state2)))))]))

; M_boolean
(define (M_boolean expr state)
  (cond
    [(null? expr) (error "passed null to M_boolean")]
    [(eq? 'true expr) (cons #t state)]
    [(eq? 'false expr) (cons #f state)]
    [(not (list? expr)) (cons (getval expr state) state)]
    [(eq? '! (operator expr))
     (let ([pair (M_value (leftoperand expr) state)])
       (cons (not (car pair)) (cdr pair)))]
    [(eq? '== (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (eq? lval (car pair2)) (cdr pair2)))))]

    [(eq? '!= (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (not (eq? lval (car pair2))) (cdr pair2)))))]

    [(eq? '< (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (< lval (car pair2)) (cdr pair2)))))]

    [(eq? '> (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (> lval (car pair2)) (cdr pair2)))))]

    [(eq? '<= (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (<= lval (car pair2)) (cdr pair2)))))]

    [(eq? '>= (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (>= lval (car pair2)) (cdr pair2)))))]

    [(eq? '|| (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (or lval (car pair2)) (cdr pair2)))))]

    [(eq? '&& (operator expr))
     (let ([pair1 (M_value (leftoperand expr) state)])
       (let ([lval (car pair1)]
             [state1 (cdr pair1)])
         (let ([pair2 (M_value (rightoperand expr) state1)])
           (cons (and lval (car pair2)) (cdr pair2)))))]))


;; M_state_stmt_list
(define (M_state_stmt_list stmt state)
  (if (null? stmt)
      (cons #t state)
      (let ([pair (M_state_stmt (car stmt) state)])
        (if (not (car pair))
            (cons #f (cdr pair))
            (M_state_stmt_list (cdr stmt) (cdr pair))))))

; M_state_stmt
(define keyword car)
(define (M_state_stmt stmt state)
  (cond
    [(null? stmt) (cons #t state)]
    [(eq? 'var (keyword stmt)) (M_state_declare stmt state)]
    [(eq? '= (keyword stmt)) (M_state_assign stmt state)]
    [(eq? 'if (keyword stmt)) (M_state_if stmt state)]
    [(eq? 'while (keyword stmt)) (M_state_while stmt state)]
    [(eq? 'return (keyword stmt)) (M_state_return stmt state)]
    [else (cons #t state)]))

; M_state_return
(define (M_state_return stmt state)
  (if (null? stmt)
      (error "cannot return null")
      (let ([pair (M_value (cadr stmt) state)])
        (cons #f (process-output (car pair))))))

; M_state_declare
(define (M_state_declare stmt state)
  (if (= (len stmt 0) 2)
      (cons #t (declare (name stmt) state))
      (let ([declared-state (declare (name stmt) state)])
        (let ([pair (M_expr_assign (list '= (name stmt) (expr stmt)) declared-state)])
          (cons #t (cdr pair))))))

; M_state_if
(define (M_state_if stmt state)
  (let ([pair (M_value (cadr stmt) state)])
    (if (car pair)
        (M_state_stmt (caddr stmt) (cdr pair))
        (if (= (len stmt 0) 4)
            (M_state_stmt (cadddr stmt) (cdr pair))
            (cons #t (cdr pair))))))

; M_state_while
(define (M_state_while stmt state)
  (let loop ([state-current state])
    (let ([pair (M_value (cadr stmt) state-current)])
      (if (car pair)
          (let ([pair2 (M_state_stmt (caddr stmt) (cdr pair))])
            (if (car pair2)
                (loop (cdr pair2))
                (cons #f (cdr pair2))))
          (cons #t (cdr pair))))))

; interpret
(define (interpret filename)
  (let ([parse-tree (parser filename)])
    (display parse-tree)
    (newline)
    (let ([pair (M_state_stmt_list parse-tree ini_state)])
      (display (cdr pair))
      (newline)
      (cdr pair))))

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
