;; My Le
;; Hieu Dang
;; Anh Phan
;; CSDS 345 - Group 14

#lang racket/base

(require "simpleParser.rkt"
         "utils.rkt")
(require racket/list)

;; ini_state
;; Initialize an empty layer containing an empty variable name list and variable value list
(define ini_state (list (list '() '())))

;; push-layer
;; Add a new layer (enter a new code block)
(define (push-layer state)
  (cons (list '() '()) state))

;; pop-layer
;; Pop a layer (end of a code block)
(define (pop-layer state)
  (if (null? state)
      (error "There is no layer to pop")
      (cdr state)))

;; index-of-helper
;; Helper for index-of function
(define (index-of-helper name lst i)
  (cond
    [(null? lst) #f]
    [(eq? name (car lst)) i]
    [else (index-of-helper name (cdr lst) (+ i 1))]))

;; index-of
;; Find the index of an element in a list
(define (index-of name lst)
  (index-of-helper name lst 0))

;; list-set
;; Set the value of an item at the given index
(define (list-set lst idx val)
  (if (zero? idx)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;; find-binding
;; Retrieve the value of a variable within the given layer
(define (find-binding name layer)
  ((lambda (idx)
     (if (number? idx)
         (list name (list-ref (cadr layer) idx))
         #f))
   (index-of name (car layer))))

;; update-binding
;; Update the value of an existing variable within the given layer
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

;; getval
;; Get the value of a variable
(define (getval name state)
  (cond
    [(null? state)
     (error "Variable not declared:" name)]
    [else
     (let ((binding (find-binding name (car state))))
       (cond
         [binding
          (let ((val (cadr binding)))
            (if (eq? val '())
                (error "Variable not assigned:" name)
                val))]
         [else
          (getval name (cdr state))]))]))

;; declare
;; Declare a new variable
(define (declare name state)
  ((lambda (top-layer)
     (if (index-of name (car top-layer))
         (error "Variable already defined: " name)
         (cons (list (cons name (car top-layer))
                     (cons '() (cadr top-layer)))
               (cdr state))))
   (car state)))

;; assign
;; Assign a value to a variable
(define (assign name val state)
  (cond
    [(null? state) (error "Variable not declared:" name)]
    [else
     ((lambda (layer)
        (if (number? (index-of name (car layer)))
            (cons (update-binding name val layer) (cdr state))
            (cons layer (assign name val (cdr state)))))
      (car state))]))

;; contains
;; Check if a list contains the given element
(define (contains? atom lis)
  (cond
    [(null? lis) #f]
    [(eq? atom (car lis)) #t]
    [else (contains? atom (cdr lis))]))

;; len
;; Get the length of a list
(define (len lis n)
  (if (null? lis) n (len (cdr lis) (+ 1 n))))

;; signed?
;; Check if an arithmetic expression is a unary (signed) expression
(define (signed? expr)
  (eq? (len expr 0) 2))

;; bool?
;; Check if an expression is a boolean value or operation
(define (bool? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)
      (and (list? expr) (contains? (car expr) '(== != < > <= >= || && !)))))

;; arith?
;; Check if an expression is an arithmetic expression
(define (arith? expr)
  (or (number? expr)
      (and (list? expr) (contains? (car expr) '(+ - * / %)))))

;; assign?
;; Check if an expression is an assignment
(define (assign? expr)
  (and (list? expr) (eq? (car expr) '=)))

;; operator and operands
;; Helper to extract parts of an expression
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

;; process-output
;; Reformat the output of evaluation
(define (process-output output)
  (cond
    [(eq? output #t)    'true]
    [(eq? output #f)    'false]
    [(number? output)   output]
    [else               output]))

;; M_value
;; Evaluate an expression and return its value
(define (M_value expr state)
  (cond
    [(null? expr)
     (error "Called M_value on a null value")]
    
    [(assign? expr)
     (M_value (cadr expr) (M_state_assign expr state))]

    [(arith? expr)
     (M_int expr state)]

    [(and (list? expr) (contains? (car expr) '(== != < > <= >= || && !)))
     (M_boolean expr state)]

    [(bool? expr)
     (M_boolean expr state)]

    [(symbol? expr)
     (getval expr state)]

    [else
     (error "Unknown expression: " expr)]))

;; M_int
;; Evaluate arithmetic expressions
(define (M_int expr state)
  (cond
    [(number? expr) expr]
    [(symbol? expr) (getval expr state)]

    [(and (eq? '+ (operator expr)) (not (signed? expr)))
     (+ (M_value (leftoperand expr) state)
        (M_value (rightoperand expr) state))]

    [(and (eq? '+ (operator expr)) (signed? expr))
     (M_value (leftoperand expr) state)]

    [(and (eq? '- (operator expr)) (not (signed? expr)))
     (- (M_value (leftoperand expr) state)
        (M_value (rightoperand expr) state))]

    [(and (eq? '- (operator expr)) (signed? expr))
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

    [else
     (error "Unknown operator in M_int:" expr)]))

;; M_boolean
;; Evaluate boolean expressions
(define (M_boolean expr state)
  (cond
    [(eq? expr 'true)  #t]
    [(eq? expr 'false) #f]
    
    [(symbol? expr)    (getval expr state)]

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

    [else
     (error "Unknown operator: " expr)]))

;; M_state_stmt_list
;; Execute a list of statements sequentially
(define (M_state_stmt_list stmts state next brk cont ret thr)

  ;; Base case: no statements left to process
  (if (null? stmts)
      (next state)

      ;; Recursion: process the first statement
      (M_state_stmt
       (car stmts)           ; the current statement
       state                 ; the current state
       
       ;; Process the rest of the statements
       (lambda (st2)
         (M_state_stmt_list (cdr stmts) st2 next brk cont ret thr))
       brk
       cont
       ret
       thr)))

;; M_state_stmt
;; Execute a single statement
(define (M_state_stmt stmt state next brk cont ret thr)
  (cond
    ;; empty statement
    [(null? stmt)
     (next state)]

    ;; declare statement
    [(eq? (car stmt) 'var)
     ((lambda (st2)
        (next st2))
      (M_state_declare stmt state))]

    ;; assign statement
    [(eq? (car stmt) '=)
     ((lambda (st2)
        (next st2))
      (M_state_assign stmt state))]

    ;; break
    [(eq? (car stmt) 'break)
     (brk state)]

    ;; continue
    [(eq? (car stmt) 'continue)
     (cont state)]

    ;; return
    [(eq? (car stmt) 'return)
     (ret (process-output (M_value (cadr stmt) state))
          state)]

    ;; throw
    [(eq? (car stmt) 'throw)
     (thr (M_value (cadr stmt) state) state)]

    ;; if statement
    [(eq? (car stmt) 'if)
     (M_state_if stmt state next brk cont ret thr)]

    ;; while statement
    [(eq? (car stmt) 'while)
     (M_state_while stmt state next brk cont ret thr)]

    ;; try statement
    [(eq? (car stmt) 'try)
     (M_state_try stmt state next brk cont ret thr)]

    ;; block of statements
    [(and (list? stmt) (eq? (car stmt) 'begin))
     (M_state_begin (cdr stmt) state next brk cont ret thr)]

    ;; unrecognized form of expression -> skip
    [else
     (next state)]))

;; M_state_return
;; Evaluate a return statement and extract the return value
(define (M_state_return stmt state)
  (if (null? stmt) (error "Cannot return null")
      (process-output (M_value (cadr stmt) state))))

;; M_state_declare
;; Handle variable declaration
(define (M_state_declare stmt state)
  (if (= (length stmt) 2)
      (declare (cadr stmt) state)
      
      ;; If there's a value, do assignment after declaration
      ((lambda (st2)
         (M_state_assign (list '= (cadr stmt) (caddr stmt)) st2))
       (declare (cadr stmt) state))))

;; M_state_assign
;; Handle variable assignment
(define (M_state_assign stmt state)
  (assign (cadr stmt)
          (M_value (caddr stmt) state)
          state))

;; M_state_if
;; Handle an if statement
(define (M_state_if stmt state next brk cont ret thr)
  ((lambda (condExpr thenBlock elseBlock)
     (if (M_value condExpr state)

         ;; Condition is true -> evaluate then
         (M_state_stmt thenBlock
                       state
                       next brk cont ret thr)

         ;; Condition is false -> evaluate else if it exists
         (if (null? elseBlock)
             (next state)
             (M_state_stmt (car elseBlock)
                           state
                           next brk cont ret thr))))
   
   (cadr stmt)          ; the condition
   (caddr stmt)         ; then
   (if (= (length stmt) 4)
       (list (cadddr stmt))  ; else exists
       '())))                ; no else

;; M_state_while
;; Handle a while loop
(define (M_state_while stmt state next brk cont ret thr)
  ((lambda (condExpr body)
     (if (M_value condExpr state)
         
         ;; Condition is true -> evaluate loop body
         (M_state_stmt
          body
          state
          
          ;; After body, re-evaluate the loop
          (lambda (st2)
            (M_state_while stmt st2 next brk cont ret thr))
          
          ;; break encountered
          (lambda (st2)
            (next st2))
          
          ;; continue encountered
          (lambda (st2)
            (M_state_while stmt st2 next brk cont ret thr))
          ret
          thr)
         
         ;; Condition is false -> exit loop
         (next state)))
   (cadr stmt)  ; loop condition
   (caddr stmt) ; loop body
   ))

;; M_state_begin
;; Execute a block of statements
(define (M_state_begin stmts state next brk cont ret thr)
  ((lambda (newState)
     (M_state_stmt_list
      stmts
      newState
      
      ;; normal completion
      (lambda (finalSt)
        (next (pop-layer finalSt)))
      
      ;; break
      (lambda (finalSt)
        (brk (pop-layer finalSt)))
      
      ;; continue
      (lambda (finalSt)
        (cont (pop-layer finalSt)))
      
      ;; return
      (lambda (val finalSt)
        (ret val (pop-layer finalSt)))
      
      ;; throw
      (lambda (val finalSt)
        (thr val (pop-layer finalSt)))))
   
   (push-layer state)))

;; M_state_try
;; Handle a try block
(define (M_state_try stmt state next brk cont ret outer-thr)
  ((lambda (tryBlock catchBlock finallyBlock)
     (M_state_stmt_list
      tryBlock
      state
      
      ;; try finished normally
      (lambda (st2)
        (M_state_finally finallyBlock st2 next brk cont ret outer-thr))
      
      ;; break
      (lambda (st2)
        (M_state_finally
         finallyBlock
         st2
         (lambda (st3) (brk st3))
         brk cont ret outer-thr))
      
      ;; continue
      (lambda (st2)
        (M_state_finally
         finallyBlock
         st2
         (lambda (st3) (cont st3))
         brk cont ret outer-thr))
      
      ;; return
      (lambda (val st2)
        (M_state_finally
         finallyBlock
         st2
         (lambda (st3) (ret val st3))
         brk cont ret outer-thr))
      
      ;; throw
      (lambda (val st2)
        (if (and (pair? catchBlock) (eq? (car catchBlock) 'catch))
            
            ;; run the catch block
            (M_state_catch
             val
             st2
             catchBlock
             finallyBlock
             next
             brk
             cont
             ret
             outer-thr)
            
            ;; no catch block
            (M_state_finally
             finallyBlock
             st2
             (lambda (st3) (outer-thr val st3))
             brk cont ret outer-thr)))))
   
   (cadr stmt)     ; try block
   (caddr stmt)    ; catch block
   (cadddr stmt))) ; finally block

;; M_state_catch
;; Handle a catch block
(define (M_state_catch thrown-val st catchBlock finallyBlock next brk cont ret thr)
  ((lambda (varName catchBody)
     ((lambda (st2)
        (M_state_stmt_list
         catchBody
         st2

         ;; normal completion
         (lambda (st3)
           (M_state_finally finallyBlock st3 next brk cont ret thr))

         ;; break
         (lambda (st3)
           (M_state_finally
            finallyBlock
            st3
            (lambda (st4) (brk st4))
            brk cont ret thr))
         (lambda (st3)
           
           ;; continue
           (M_state_finally
            finallyBlock
            st3
            (lambda (st4) (cont st4))
            brk cont ret thr))
         (lambda (val st3)
           
           ;; return
           (M_state_finally
            finallyBlock
            st3
            (lambda (st4) (ret val st4))
            brk cont ret thr))
         (lambda (val st3)
           
           ;; re-throw
           (M_state_finally
            finallyBlock
            st3
            (lambda (st4) (thr val st4))
            brk cont ret thr))))
      
      ;; declare and assign the thrown value to the catch variable
      (assign varName
              thrown-val
              (declare varName (push-layer st)))))
   (caadr catchBlock)
   (caddr catchBlock)))

;; M_state_finally
;; Handle a finally block
(define (M_state_finally finPart st next brk cont ret thr)
  (if (and (pair? finPart) (eq? (car finPart) 'finally))

      ;; Run the list of finally statements
      (M_state_stmt_list
       (cadr finPart)
       st
       next
       brk
       cont
       ret
       thr)

      ;; No finally block
      (next st)))

;; interpret
;; Parse test files
(define (interpret filename)
  ((lambda (parsed)
     (display parsed)
     (newline)
     (M_state_stmt_list
      parsed
      ini_state

      ;; next-cont: if all statements finish with no return/throw
      (lambda (finalState)
        (display "Program ended with no 'return' or 'throw'. Final state of the program is:\n")
        (display finalState)
        (newline))

      ;; break at top layer
      (lambda (st)
        (error "Break used outside of loop."))

      ;; continue at top layer
      (lambda (st)
        (error "Continue used outside of loop."))

      ;; Return value of evaluation
      (lambda (val st)
        (display "Result: ")
        (display val)
        (newline))

      ;; throw-cont at top layer/ uncaught throw
      (lambda (val st)
        (error "Uncaught throw" val))))
   
   (parser filename)))

;(interpret "tests/1.txt")
;(interpret "tests/2.txt")
;(interpret "tests/3.txt")
;(interpret "tests/4.txt")
;(interpret "tests/5.txt")
;(interpret "tests/6.txt")
;(interpret "tests/7.txt")
;(interpret "tests/8.txt")
;(interpret "tests/9.txt")
;(interpret "tests/10.txt")
;(interpret "tests/11.txt")
;(interpret "tests/12.txt")
;(interpret "tests/13.txt")
;(interpret "tests/14.txt")
;(interpret "tests/15.txt")
;(interpret "tests/16.txt")
;(interpret "tests/17.txt")
;(interpret "tests/18.txt")
;(interpret "tests/19.txt")
;(interpret "tests/20.txt")

;(interpret "tests2/1.txt")
;(interpret "tests2/2.txt")
;(interpret "tests2/3.txt")
;(interpret "tests2/4.txt")
;(interpret "tests2/5.txt")
;(interpret "tests2/6.txt")
;(interpret "tests2/7.txt")
;(interpret "tests2/8.txt")
;(interpret "tests2/9.txt")
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