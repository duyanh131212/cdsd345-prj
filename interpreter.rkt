#lang racket
(require "functionParser.rkt")

; The main function. Calls parser to get the parse tree and interprets it with a new environment.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc 
      (lambda (return-program)
        (eval-function-call '(funcall main) 
                          (interpret-global-list (parser file) (newenvironment) return-program)
                          return-program))))))

; Process all global declarations and function definitions
(define interpret-global-list
  (lambda (program environment return-program)
    (cond
      ((null? program) environment)
      ((eq? 'var (statement-type (car program))) 
       (interpret-global-list (cdr program) 
                             (interpret-declare (car program) environment (lambda (env) env))
                             return-program))
      ((eq? '= (statement-type (car program)))
       (interpret-global-list (cdr program)
                             (interpret-assign (car program) environment (lambda (env) env))
                             return-program))
      ((eq? 'function (statement-type (car program)))
       (interpret-global-list (cdr program)
                             (interpret-function-definition (car program) environment)
                             return-program))
      (else (myerror "Invalid statement at top level"))))) 

; interprets a list of statements. The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw 
                           (lambda (env) (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define interpret-statement
  (lambda (statement environment return break continue throw next)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment next))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment next))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw next))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw next))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw next))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw next))
      ((eq? 'function (statement-type statement)) (next (interpret-function-definition statement environment)))
      ((eq? 'funcall (statement-type statement)) 
       (call/cc
        (lambda (function-return)
          ; Function is called, but result is discarded
          ; We only care about the environment changes
          (next (eval-function-call-as-statement statement environment)))))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Create a function definition and add it to the environment
(define interpret-function-definition
  (lambda (statement environment)
    (insert (function-name statement) 
            (create-closure (function-name statement) (function-parameters statement) (function-body statement) environment)
            environment)))

; Create a function closure containing parameters, body, and defining environment
; Added function name as a parameter to allow for recursion
(define create-closure
  (lambda (name parameters body environment)
    (list 'closure name parameters body environment)))

; Extract elements from a closure
(define closure-name cadr)
(define closure-parameters caddr)
(define closure-body cadddr)
(define closure-environment (lambda (closure) (car (cddddr closure))))

; Evaluate a function call and return the value
(define eval-function-call
  (lambda (statement environment return)
    (eval-function-body (lookup (function-name statement) environment)
                      (function-args statement)
                      environment
                      return)))

; NEW: Evaluate a function call as a statement (ignoring the return value)
; Returns the updated environment with any global variable changes
(define eval-function-call-as-statement
  (lambda (statement environment)
    (call/cc
     (lambda (discard-return)
       (eval-function-body-statement 
        (lookup (function-name statement) environment)
        (function-args statement)
        environment
        discard-return)))))

; Evaluate the function body with the prepared environment
(define eval-function-body
  (lambda (closure args calling-env return)
    (interpret-statement-list 
     (closure-body closure)
     (bind-parameters (closure-name closure) (closure-parameters closure) args calling-env (push-frame (closure-environment closure)))
     return
     (lambda (env) (myerror "Break used outside of loop"))
     (lambda (env) (myerror "Continue used outside of loop"))
     (lambda (v env) (myerror "Uncaught exception thrown"))
     (lambda (env) (myerror "Function finished without return")))))

; NEW: Function to evaluate a function body when called as a statement
; The key difference is we capture any global variables changed and return the updated environment
(define eval-function-body-statement
  (lambda (closure args calling-env discard-return)
    (call/cc
     (lambda (return-with-env)
       (interpret-statement-list 
        (closure-body closure)
        (bind-parameters (closure-name closure) (closure-parameters closure) args calling-env (push-frame (closure-environment closure)))
        (lambda (v) (update-global-environment calling-env (pop-frame-to-global v) return-with-env))
        (lambda (env) (myerror "Break used outside of loop"))
        (lambda (env) (myerror "Continue used outside of loop"))
        (lambda (v env) (myerror "Uncaught exception thrown"))
        (lambda (env) (update-global-environment calling-env (pop-frame-to-global env) return-with-env)))))))

; Extract the global frame from a function's environment
; Fixed: Added check to ensure environment is a list before checking its length
(define pop-frame-to-global
  (lambda (environment)
    (cond
      ((not (list? environment)) environment) ; Handle non-list values
      ((null? environment) environment) ; Handle empty lists
      ((= (length environment) 1) environment) ; Return when we're at the bottom (global) frame
      (else (pop-frame-to-global (pop-frame environment))))))

; Update global environment by copying any changed variables
(define update-global-environment
  (lambda (global-env function-global-env return-continuation)
    (return-continuation (copy-modified-globals global-env function-global-env))))

; Copy any modified global variables from function environment back to global environment
; Fixed: Added safety checks for structure
(define copy-modified-globals
  (lambda (global-env function-global-env)
    (cond
      ((not (list? function-global-env)) global-env) ; Handle non-list values
      ((null? function-global-env) global-env) ; Handle empty environment
      ((not (list? (car function-global-env))) global-env) ; Handle malformed environment
      ((null? (car function-global-env)) global-env) ; Handle empty frame
      ((not (list? (variables (car function-global-env)))) global-env) ; Handle malformed variables list
      ((null? (variables (car function-global-env))) global-env) ; Handle empty variables list
      (else (copy-modified-globals-helper global-env function-global-env)))))

; Helper function for copying modified globals, with proper structure checks
(define copy-modified-globals-helper
  (lambda (global-env function-global-env)
    (if (or (null? (variables (car function-global-env)))
            (not (list? (car (variables (car function-global-env))))))
        global-env
        (copy-modified-globals 
         (if (exists? (caar (variables (car function-global-env))) global-env)
             (update-value-if-changed (caar (variables (car function-global-env))) 
                                      (unbox (caar (store (car function-global-env)))) 
                                      global-env)
             global-env)
         (cons (list (cdar (variables (car function-global-env))) 
                     (cdar (store (car function-global-env))))
               (cdr function-global-env))))))

; Update a value in the global environment if it has been changed
(define update-value-if-changed
  (lambda (var val environment)
    (if (exists? var environment)
        (begin
          (set-box! (lookup var environment) val)
          environment)
        environment)))

; Bind parameters to arguments in the function environment
; Modified to add the function itself to its environment for recursion
(define bind-parameters
  (lambda (func-name params args calling-env function-env)
    (bind-parameters-helper func-name params args calling-env 
                            (if (null? func-name)
                                function-env
                                (insert func-name (lookup func-name calling-env) function-env)))))

; Helper function to bind parameters without adding function name again
(define bind-parameters-helper
  (lambda (func-name params args calling-env function-env)
    (cond
      ((and (null? params) (null? args)) function-env)
      ((or (null? params) (null? args)) (myerror "Parameter-argument mismatch"))
      ((eq? (car params) '&) 
       (if (not (symbol? (car args)))
           (myerror "Cannot pass non-variable by reference:" (car args))
           (bind-parameters-helper func-name
                                 (cddr params) 
                                 (cdr args) 
                                 calling-env 
                                 (insert (cadr params) (lookup (car args) calling-env) function-env))))
      (else (bind-parameters-helper func-name
                                  (cdr params) 
                                  (cdr args) 
                                  calling-env 
                                  (insert (car params) 
                                         (box (eval-expression (car args) calling-env)) 
                                         function-env))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

; Adds a new variable binding to the environment. There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) (box (eval-expression (get-declare-value statement) environment)) environment))
        (next (insert (get-declare-var statement) (box 'novalue) environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment next)
    (if (exists? (get-assign-lhs statement) environment)
        (begin
          (set-box! (lookup (get-assign-lhs statement) environment) (eval-expression (get-assign-rhs statement) environment))
          (next environment))
        (myerror "Variable used before declared:" (get-assign-lhs statement)))))

; We need to check if there is an else condition. Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) environment) 
       (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement) 
       (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop. We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw next)
    ((lambda (loop)
       (loop loop (get-condition statement) (get-body statement) environment))
     (lambda (self condition body environment)
       (if (eval-expression condition environment)
           (interpret-statement body environment 
                              return 
                              (lambda (env) (next env)) 
                              (lambda (env) (self self condition body env)) 
                              throw 
                              (lambda (env) (self self condition body env)))
           (next environment))))))

; Interprets a block. The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                            (push-frame environment)
                            return
                            (lambda (env) (break (pop-frame env)))
                            (lambda (env) (continue (pop-frame env)))
                            (lambda (v env) (throw v (pop-frame env)))
                            (lambda (env) (next (pop-frame env))))))

; We use a continuation to throw the proper value.
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block
(define interpret-try
  (lambda (statement environment return break continue throw next)
    (interpret-try-helper (get-try statement) 
                        (get-catch statement) 
                        (get-finally statement) 
                        environment 
                        return break continue throw next)))

; Helper function for try-catch-finally 
(define interpret-try-helper
  (lambda (try-block catch-block finally-block environment return break continue throw next)
    (call/cc
     (lambda (try-return)
       (interpret-block (make-try-block try-block)
                      environment
                      (create-finally-continuation finally-block environment return break continue throw try-return next)
                      (create-finally-continuation finally-block environment break break continue throw try-return next)
                      (create-finally-continuation finally-block environment continue break continue throw try-return next)
                      (create-throw-catch-continuation catch-block finally-block environment return break continue throw try-return next)
                      (lambda (env) (interpret-block (make-finally-block finally-block) env return break continue throw next)))))))

; Create a continuation for return/break/continue that runs the finally block
(define create-finally-continuation
  (lambda (finally-block environment cont break continue throw try-return next)
    (lambda (v)
      (interpret-block (make-finally-block finally-block)
                     environment
                     (lambda (v2) (cont v))
                     break continue throw
                     (lambda (env) (try-return (cont v)))))))

; Create a continuation for throw that runs the catch and finally blocks
(define create-throw-catch-continuation
  (lambda (catch-block finally-block environment return break continue throw try-return next)
    (cond
      ((null? catch-block) 
       (lambda (ex env) 
         (interpret-block (make-finally-block finally-block) env return break continue throw 
                        (lambda (env2) (throw ex env2)))))
      ((not (eq? 'catch (statement-type catch-block))) 
       (myerror "Incorrect catch statement"))
      (else 
       (lambda (ex env)
         (interpret-block 
          (cons 'begin (get-body catch-block))
          (insert (catch-var catch-block) (box ex) (push-frame env))
          return
          (lambda (env2) (break (pop-frame env2)))
          (lambda (env2) (continue (pop-frame env2)))
          (lambda (v env2) (throw v (pop-frame env2)))
          (lambda (env2) 
            (interpret-block (make-finally-block finally-block) 
                          (pop-frame env2) 
                          return break continue throw next))))))))

; Helper methods to create try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) 
       (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (unbox (lookup expr environment)))
      ((eq? 'funcall (car expr)) (call/cc (lambda (return) (eval-function-call expr environment return))))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) 
       (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal. We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

; Helper functions for function specific items
(define function-name cadr)
(define function-parameters caddr)
(define function-body cadddr)
(define function-args cddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment. If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup. Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (if (exists? var environment)
        (lookup-in-env var environment)
        (myerror "error: undefined variable" var))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (get-value (indexof var (variables frame)) (store frame))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons val (store frame)))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))


; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    ((lambda (makestr)
       (error-break (display (string-append str (makestr makestr "" vals)))))
     (lambda (self str vals)
       (if (null? vals)
           str
           (self self
                (string-append str (string-append " " (symbol->string (car vals))))
                (cdr vals)))))))

(interpret "tests3/1.txt")
(interpret "tests3/2.txt")
(interpret "tests3/3.txt")
(interpret "tests3/4.txt")
(interpret "tests3/5.txt")
(interpret "tests3/6.txt")
(interpret "tests3/7.txt")
(interpret "tests3/8.txt")
(interpret "tests3/9.txt")
(interpret "tests3/10.txt")
(interpret "tests3/11.txt")
(interpret "tests3/12.txt")
(interpret "tests3/13.txt")
(interpret "tests3/14.txt")
(interpret "tests3/15.txt")
(interpret "tests3/16.txt")
(interpret "tests3/17.txt")
(interpret "tests3/18.txt")
(interpret "tests3/19.txt")
(interpret "tests3/20.txt")
;(interpret "tests3/21.txt")
;(interpret "tests3/22.txt")
;(interpret "tests3/23.txt")