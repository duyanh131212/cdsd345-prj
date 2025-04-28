#lang racket

(require "classParser.rkt")

;------------------------------------
; CSDS 345 - Project 4
; Group 13
; Hieu Dang, My Le, Anh Phan
;------------------------------------

; The main function. Calls parser to get the parse tree and interprets it
(define interpret
  (lambda (filename class-name)
    (scheme->language
     (call/cc
      (lambda (return-program)
        (interpret-main-class
         (interpret-classes
          (parser filename)
          (newenvironment)
          return-program)
         (string->symbol class-name)
         return-program))))))

; Interpret the main class and its main method
(define interpret-main-class
  (lambda (class-env class-name return-program)
    (interpret-main-method
     (lookup class-name class-env)
     class-env
     return-program)))

; Helper function to interpret the main method
(define interpret-main-method
  (lambda (main-class class-env return-program)
    (eval-function-body
     (lookup-method 'main (class-methods main-class))
     '()
     class-env
     main-class
     #f
     return-program
     (lambda (ex env) (myerror "Uncaught exception thrown:" ex)))))

; Function to lookup a method in a method list
(define lookup-method
  (lambda (method-name methods)
    (cond
      ((null? methods) (myerror "Method not found:" method-name))
      ((eq? method-name (caar methods)) (cadar methods))
      (else (lookup-method method-name (cdr methods))))))

(define lookup-in-method-list
  (lambda (method-name methods)
    (cond
      ((null? methods) (myerror "Method not found:" method-name))
      ((eq? method-name (caar methods)) (cadar methods))
      (else (lookup-in-method-list method-name (cdr methods))))))

; Interpret all class definitions
(define interpret-classes
  (lambda (program environment return-program)
    (cond
      ((null? program) environment)
      ((eq? 'class (statement-type (car program)))
       (interpret-classes (cdr program)
                          (interpret-class-definition (car program) environment return-program)
                          return-program))
      (else (myerror "Invalid statement at top level")))))

; Interpret a class definition
(define interpret-class-definition
  (lambda (statement environment return-program)
    (insert (class-name statement)
            (create-class-closure statement
                                   (if (null? (class-extends statement))
                                       '()
                                       (lookup (extends-class-name statement) environment))
                                   environment)
            environment)))

; Create a class closure containing all class information
(define create-class-closure
  (lambda (statement parent-class global-env)
    (list 'class (class-name statement) 
          parent-class
          (interpret-class-fields (class-body statement) 
                                 (if (null? parent-class) '() (class-fields parent-class)))
          (interpret-class-methods (class-name statement) 
                                 (class-body statement) 
                                 (if (null? parent-class) '() (class-methods parent-class))
                                 global-env))))

; Extract elements from a class closure
(define class-name 
  (lambda (class)
    (if (eq? 'class (statement-type class))
        (cadr class)
        (cadr class))))

(define class-extends
  (lambda (class)
    (caddr class)))

(define extends-class-name
  (lambda (class)
    (cadr (cadr (class-extends class)))))

(define class-body
  (lambda (class)
    (cadddr class)))

(define class-parent
  (lambda (class-closure)
    (caddr class-closure)))

(define class-fields
  (lambda (class-closure)
    (cadddr class-closure)))

(define class-methods
  (lambda (class-closure)
    (car (cddddr class-closure))))

; Interpret class fields from class body
(define interpret-class-fields
  (lambda (class-body parent-fields)
    (cond
      ((null? class-body) parent-fields)
      ((and (eq? 'var (statement-type (car class-body))) 
            (not (eq? 'static-var (statement-type (car class-body))))) 
       (interpret-class-fields 
        (cdr class-body) 
        (cons (list (get-declare-var (car class-body))
                    (if (exists-declare-value? (car class-body))
                        (get-declare-value (car class-body))
                        'novalue))
              parent-fields)))
      (else (interpret-class-fields (cdr class-body) parent-fields)))))

; Interpret class methods from class body
(define interpret-class-methods
  (lambda (class-name class-body parent-methods global-env)
    (cond
      ((null? class-body) parent-methods)
      ((eq? 'static-function (statement-type (car class-body)))
       (interpret-class-methods 
        class-name
        (cdr class-body) 
        (replace-or-add (function-name (car class-body))
                       (create-method-closure (function-name (car class-body)) 
                                             (function-parameters (car class-body))
                                             (function-body (car class-body)) 
                                             global-env
                                             class-name
                                             #t)
                       parent-methods)
        global-env))
      ((eq? 'function (statement-type (car class-body)))
       (interpret-class-methods 
        class-name
        (cdr class-body) 
        (replace-or-add (function-name (car class-body))
                       (create-method-closure (function-name (car class-body)) 
                                             (cons 'this (function-parameters (car class-body)))
                                             (function-body (car class-body)) 
                                             global-env
                                             class-name
                                             #f)
                       parent-methods)
        global-env))
      (else (interpret-class-methods class-name (cdr class-body) parent-methods global-env)))))

; Helper function to replace or add an element to a list
(define replace-or-add
  (lambda (name value lst)
    (cond
      ((null? lst) (list (list name value)))
      ((eq? name (caar lst)) (cons (list name value) (cdr lst)))
      (else (cons (car lst) (replace-or-add name value (cdr lst)))))))

; Create a method closure with additional class information
(define create-method-closure
  (lambda (name parameters body environment class-name is-static)
    (list 'method name parameters body environment class-name is-static)))

; Extract elements from a method closure
(define method-name cadr)
(define method-parameters caddr)
(define method-body cadddr)
(define method-environment (lambda (method) (car (cddddr method))))
(define method-class (lambda (method) (cadr (cddddr method))))
(define method-static? (lambda (method) (caddr (cddddr method))))

; Create a new object instance
(define create-object-instance
  (lambda (class-closure env)
    (list 'object class-closure (initialize-fields (class-fields class-closure) env class-closure))))

; Initialize fields for a new object
(define initialize-fields
  (lambda (fields env class)
    (cond
      ((null? fields) '())
      (else 
       (cons (list (caar fields) 
                    (box (if (eq? (cadar fields) 'novalue)
                             (default-value (caar fields))
                             (eval-expression (cadar fields) env class #f))))
               (initialize-fields (cdr fields) env class))))))

; Default values for uninitialized fields
(define default-value
  (lambda (field-name)
    0))  ; Default to 0 for simplicity

; Extract elements from an object
(define object-class cadr)
(define object-fields caddr)

; Lookup a field in an object
(define object-lookup-field
  (lambda (field-name obj)
    (cond
      ((null? (object-fields obj)) (myerror "Field not found in object:" field-name))
      ((eq? field-name (caar (object-fields obj))) (cadar (object-fields obj)))
      (else (object-lookup-field field-name (list 'object (object-class obj) (cdr (object-fields obj))))))))

; Update a field in an object
(define object-update-field
  (lambda (field-name value obj)
    (cond
      ((null? (object-fields obj)) (myerror "Field not found in object:" field-name))
      ((eq? field-name (caar (object-fields obj)))
       (begin
         (set-box! (cadar (object-fields obj)) value)
         obj))
      (else (object-update-field field-name value 
                               (list 'object (object-class obj) (cdr (object-fields obj))))))))

; Evaluate a function call and return the value
(define eval-function-call
  (lambda (statement environment class-context this-obj return throw)
    (cond
      ((eq? 'new (car (function-name statement)))
       (display "Creating new instance of class: ") (display (cadr (function-name statement))) (newline)
       (display "Environment: ") (display environment) (newline)
       (create-object-instance (lookup-class (cadr (function-name statement)) environment) environment))
      ((and (list? (function-name statement)) (eq? 'dot (car (function-name statement))))
       (eval-function-call-dot (cadr (function-name statement)) 
                              (caddr (function-name statement)) 
                              (function-args statement) 
                              environment class-context this-obj return throw))
      (else 
       (eval-function-body
        (if class-context
            (lookup (function-name statement) (class-methods class-context))
            (lookup (function-name statement) environment))
        (map (lambda (arg) (eval-expression arg environment class-context this-obj)) 
             (function-args statement))
        environment 
        class-context 
        this-obj 
        return 
        throw)))))

; Helper function for dot function calls
(define eval-function-call-dot
  (lambda (obj-expr method-name args environment class-context this-obj return throw)
    (eval-function-body
     (lookup-method method-name
                    (class-methods
                     (object-class
                      (eval-expression obj-expr environment class-context this-obj))))
     (map (lambda (arg)
            (eval-expression arg environment class-context this-obj))
          args)
     environment
     (object-class
      (eval-expression obj-expr environment class-context this-obj))
     (eval-expression obj-expr environment class-context this-obj)
     return
     throw)))

; Evaluate the function body with the prepared environment
(define eval-function-body
  (lambda (method args environment class-context this-obj return throw)
    (interpret-statement-list 
     (method-body method)
     (bind-parameters method args environment class-context this-obj)
     return
     (lambda (env) (myerror "Break used outside of loop"))
     (lambda (env) (myerror "Continue used outside of loop"))
     throw
     (lambda (env) (myerror "Function finished without return")))))

; Bind parameters to arguments in the function environment
(define bind-parameters
  (lambda (method args env class-context this-obj)
    (if (method-static? method)
        ; For static methods, we want to make sure the original environment 
        ; is still accessible for class lookups
        (bind-params-helper (method-parameters method) args env 
                           (push-frame-preserve-globals env (method-environment method)))
        ; For instance methods, similar approach but add 'this'
        (bind-params-helper (cdr (method-parameters method)) args env
                           (insert 'this (box this-obj) 
                                  (push-frame-preserve-globals env (method-environment method)))))))

; Helper to push a frame while preserving access to global environment
(define push-frame-preserve-globals
  (lambda (global-env method-env)
    (cons (newframe) global-env)))

; Helper function to bind parameters
(define bind-params-helper
  (lambda (params args env function-env)
    (cond
      ((and (null? params) (null? args)) function-env)
      ((or (null? params) (null? args)) (myerror "Parameter-argument mismatch"))
      (else (bind-params-helper
             (cdr params) 
             (cdr args) 
             env 
             (insert (car params) 
                    (box (car args)) 
                    function-env))))))

; interprets a list of statements. The state/environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw next)
    (if (null? statement-list)
        (next environment)
        (interpret-statement (car statement-list) environment return break continue throw 
                           (lambda (env) (interpret-statement-list (cdr statement-list) env return break continue throw next))))))

; interpret a statement in the environment with continuations
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
      ((eq? 'funcall (statement-type statement)) 
       (call/cc
        (lambda (function-return)
          (eval-function-call 
           statement 
           environment 
           (extract-class-context environment) 
           (extract-this-object environment) 
           function-return
           throw)
          (next environment))))
      (else (myerror "Unknown statement:" (statement-type statement))))))

; Extract the class context from environment (if available)
(define extract-class-context
  (lambda (environment)
    (extract-class-from-this (lookup-safe 'this environment))))

; Helper function to extract class from this object
(define extract-class-from-this
  (lambda (this-binding)
    (if this-binding
        (object-class (unbox this-binding))
        #f)))

; Extract the this object from environment (if available)
(define extract-this-object
  (lambda (environment)
    (extract-this-from-binding (lookup-safe 'this environment))))

; Helper function to extract this from binding
(define extract-this-from-binding
  (lambda (this-binding)
    (if this-binding
        (unbox this-binding)
        #f)))

; Safe lookup that returns #f instead of error if not found
(define lookup-safe
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) 
       (lookup-in-frame var (topframe environment)))
      (else (lookup-safe var (remainingframes environment))))))

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) 
                            environment 
                            (extract-class-context environment)
                            (extract-this-object environment)))))

; Adds a new variable binding to the environment
(define interpret-declare
  (lambda (statement environment next)
    (if (exists-declare-value? statement)
        (next (insert (get-declare-var statement) 
                     (box (eval-expression (get-declare-value statement) 
                                          environment 
                                          (extract-class-context environment)
                                          (extract-this-object environment))) 
                     environment))
        (next (insert (get-declare-var statement) (box 'novalue) environment)))))

; Updates the environment to add a new binding for a variable
(define interpret-assign
  (lambda (statement environment next)
    (interpret-assign-helper (get-assign-lhs statement) 
                            (get-assign-rhs statement) 
                            environment 
                            next)))

; Helper for interpret-assign to handle different lhs types
(define interpret-assign-helper
  (lambda (lhs rhs environment next)
    (cond
      ; Case 1: Simple variable assignment
      ((symbol? lhs)
       (if (exists? lhs environment)
           (next (begin
                  (set-box! (lookup lhs environment) 
                           (eval-expression rhs 
                                          environment 
                                          (extract-class-context environment)
                                          (extract-this-object environment)))
                  environment))
           (assign-to-field lhs rhs environment next)))
      
      ; Case 2: Object field assignment (dot operator)
      ((and (list? lhs) (eq? 'dot (car lhs)))
       (next (object-update-field 
              (caddr lhs)
              (eval-expression rhs environment (extract-class-context environment) (extract-this-object environment))
              (eval-expression (cadr lhs) environment (extract-class-context environment) (extract-this-object environment)))))
      
      (else (myerror "Invalid assignment target")))))

; Helper to assign to an object field
(define assign-to-field
  (lambda (field-name rhs environment next)
    (if (extract-this-object environment)
        (next (object-update-field 
               field-name
               (eval-expression rhs environment (extract-class-context environment) (extract-this-object environment))
               (extract-this-object environment)))
        (myerror "Variable not found:" field-name))))

; Evaluates the expression and executes the appropriate branch
(define interpret-if
  (lambda (statement environment return break continue throw next)
    (cond
      ((eval-expression (get-condition statement) 
                       environment 
                       (extract-class-context environment)
                       (extract-this-object environment)) 
       (interpret-statement (get-then statement) environment return break continue throw next))
      ((exists-else? statement) 
       (interpret-statement (get-else statement) environment return break continue throw next))
      (else (next environment)))))

; Interprets a while loop
(define interpret-while
  (lambda (statement environment return throw next)
    ((lambda (loop)
       (loop loop (get-condition statement) (get-body statement) environment))
     (lambda (self condition body environment)
       (if (eval-expression condition 
                          environment 
                          (extract-class-context environment)
                          (extract-this-object environment))
           (interpret-statement body environment 
                              return 
                              (lambda (env) (next env)) 
                              (lambda (env) (self self condition body env)) 
                              throw 
                              (lambda (env) (self self condition body env)))
           (next environment))))))

; Interprets a block
(define interpret-block
  (lambda (statement environment return break continue throw next)
    (interpret-statement-list (cdr statement)
                            (push-frame environment)
                            return
                            (lambda (env) (break (pop-frame env)))
                            (lambda (env) (continue (pop-frame env)))
                            (lambda (v env) (throw v (pop-frame env)))
                            (lambda (env) (next (pop-frame env))))))

; Throws an exception
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) 
                          environment 
                          (extract-class-context environment)
                          (extract-this-object environment)) 
           environment)))

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

; Evaluates expressions, including object-oriented constructs
(define eval-expression
  (lambda (expr environment class-context this-obj)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((eq? expr 'this) 
       (if this-obj 
           this-obj 
           (myerror "Using 'this' outside of instance method context")))
      ((not (list? expr)) 
       (eval-variable expr environment this-obj))
      ((eq? 'new (car expr)) 
       (create-object-instance (lookup-class (cadr expr) environment) environment))
      ((eq? 'dot (car expr))
       (eval-dot-expression (cadr expr) (caddr expr) environment class-context this-obj))
      ((eq? 'funcall (car expr)) 
       (call/cc
        (lambda (return-cont)
          (eval-function-call expr
                            environment
                            class-context
                            this-obj
                            return-cont
                            (lambda (ex env)
                              (myerror "Uncaught exception thrown in expression" ex))))))
      (else (eval-operator expr environment class-context this-obj)))))

; Evaluate a variable, possibly in an object
(define eval-variable
  (lambda (var environment this-obj)
    (cond
      ((exists? var environment)
       (unbox (lookup var environment)))
      ((and this-obj (field-exists? var (object-fields this-obj)))
       (unbox (object-lookup-field var this-obj)))
      (else (myerror "Variable not found:" var)))))

; Check if a field exists in an object
(define field-exists?
  (lambda (field-name object-fields)
    (cond
      ((null? object-fields) #f)
      ((eq? field-name (caar object-fields)) #t)
      (else (field-exists? field-name (cdr object-fields))))))

; Evaluate a dot expression
(define eval-dot-expression
  (lambda (obj-expr field-or-method environment class-context this-obj)
    (let ((obj (eval-expression obj-expr environment class-context this-obj)))
      (cond
        ((eq? obj-expr 'super)
         (if this-obj
             (eval-super-expression field-or-method environment class-context this-obj)
             (myerror "Using 'super' outside of instance method context")))
        (else
         (if (method-exists? field-or-method (class-methods (object-class obj)))
             (lookup-method field-or-method (class-methods (object-class obj)))
             (unbox (object-lookup-field field-or-method obj))))))))

; Evaluate a super expression
(define eval-super-expression
  (lambda (field-or-method environment class-context this-obj)
    (if (null? (class-parent class-context))
        (myerror "No parent class found")
        (lookup-field-or-method 
         (list 'object (class-parent class-context) (object-fields this-obj))
         field-or-method 
         environment 
         (class-parent class-context)))))

; Evaluate a normal dot expression
(define eval-dot-normal
  (lambda (obj-expr field-or-method environment class-context this-obj)
    (newline)
    (lookup-field-or-method 
     (eval-expression obj-expr environment class-context this-obj)
     field-or-method 
     environment 
     class-context)))

; Lookup a field or method from an object
(define lookup-field-or-method
  (lambda (obj field-or-method environment class-context)
    (cond
      ((method-exists? field-or-method (class-methods (object-class obj)))
       (lookup-method field-or-method (class-methods (object-class obj))))
      ((field-exists? field-or-method (object-fields obj))
       (unbox (object-lookup-field field-or-method obj)))
      (else (myerror "Field or method not found:" field-or-method)))))

; Lookup a class
(define lookup-class
  (lambda (class-name environment)
    (cond
      ((null? environment) (myerror "Class not found:" class-name))
      ((exists-in-list? class-name (variables (topframe environment)))
       (lookup-in-frame class-name (topframe environment)))
      (else
       (lookup-class class-name (remainingframes environment))))))

(define exists-in-topframe?
  (lambda (var environment)
    (exists-in-list? var (variables (topframe environment)))))

; Check if a method exists in a class
(define method-exists?
  (lambda (method-name methods)
    (cond
      ((null? methods) #f)
      ((eq? method-name (caar methods)) #t)
      (else (method-exists? method-name (cdr methods))))))

; Evaluate a binary (or unary) operator
(define eval-operator
  (lambda (expr environment class-context this-obj)
    (cond
      ((eq? '! (operator expr)) 
       (not (eval-expression (operand1 expr) environment class-context this-obj)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) 
       (- (eval-expression (operand1 expr) environment class-context this-obj)))
      (else (eval-binary-op expr 
                           (eval-expression (operand1 expr) environment class-context this-obj) 
                           environment class-context this-obj)))))

; Complete the evaluation of the binary operator
(define eval-binary-op
  (lambda (expr op1value environment class-context this-obj)
    (cond
      ((eq? '+ (operator expr)) 
       (+ op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '- (operator expr)) 
       (- op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '* (operator expr)) 
       (* op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '/ (operator expr)) 
       (quotient op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '% (operator expr)) 
       (remainder op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '== (operator expr)) 
       (isequal op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '!= (operator expr)) 
       (not (isequal op1value (eval-expression (operand2 expr) environment class-context this-obj))))
      ((eq? '< (operator expr)) 
       (< op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '> (operator expr)) 
       (> op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '<= (operator expr)) 
       (<= op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '>= (operator expr)) 
       (>= op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '|| (operator expr)) 
       (or op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      ((eq? '&& (operator expr)) 
       (and op1value (eval-expression (operand2 expr) environment class-context this-obj)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal
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

; Looks up a value in the environment
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup
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
      ((exists-in-list? var (variables (topframe environment))) 
       (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) 
       (myerror "error: undefined variable" var))
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

; Adds a new variable/value binding pair into the environment
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Add a new variable/value pair to the frame
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
      ((and (list? v) (eq? (car v) 'object)) 
       v) ; Keep objects as they are
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
                (string-append str " " (format "~a" (car vals)))
                (cdr vals)))))))


(interpret "tests4/1.txt" "A")
(interpret "tests4/2.txt" "A")
(interpret "tests4/3.txt" "A")
;(interpret "tests4/4.txt" "A")
;(interpret "tests4/5.txt")
;(interpret "tests4/6.txt")
;(interpret "tests4/7.txt")
;(interpret "tests4/8.txt")
;(interpret "tests4/9.txt")
;(interpret "tests4/10.txt")
;(interpret "tests4/11.txt")
;(interpret "tests4/12.txt")
;(interpret "tests4/13.txt")