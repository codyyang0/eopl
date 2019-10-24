(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  
  (require "drscheme-init.scm")
  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")

  ;(provide value-of-program value-of instrument-let instrument-newref)
  (provide (all-defined-out))

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.


;;;;;;;;;;;;;;;; result of program ;;;;;;;;;;;;;;
; A program is a statement. A statement does not return a value, but acts
; by modifying the store and by printing.

(define result-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (stmt1)
         (result-of stmt1 (init-env))))))

(define result-of
  (lambda (stmt env)
    (cases statement stmt
      
      (assign-statement (var exp1)
        (begin
          (setref!
           (apply-env env var)
           (value-of exp1 env))))
      
      (print-statement (exp1)
        (let ((val (value-of exp1 env)))
          (eopl:printf "~s~%" (expval->num val))))
      
      (block-statement (stmts)
        (for-each
         (lambda (stmt)
           (result-of stmt env))
         stmts))
      
      (if-statement (exp1 stmt1 stmt2)
        (if (expval->bool (value-of exp1 env))
            (result-of stmt1 env)
            (result-of stmt2 env)))
      
      (while-statement (exp1 stmt1)
        (if (expval->bool (value-of exp1 env))
            (begin
              (result-of stmt1 env)
              (result-of (while-statement exp1 stmt1) env))
             (eopl:printf "~%")))

      (do-while-statement (stmt1 exp1)
        (begin
          (result-of stmt1 env)
          (if (expval->bool (value-of exp1 env))
              (result-of (do-while-statement stmt1 exp1) env)
              (eopl:printf "do-while statement was done"))))
         
      (vars-statement (vars exps stmts)
        (letrec ((extend-vars-env
                  (lambda (vars exps e-env)
                    (if (null? vars)
                        e-env
                        (extend-vars-env (cdr vars) (cdr exps) (extend-env (car vars) (newref (value-of (car exps) env)) e-env))))))
          (let ((new-env (extend-vars-env vars exps env)))
            (result-of stmts new-env))))
      
      (read-statement (var)
        (setref! (apply-env env var) (num-val (read)))
        (eopl:printf "~s ~%" (deref (apply-env env var))))
      
      )))
                
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
;  (define value-of-program 
;    (lambda (pgm)
;      (initialize-store!)
;      (cases program pgm
;        (a-program (exp1)
;          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 118, 119
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))
;          (let ((val (apply-env env var)))
;            (if (expval? val)
;                val
;                (deref val))))

        ;\commentbox{\diffspec}
        (oper-exp (oper exp1 exp2)
          (let ((val2 (value-of exp2 env))
                (val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              ;(display env)
              (num-val
                ((eval (string->symbol oper)) num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        (not-exp (exp1)
          (bool-val (not (expval->bool (value-of exp1 env)))))
         
        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((v1 (value-of exp1 env)))
            (value-of body
              (extend-env var (newref v1) env))))

;        (let-exp (var exp1 body)
;          (let ((v1 (value-of exp1 env)))
;            (value-of body
;              (extend-env var v1 env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (map (lambda (rand)
                               (value-of rand env)) rands)))
              (newline)
              (eopl:printf "call-exp ~s ~%" args)
              (display env)
              (apply-procedure proc args)))
        
        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))
        
        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of exp1 env))
            (num-val 27)))

        ; mutable variable
        ; 满足exercise 4.21
        ; The effect of the setdynamic expression is to assign temporarily the value of exp1 to
        ; var, evaluate body, reassign var to its original value, and return the value of body.
        ; 这个只是修改了当前环境中，最近的变量在store中的值，无法穿透值函数中定义是闭包中的同名变量值
        ; 除非闭包中的环境与当前的环境均是同一个
        (setdynamic-exp (var exp1 body)
          (let* ((ref (apply-env env var))
                 (origin-val (deref ref)))
            (newline)
            (eopl:printf "old val for ~s : ~s ~%" var origin-val)
            (setref! ref (value-of exp1 env))
            (let ((val (value-of body env)))
              (setref! ref origin-val)
              (newline)
              (eopl:printf "return val: ~s ~%" val)
              val)))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 args)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((new-env (extend-env vars (list->vector args) saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    vars)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env))))))  

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
