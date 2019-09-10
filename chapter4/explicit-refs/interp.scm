(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  (require "answer.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env) (get-store))))))
  
  ;; value-of : Exp * Env * Store -> Answer
  ;; Page: 113
  (define value-of
    (lambda (exp env store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num)
          (an-answer (num-val num) store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var)
          (an-answer
           (apply-store store (apply-env env var))
           store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (cases answer (value-of exp1 env store)
            (an-answer (v1 new-store1)
              (cases answer (value-of exp2 env new-store1)
                (an-answer (v2 new-store2)
                  (let ((num1 (expval->num v1))
                        (num2 (expval->num v2)))
                    (an-answer (num-val (- num1 num2)) new-store2)))))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (cases answer (value-of exp1 env store)
            (an-answer (v1 new-store)
              (let ((num1 (expval->num v1)))
                (if (zero? num1)
                    (an-answer (bool-val #t) new-store)
                    (an-answer (bool-val #f) new-store))))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (cases answer (value-of exp1 env store)
            (an-answer (val new-store)
              (if (expval->bool val)
                  (value-of exp2 env new-store)
                  (value-of exp3 env new-store)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)
          (cases answer (value-of exp1 env store)
            (an-answer (v1 new-store1)
              (let* ((new-store2 (extend-store new-store1 v1))
                     (ref (store->ref new-store2)))
                (value-of body (extend-env ref env) new-store2)))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env store)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

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

        (list-exp (exp1 exps)
          (let ((val (ref-val (newref (value-of exp1 env)))))
            (letrec
                ((value-of-lists
                  (lambda (es)
                    (if (null? es)
                        val
                        (begin
                          (newref (value-of (car es) env))
                          (value-of-lists (cdr es)))))))
              (value-of-lists exps))))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (cases answer (value-of exp1 env store)
            (an-answer (v1 new-store)
              (let ((ref1 (expval->ref v1)))
                (an-answer (deref ref1) new-store)))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env store)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
