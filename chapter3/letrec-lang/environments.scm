(module environments (lib "eopl.ss" "eopl") 


  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")




  
  
  (provide (all-defined-out))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env '(i v x) (list (num-val 1) (num-val 5) (num-val 10)) (empty-env))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  
  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (p-vars vals saved-env)
          (let loop ((index 0)
                     (vars p-vars))
            (cond ((null? vars) (apply-env saved-env search-sym))
                  ((eqv? search-sym (car vars))
                   (if (not (vector? vals))
                    (car vals)
                    (vector-ref vals index)))
                  (else
                   (loop (+ index 1) (cdr vars)))))))))
  )