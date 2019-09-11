(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?
  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

(define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  
;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar (lambda (var)
              (or (symbol? var)
                  (list-of var))))
      (bval (lambda (val)
              (or (reference? val)
                  (vector? val))))
      (saved-env environment?)))

  (define extend-env-rec*
    (lambda (proc-names b-vars proc-bodies saved-env)
      (let ((vec-length (length proc-names)))
        (let ((vec (make-vector vec-length)))
          (let ((new-env (extend-env proc-names vec saved-env)))
            (letrec ((init-vec
                      (lambda (i)
                        (if (< i vec-length)
                            (vector-set!
                             vec i
                             (newref
                              (proc-val (procedure (list-ref b-vars i) (list-ref proc-bodies i) new-env))))
                            (init-vec (+ i 1))))))
              (init-vec 0)
              new-env))))))
  
  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym val)              ; val is a denoted value-- a
                                        ; reference. 
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

)
