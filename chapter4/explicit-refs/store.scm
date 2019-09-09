(module store (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
   
  (provide initialize-store! reference? newref deref setref!
    instrument-newref get-store-as-list)
  
  (define instrument-newref (make-parameter #f))
  
  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;
  
  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
;  (define empty-store
;    (lambda () '()))
  (define empty-store
    (lambda ()
      (vector (make-vector 1024) 999)))
  ; 使用多维数组来表示store
  ; 第一个级别的size为1024，其中1-1000[0-999]存放expval
  ; 第一个级别的1001->1012[1000-1011]存放第二级别的vector,第二级别每个vector存放1024个expval
  ; 第一个级别的1013->1024[1012-1023]存放第三级别的vector
  ; 第三级别的vector，每个都有256个slot，存放第二级别的vector

  (define store->vec
    (lambda (store)
      (vector-ref store 0)))

  (define store->ref
    (lambda (store)
      (vector-ref store 1)))
  
  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  (define get-store
    (lambda () the-store))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (and (integer? v)
           (not (negative? v)))))

  (define level-1-buckets 1000)
  (define level-2-buckets 12)
  (define level-3-buckets 256)
  (define slots 1024)

  ; integer -> vector(slot-no)
  ; integer -> vector(level-1-bucket-no, slot-no)
  ; integer -> vector(level-1-bucket-no, level-2-bucket-no, slot-no)
  (define index
    (lambda (references)      
      (cond ((< references level-1-buckets) (vector references))
            ((< references (+ level-1-buckets
                              (* level-2-buckets slots)))
             (let* ((bucket-no (quotient (- references level-1-buckets) slots))
                    (slot-no   (modulo   (- references level-1-buckets) slots)))
               (vector (+ level-1-buckets bucket-no) slot-no)))
            (else
             (let* ((level-1-bucket-no
                     (quotient (- references
                                  level-1-buckets
                                  (* level-2-buckets slots))
                               (* level-3-buckets slots)))
                    (level-2-bucket-no
                     (quotient (- references
                                  level-1-buckets
                                  (* level-2-buckets slots)
                                  (* level-1-bucket-no level-3-buckets slots))
                               slots))
                    (slot-no
                     (modulo (- references
                                  level-1-buckets
                                  (* level-2-buckets slots)
                                  (* level-1-bucket-no level-3-buckets slots))
                             slots)))
               (vector (+ level-1-buckets level-2-buckets level-1-bucket-no)
                       level-2-bucket-no
                       slot-no))))))
  
  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (val)      
      (let* ((store (get-store))
             (vec (store->vec store))
             (next-reference (+ (store->ref store) 1))
             (refer-vec (index next-reference))
             (refer-vec-length (vector-length refer-vec)))
        (begin
          (cond ((equal? refer-vec-length 1)
                 (let ((bucket-no (vector-ref refer-vec 0)))
                   (vector-set! vec bucket-no val)))
                ((equal? refer-vec-length 2)
                 (let ((level-1-bucket-no (vector-ref refer-vec 0))
                       (slot-no (vector-ref refer-vec 1)))
                   (begin 
                     (if (not (vector? (vector-ref vec level-1-bucket-no)))
                         (vector-set! vec level-1-bucket-no (make-vector slots))
                         (display "level-1-bucket need not extend"))
                     (vector-set! (vector-ref vec level-1-bucket-no) slot-no val))))
                ((equal? refer-vec-length 3)
                 (let ((level-1-bucket-no (vector-ref refer-vec 0))
                       (level-2-bucket-no (vector-ref refer-vec 1))
                       (slot-no (vector-ref refer-vec 2)))
                   (begin
                     (if (not (vector? (vector-ref vec level-1-bucket-no)))
                         (vector-set! vec level-1-bucket-no (make-vector level-3-buckets))
                         (display "level-1-bucket need not extend"))
                     (if (not (vector? (vector-ref (vector-ref vec level-1-bucket-no) level-2-bucket-no)))
                         (vector-set! (vector-ref vec level-1-bucket-no) level-2-bucket-no (make-vector slots))
                         (display "level-2-bucket need not extend"))
                     (vector-set! (vector-ref (vector-ref vec level-1-bucket-no) level-2-bucket-no) slot-no val))))
                (else
                 (eopl:error "reference is wrong")))
          (vector-set! store 1 next-reference)
          next-reference))))
          
  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref
    (lambda (ref)
      (let ((index-list (index ref)))
        (let loop ((v (store->vec (get-store)))
                   (index 0))
          (if (not (vector? (vector-ref v (vector-ref index-list index))))
              (vector-ref v (vector-ref index-list index))
              (loop (vector-ref v (vector-ref index-list index)) (+ index 1)))))))
      

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!                       
    (lambda (ref val)
      (let ((store-ref (store->ref (get-store)))
            (store-vec (store->vec (get-store))))
        (if (> ref store-ref)
            (begin
              (display "wrong reference no")
              (newline))
            (let* ((index-vec (index ref))
                  (refer-vec-length (vector-length index-vec)))
               (cond ((equal? refer-vec-length 1) (vector-set! store-vec ref val))
                     ((equal? refer-vec-length 2) (vector-set!
                                                   (vector-ref store-vec (vector-ref index-vec 0))
                                                   (vector-ref index-vec 1)
                                                   val))
                     (else
                      (vector-set!
                       (vector-ref (vector-ref store-vec (vector-ref index-vec 0)) (vector-ref index-vec 1))
                       (vector-ref index-vec 2)
                       val))))))))
                    

  (define report-invalid-reference
    (lambda (ref the-store)
      (eopl:error 'setref
        "illegal reference ~s in store ~s"
        ref the-store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
   (define get-store-as-list
     (lambda ()
       (letrec
         ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                '()
                (cons
                  (list n (car sto))
                  (inner-loop (cdr sto) (+ n 1)))))))
         (inner-loop the-store 0))))

  )