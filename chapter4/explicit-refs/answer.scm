(module answer (lib "eopl.ss" "eopl")

  (require "data-structures.scm")
  (require "store.scm")

  (provide (all-defined-out))

  (define-datatype answer answer?
    (an-answer
     (val expval?)
     (store store?)))

)