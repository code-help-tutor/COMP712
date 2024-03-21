#lang eopl
;;;
;;; Data structures for MULT-lang
;;;
(require "multlang.rkt")        
(provide (all-defined-out))

;;;========= Expressed Values ===========

;;; Expressed values

(define-datatype expval expval?
  [num-val [value number?]]
  [bool-val [boolean boolean?]]
  [proc-val [proc proc?]])

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s" variant value)))

(define expval->num
  (lambda (v)
    (cases expval v
      [num-val (num) num]
      [else (expval-extractor-error 'num v)])))

(define expval->bool
  (lambda (v)
    (cases expval v
      [bool-val (bool) bool]
      [else (expval-extractor-error 'bool v)])))

(define expval->proc
  (lambda (v)
    (cases expval v
      [proc-val (proc) proc]
      [else (expval-extractor-error 'proc v)])))

;;;========= Procedures =========
(define-datatype proc proc?
  [procedure [bvars (list-of symbol?)]
             [body expression?]
             [env environment?]])

;;;========= Primitives =========
(define (apply-primitive prim args)
  (cases primitive prim
    (add-prim ()
              (let ((val1 (expval->num (car args)))
                    (val2 (expval->num (cadr args))))
                (num-val (+ val1 val2))))
    (subtract-prim ()
                   (let ((val1 (expval->num (car args)))
                         (val2 (expval->num (cadr args))))
                     (num-val (- val1 val2))))
    (mult-prim ()
               (let ((val1 (expval->num (car args)))
                     (val2 (expval->num (cadr args))))
                 (num-val (* val1 val2))))
    ))

;;;========= Environment Structures ===========

(define-datatype environment environment?
  [empty-env]
  [extend-env [bvar symbol?]
              [bval expval?]
              [saved-env environment?]]
  [extend-env-rec [ids (list-of symbol?)]
                  [bvars (list-of (list-of symbol?))]
                  [bodies (list-of expression?)]
                  [saved-env environment?]])

(define init-env empty-env)

(define lookup-env
  (lambda (env search-sym)
    (cases environment env
      [empty-env () (eopl:error 'apply-env "No binding for ~s" search-sym)]
      [extend-env (var val saved-env)
                  (if (eqv? search-sym var)
                      val
                      (lookup-env saved-env search-sym))]
      [extend-env-rec (p-names b-vars p-bodies saved-env)
                      (let loop ([p-names p-names]
                                 [b-vars b-vars]
                                 [p-bodies p-bodies])
                        (if (null? p-names)
                            (lookup-env saved-env search-sym)
                            (if (eqv? search-sym (car p-names))
                                (proc-val (procedure (car b-vars)
                                                     (car p-bodies)
                                                     env))
                                (loop (cdr p-names)
                                      (cdr b-vars)
                                      (cdr p-bodies)))))])))
