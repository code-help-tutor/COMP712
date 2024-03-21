#lang eopl
;;;
;;;========== Interpreter for MULT-lang ==========
;;;

(require "multlang.rkt")
(require "data-structures.rkt")

(provide value-of-program value-of)

;;; The initial environment
(define init-env empty-env)

;;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program (exp1) (value-of exp1 (init-env))])))

;;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (id) (lookup-env env id))
      (primcall-exp (prim rands)
                    (let ((args (values-of rands env)))
                      (apply-primitive prim args)))   
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      [let-exp (vars exps body) (let loop ([env env]
                                           [vars vars]
                                           [exps exps])
                                  (if (null? vars)
                                      (value-of body env)
                                      (loop (extend-env (car vars) (value-of (car exps) env) env)
                                            (cdr vars)
                                            (cdr exps))))]
      (proc-exp (bvars body)
                (procedure bvars body env))
      [call-exp (rator rands) (let ([proc (expval->proc (value-of rator env))]
                                    [args (map (lambda (rand)
                                                 (value-of rand env))
                                               rands)])
                                (apply-procedure proc args))]
      [letrec-exp (p-names b-vars p-bodies letrec-body) (value-of letrec-body
                                                                  (extend-env-rec p-names b-vars p-bodies env))])))

(define (values-of exps env)
  (map (lambda (exp) (value-of exp env))
       exps))

;;; apply-procedure : Procedure * Expval -> Expval

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      [procedure (vars body saved-env)
                 (let loop ([env saved-env]
                            [vars vars]
                            [args args])
                   (if (null? vars)
                       (value-of body env)
                       (loop (extend-env (car vars) (car args) env)
                             (cdr vars)
                             (cdr args))))])))

;; Interface.

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; Provides.

(provide num-val bool-val run)