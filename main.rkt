#lang racket


(require racket/pretty (for-syntax syntax/parse))
(provide match/count match*/count)

(define-syntax (match/count stx)
  (define-syntax-class cl
    (pattern [pat body ... body0]
             #:with p* (syntax/loc #'pat (pat))
             #:with e #'[p* body ... body0]))
  (syntax-parse stx
    [(_ arg c:cl ...)
     #'(match*/count (arg) c.e ...)]))

(define-syntax (match*/count stx)
  (syntax-parse stx
    [(_ args [pat body ... body0] ...)
     (define x (syntax-local-lift-expression #'(make-hash)))
     (syntax-local-lift-expression
      #`(exit-handler 
         (let ([ex (exit-handler)])
           (lambda (n)
             (pretty-print (sort #:key cdr (hash->list #,x) >))
             (flush-output)
             (ex n)))))
     (syntax-local-lift-expression
      #`(executable-yield-handler 
         (let ([ex (executable-yield-handler)])
           (lambda (n)
             (pretty-print (sort #:key cdr (hash->list #,x) >))
             (flush-output)
             (ex n)))))
     (with-syntax ([(k ...) (for/list ([p (syntax->list #'(pat ...))])
                              (syntax-line p))])
       #`(match* args
           [pat body ... (hash-set! #,x k (add1 (hash-ref #,x k 0))) body0] ...))]))