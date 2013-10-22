#lang racket


(require racket/pretty (for-syntax syntax/parse))
(provide match/count match*/count)

(define-syntax (match/count stx)
  (syntax-parse stx
    [(_ arg [pat body ... body0] ...)
     #'(match*/count (arg) [(pat) body ... body0] ...)]))

(define-syntax (match*/count stx)
  (syntax-parse stx
    [(_ args [pat body ... body0] ...)
     (define x (syntax-local-lift-expression #'(make-hash)))
     (syntax-local-lift-expression
      #`(exit-handler 
         (let ([ex (exit-handler)])
           (lambda (n) (pretty-print (sort #:key cdr (hash->list #,x) >)) (ex n)))))
     (with-syntax ([(k ...) (for/list ([p (syntax->list #'(pat ...))])
                              (syntax-line p))])
       #`(match* args
           [pat body ... (hash-set! #,x k (add1 (hash-ref #,x k 0))) body0] ...))]))