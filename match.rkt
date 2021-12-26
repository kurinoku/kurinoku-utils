#lang racket/base


(require racket/match
         racket/function
         (for-syntax racket/base
                     syntax/parse))

(provide regexp*)

(module+ test
  (require rackunit
           racket/path))

(define re-value? (disjoin regexp? byte-regexp? string? bytes?))

(define can-match? (disjoin string? bytes? path? input-port?))

(define ((match-regexp-helper pat) value)
  (cond
    [(input-port? value) (regexp-try-match pat value)]
    [else (regexp-match pat value)]))
    

(define-match-expander regexp*
  (lambda (stx)
    (syntax-parse stx
      [(_ rx-expr pat)
       #:declare rx-expr (expr/c #'re-value?)
       #'(? can-match? (app (match-regexp-helper rx-expr.c) pat))])))

(module+ test
  (check-equal?
   (match (string->some-system-path "/dev/null" 'unix)
     [(regexp* #px"^/dev/(null)$" (list _ n)) n])
   "null")
  (check-equal?
   (match (open-input-string "a b c")
     [(regexp* #px"^\\s+" (list x)) x]
     [(regexp* #px"^a" (list x)) x])
   #"a")

  )

  
