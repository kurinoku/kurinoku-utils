#lang racket/base

(require racket/dict
         racket/class
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide send/keyword-apply/dict)

(define-syntax (send/keyword-apply/dict stx)
  (syntax-parse stx
    [(_ obj-expr:expr method-id:id kw-dict pos-arg:expr ... pos-args (~seq kw:keyword kw-arg:expr ) ...)
     #:declare pos-args (expr/c #'list?)
     #:declare kw-dict (expr/c #'dict?)

     #'(let* ([dct kw-dict.c]
              [dct (if (immutable? dct)
                       (dict-set dct (~@ 'kw kw-arg) ...)
                       (let ([copy (dict-copy dct)])
                         (dict-set! copy (~@ 'kw kw-arg) ...)
                         copy))]
              [kw-lst (sort (dict-keys dct) keyword<?)]
              [kwargs (for/list ([k (in-list kw-lst)]) (dict-ref dct k))])
         (send/keyword-apply obj-expr method-id kw-lst kwargs pos-arg ... pos-args.c))]))


