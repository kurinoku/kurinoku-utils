#lang racket/base

(require racket/dict
         racket/class
         racket/list
         static-rename
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide send/keyword-apply/dict)

(module+ test
  (require racket/class
           racket/string
           rackunit))

(define -send/keyword-apply/dict
  (let ([send/keyword-apply/dict
         (make-keyword-procedure
          (lambda (kw kwargs m-proc obj kw-dict . rest)
            (define-values (pos-arg pos-args)
              (let-values ([(p ps) (split-at-right rest 1)])
                (values p (first ps))))

            (define (dict-set*!/copy dct . rest)
              (let ([copy (dict-copy dct)])
                (apply dict-set*! copy rest)
                copy))

            (define interleaved-kws (map cons kw kwargs))
     
            (let* ([h (make-hash (append (dict->list kw-dict) interleaved-kws))]
                   [alist (sort (hash->list h) keyword<? #:key car)]
                   [new-kw (map car alist)]
                   [new-kwargs (map cdr alist)])
              (m-proc obj new-kw new-kwargs (append pos-arg pos-args)))))])
    send/keyword-apply/dict))

(define-syntax (send/keyword-apply/dict stx)
  (syntax-parse stx
    [(_ obj-expr:expr method-id:id x ...)
     #'(-send/keyword-apply/dict
        (lambda (obj kw kwargs rest)
          (send/keyword-apply obj method-id kw kwargs rest))
        obj-expr
        x ...)]))

(define-syntax (__send/keyword-apply/dict stx)
  (syntax-parse stx
    [(_ obj-expr:expr method-id:id kw-dict pos-arg:expr ... pos-args (~seq kw:keyword kw-arg:expr ) ...)
     #:declare pos-args (expr/c #'list?)
     #:declare kw-dict (expr/c #'dict?)

     #'(let* ([dct kw-dict.c]
              [dct (if (immutable? dct)
                       (dict-set* dct (~@ 'kw kw-arg) ...)
                       (let ([copy (dict-copy dct)])
                         (dict-set*! copy (~@ 'kw kw-arg) ...)
                         copy))]
              [kw-lst (sort (dict-keys dct) keyword<?)]
              [kwargs (for/list ([k (in-list kw-lst)]) (dict-ref dct k))])
         (send/keyword-apply obj-expr method-id kw-lst kwargs pos-arg ... pos-args.c))]))

(module+ test
  (define c%
    (class object%
      (super-new)

      (define (inner-string-join #:sep [sep " "] #:before-first [before-first ""] . args)
        (string-join #:before-first before-first args sep))

      (public [inner-string-join string-join])
      ))

  (define c (new c%))

  (check-equal? (send/keyword-apply/dict c string-join (hash '#:sep "") '("a" "b") #:before-first ">")
                ">ab")
  )