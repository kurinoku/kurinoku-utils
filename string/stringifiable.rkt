#lang racket

(require racket/generic)

(provide gen:stringifiable
         stringifiable?
         stringifiable-apply
         stringifiable-get-string
         stringifiable-render)

(define-generics stringifiable
  (stringifiable-get-string stringifiable)
  (stringifiable-render stringifiable)
  #:fast-defaults
  ([string? (define stringifiable-get-string values)
            (define stringifiable-render values)]))

(define stringifiable-apply
  (make-keyword-procedure
   (lambda (kw-list kwargs proc . rest)
     (define (try-get-string x)
       (if (stringifiable? x)
           (stringifiable-get-string x)
           x))
     (keyword-apply proc
                    kw-list
                    (map try-get-string kwargs)
                    (map try-get-string rest)))))