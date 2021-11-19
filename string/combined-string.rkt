#lang racket

(require racket/generic
         "stringifiable.rkt")

(provide make-combined-string)

(struct combined-string (strs) #:transparent
  #:methods gen:stringifiable
  [(define/generic get-string stringifiable-get-string)
   (define/generic render stringifiable-render)
   (define (stringifiable-get-string stringifiable) (string-join (map get-string (combined-string-strs stringifiable)) ""))
   (define (stringifiable-render stringifiable) (string-join (map render (combined-string-strs stringifiable)) ""))])

(define/contract (make-combined-string . rest)
  (->* () #:rest (listof stringifiable?) combined-string)
  (combined-string rest))
