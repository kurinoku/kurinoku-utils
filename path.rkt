#lang racket/base

(require racket/list
         static-rename)

(provide get-path-name
         path-name
         get-path-base
         path-base
         path-must-be-dir?)

(define ((-split-path select) p)
  (select
   (call-with-values
    (lambda ()
      (split-path p))
    list)))

(define get-path-name (static-rename get-path-name (-split-path second)))
(define path-name get-path-name)
(define get-path-base (static-rename get-path-base (-split-path first)))
(define path-base get-path-base)
(define path-must-be-dir? (static-rename path-must-be-dir? (-split-path third)))
     

