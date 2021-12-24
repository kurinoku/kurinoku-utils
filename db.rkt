#lang racket/base

(require racket/contract
         db)

(provide (contract-out
          #:unprotected-submodule no-contract
          [query-user-version (-> connection? integer?)]
          [set-user-version! (-> connection? integer? void?)]))

(define (query-user-version conn)
  (query-value conn "PRAGMA user_version"))

(define (set-user-version! conn new-version)
  (query-exec conn (format "PRAGMA user_version=~a" new-version)))
