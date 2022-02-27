#lang racket/base

(require racket/contract
         racket/bool
         db)

(provide (contract-out
          #:unprotected-submodule no-contract
          [query-user-version (-> connection? integer?)]
          [set-user-version! (-> connection? integer? void?)]
          [sql-null->false-or-apply (-> any/c (-> any/c any/c) any)]
          [false->sql-null-or-apply (-> any/c (-> any/c any/c) any)]
          [db-simple-result-insert-id (-> simple-result? (or/c integer? #f))]))

(define (query-user-version conn)
  (query-value conn "PRAGMA user_version"))

(define (set-user-version! conn new-version)
  (query-exec conn (format "PRAGMA user_version=~a" new-version)))

(define (sql-null->false-or-apply v f)
  (if (sql-null? v)
      #f
      (f v)))

(define (false->sql-null-or-apply v f)
  (if (false? v)
      sql-null
      (f v)))

(define (db-simple-result-insert-id result)
  (cdr (assoc 'insert-id (simple-result-info result))))
