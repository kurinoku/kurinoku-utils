#lang racket/base

(require db/unsafe/sqlite3)
  
(provide sqlite3-regexp-match?
         db-sqlite3-install-regexp-procedure)

(define (sqlite3-regexp-match? pat x)
  (if (regexp-match? (pregexp pat) x) 1 0))

(define (db-sqlite3-install-regexp-procedure conn)
  (sqlite3-create-function conn 'regexp 2 sqlite3-regexp-match?))
