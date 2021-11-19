#lang racket/base

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide match-port-with-regexp-try
         regexp-match-port)

(module+ test
  (require rackunit))

(define (match-port-with-regexp-try in clauses)
  (let/cc return
    (for ([clause (in-list clauses)])
      (let/cc continue
        (match clause
          [(list re f)
           (return (f (regexp-try-match re in) continue))])))))
         

(define-syntax (regexp-match-port stx)
  (syntax-parse stx
    [(_ port-in
        [re m body ...+]
        ...+)

     #`(match-port-with-regexp-try port-in
                                   (list (list re (lambda (res continue)
                                                    (match res
                                                      [m body ...]
                                                      [_ (continue (void))])))
                                         ...))]))

(module+ test
  (check-equal? (regexp-match-port (open-input-string "AAA\n")
                                  [#px"^\\s+" (list _) 'blank]
                                  [#px"^(\\S*)" (list _ A) 'A])
                'A)
  (check-equal? (regexp-match-port (open-input-string "\nasdasdasd")
                                  [#px"^\\s+" (list _) 'blank]
                                  [#px"^(\\S*)" (list _ A) 'A])
                'blank))
      
