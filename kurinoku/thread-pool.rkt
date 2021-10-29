#lang racket/base

(require racket/contract
         racket/async-channel)

(provide (contract-out
          [make-pool (->* () (exact-positive-integer?) pool?)]
          [pool-put (-> pool? procedure? void?)]
          [pool-close (-> pool? void?)]
          [pool-for-each (->* (pool? procedure? list?) (#:close? any/c) #:rest (listof list?) void?)]
          [for-each/pool (->* (procedure? list?) (#:size exact-positive-integer?) #:rest (listof list?) void?)]
          [map/pool (->* (procedure? list?) (#:size exact-positive-integer?) #:rest (listof list?) list?)]
          [pool? (-> any/c boolean?)]
          [pool-running? (-> pool? boolean?)]
          ))

(struct pool (ths ch))

(define (make-pool [n 4])

  (define ch (make-async-channel))
  
  (define (target)
    (let loop ()
      (define msg (async-channel-get ch))
      (unless (eq? msg 'stop)
        (msg)
        (loop))))

  (pool (for/list ([i (in-range 4)]) (thread target))
        ch))

(define (pool-put p thunk)
  (async-channel-put (pool-ch p) thunk))

(define (pool-close p)
  (define ths (pool-ths p))
  (for ([_ (in-list ths)])
    (pool-put p 'stop))
  (for ([th (in-list ths)])
    (thread-wait th)))

(define (pool-for-each p f l0 #:close? [close? #f] . ls)
  (apply
   for-each (lambda args
              (pool-put p (lambda () (apply f args))))
   l0
   ls)
  (when close? (pool-close p)))

(define (for-each/pool f l0 #:size [size 4] . ls)
  (define p (make-pool size))
  (apply pool-for-each p f l0 ls))

(define (map/pool f l0 #:size [size 4] . ls)
  (define p (make-pool size))
  (define chs (for/list ([_ (in-range size)]) (make-async-channel)))
  (define ((return-thunk ch) . args)
    (lambda ()
      (define result (apply f args))
      (async-channel-put ch result)))
  (define ret-chs (map return-thunk chs))
  (for ([args (in-values-sequence (apply in-parallel l0 ls))]
        [ret-ch (in-cycle (in-list ret-chs))])
    (pool-put p (apply ret-ch args)))
  (begin0
    (for/list ([_ (in-list l0)]
             [ch (in-cycle (in-list chs))])
    (async-channel-get ch))
    (pool-close p)))

(define (pool-running? p)
  (ormap thread-running? (pool-ths p)))
