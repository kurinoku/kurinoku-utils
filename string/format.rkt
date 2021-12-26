#lang racket/base

(require racket/match
         racket/stream
         data/integer-set)

(provide format/hash:find-positions
         format/hash:replace-positions
         format/hash)

(module+ test
  (require rackunit))

(define DEFAULT-PLACEHOLDER-REGEX #px"\\{([^\\{\\}]*)\\}")

(define (format/hash:find-positions fmt [placeholder DEFAULT-PLACEHOLDER-REGEX])
  (let ([positions (regexp-match-positions* #:match-select values placeholder fmt)])
    (for/list ([p (in-list positions)])
      (match p
        [(list p (cons start end))
         (vector p (substring fmt start end))]))))

(define ((on-not-found who) h key)
  (raise-arguments-error
   who
   "placeholder not found in hash table"
   "placeholder" key
   "hash table" h))

(define (format/hash:replace-positions positions fmt h [on-not-found (on-not-found 'format/hash:replace-positions)])
  (define marked/stream (sequence->stream (in-list positions)))

  (for/fold ([result '()]
             [marked marked/stream]
             
             #:result (list->string (reverse result)))
            ([c (in-string fmt)]
             [i (in-naturals)]
             #:when #t
             [current-interval (in-value (if (stream-empty? marked) #f (stream-first marked)))])
    (define-values (start end key)
      (match current-interval
        [#f (values #f #f #f)]
        [(vector (cons start end) key) (values start end key)]))

    (define (add-c) (values (list* c result) marked))
    
    (define (hash-ref-error)
      (on-not-found h key))
    
    (cond
      [(not start) (add-c)]
      [(equal? start i)
       (values
        (append (reverse (string->list (hash-ref h key hash-ref-error))) result)
        marked)]
      [(equal? (sub1 end) i) (values result (stream-rest marked))]
      [(< start i end) (values result marked)]
      [else (add-c)])))

(define (format/hash fmt h #:placeholder [placeholder DEFAULT-PLACEHOLDER-REGEX])
  (define positions (format/hash:find-positions fmt placeholder))
  (format/hash:replace-positions positions fmt h (on-not-found 'format/hash)))

(module+ test
  (check-equal? (format/hash "{} abc {ultra}." (hash "" "hello" "ultra" "man"))
                "hello abc man.")

  (check-exn #px"^format/hash: placeholder not found in hash table"
             (lambda () (format/hash "{}" (hash))))
  )
