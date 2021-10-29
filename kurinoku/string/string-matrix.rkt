#lang racket

(require "stringifiable.rkt")

(module+ test
  (require rackunit))

(provide render-mat
         make-simple-matrix
         make-mat)

(define (get-max-height-in-row row)
  (for/fold ([last 0])
            ([x row])
    (max (sequence-length x) last)))

(define (get-max-width-in-column table col-index)
  (for/fold ([last 0])
            ([row table])
    (max last
         (for/fold ([last 0])
                   ([s (sequence-ref row col-index)])
           (max last
                (stringifiable-apply string-length s))))))

(define matrix-row/c (listof (listof stringifiable?)))
(define matrix/c (listof matrix-row/c))

(define/contract (render-mat matrix)
  (-> matrix/c string?)
  (define mx-heights (for/list ([row matrix]) (get-max-height-in-row row)))
  (define mx-widths (for/list ([col (in-range (sequence-length (sequence-ref matrix 0)))]) (get-max-width-in-column matrix col)))

  (string-join
   (for/list ([row (in-list matrix)]
              [mx-height (in-list mx-heights)]
              #:when #t
              [cur (in-range (max mx-height 1))])
     (string-join
      (for/list ([col (in-list row)]
                 [mx-width (in-list mx-widths)]
                 #:when #t
                 [x (in-value (sequence-ref (in-sequences col (in-cycle (in-value ""))) cur))])
        (~a #:min-width mx-width (stringifiable-render x)))
      " "))
   "\n"))
             
(define/contract (make-simple-matrix col-count . rest)
  (->* (exact-positive-integer?) #:rest (listof stringifiable?) matrix/c)
  (for/list ([c (in-slice col-count rest)])
    (map list c)))
          
(define/contract (make-mat col-count row-count filler)
  (-> exact-positive-integer? exact-positive-integer?
      (listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? stringifiable?))
      matrix/c)

  (define (fix-hash h) (for/hash ([(k v) (in-hash h)]) (values k (reverse v))))
  (define hash-filler
    (fix-hash
     (for/fold ([h (hash)])
               ([fill (in-list filler)])
       (match fill
         [(list col row str)
          (hash-update h (list col row) (lambda (lst) (list* str lst)) '())]))))

  (for/list ([row (in-range row-count)])
    (for/list ([col (in-range col-count)])
      (hash-ref hash-filler (list col row) '()))))

(module+ test
  (check-equal? (make-mat 2 2 '((0 0 "a") (0 0 "b") (1 1 "tcd"))) '((("a" "b") ()) (() ("tcd"))))
  )
  


                  