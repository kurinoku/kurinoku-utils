#lang racket

(require racket/random
         racket/generic
         sugar
         "stringifiable.rkt")

(provide ->colored-string
         make-colorizer/random
         try-render-color)

(module+ sugar
  (provide (rename-out [:->string ->string]
                       [:stringish? stringish?])))

(module+ test
  (require rackunit))

(struct colored-string (str color bg) #:transparent
  #:methods gen:stringifiable
  [(define (stringifiable-get-string stringifiable) (colored-string-str stringifiable))
   (define (stringifiable-render stringifiable) (try-render-color stringifiable))])

(define *color-symbols*
  (hash 'black 30
        'red 31
        'green 32
        'yellow 33
        'blue 34
        'magenta 35
        'cyan 36
        'white 37
        #f #f
        'reset 0))

(define (get-good-bg c)
  (case c
    [(black red blue) 'white]
    [else 'black]))

(define *bg-colors*
  (hash 'black 40
        'red 41
        'green 42
        'yellow 43
        'blue 44
        'magenta 45
        'cyan 46
        'white 47
        #f #f
        'reset 0))

(define color/c (apply or/c (hash-keys *color-symbols*)))
(define bg-color/c (apply or/c (hash-keys *bg-colors*)))

(define (get-color s)
  (hash-ref *color-symbols* s))

(define (get-bg-color s)
  (hash-ref *bg-colors* s))

(define (:stringish? x)
  (or (colored-string? x)
      (stringish? x)))

(define (:->string x)
  (cond
    [(colored-string? x) (colored-string-str x)]
    [else (->string x)]))

(define/contract (try-render-color s)
  (-> any/c any)
  (match s
    [(colored-string str c bg)
     (define (apply-color c str reset)
       (if c
           (format "\033[~am~a\033[~am" c str reset)
           str))
     (apply-color
      (get-bg-color bg)
      (apply-color
       (get-color c)
       str
       (get-color 'reset))
      (get-bg-color 'reset))]
    [_ s]))

(define/contract (->colored-string str [color #f] [bg #f])
  (->* (string?) (color/c bg-color/c) colored-string?) 
  (colored-string str color bg))

(define/contract (make-colorizer/random)
  (-> (-> string? colored-string?))
  (define c (random-ref (remove* '(reset #f) (hash-keys *color-symbols*))))
  (lambda (str)
    (->colored-string str c (get-good-bg c))))
  

