#lang racket

(require racket/struct)

(provide make-hash-tree
         hash-tree?
         hash-tree-ref
         hash-tree-ref!
         hash-tree-set!
         hash-tree-set*!
         hash-tree-keys
         hash-tree-update!)

(module+ test
  (require rackunit))

(struct hash-tree (root [max-depth #:mutable])
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (o) (format "~a[depth=~a]" 'hash-tree (hash-tree-max-depth o)))
      (lambda (o)
        (list (hash->list (hash-tree-root o))))))])

(struct node-tree (tree has-value? value) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (_) 'node-tree)
      (lambda (obj) `(,@(if (node-tree-has-value? obj) (list (node-tree-value obj)) '())
                      ,(hash->list (node-tree-tree obj))))))])

(define (set-value-of-node-tree! nt value)
  (set-node-tree-value! nt value)
  (set-node-tree-has-value?! nt #t))

(define (make-hash-tree) (hash-tree (make-hash) 0))

(define (make-default-node) (node-tree (make-hash) #f #f))

(define not-found (gensym))

(define (parse-path p)
  (if (list? p) p
      (list p)))

(define (-hash-tree-ref ht path failure-result [hash-ref hash-ref] #:break-when [break-when (lambda (v) #f)])
  (for/fold ([result ht])
            ([p-part (in-list (parse-path path))])
    #:break (break-when result)
    (match result
      [(hash-tree root _)
       (hash-ref root p-part failure-result)]
      [(node-tree tree _ _)
       (hash-ref tree p-part failure-result)])))

(define (hash-tree-ref ht path
                       [failure-result
                        (lambda ()
                          (raise-arguments-error 'hash-tree-ref "no value found for path+" "path" path))])
  (define result (-hash-tree-ref
                  ht path
                  not-found
                  #:break-when (lambda (v) (eq? not-found v))))
  (match result
    [(node-tree _ #t value) value]
    [_ (if (procedure? failure-result) (failure-result) failure-result)]))

(define (hash-tree-set! ht path value)
  (define last
    (-hash-tree-ref
     ht path
     make-default-node
     hash-ref!))
  (define path-len (length (parse-path path)))
  (when (< (hash-tree-max-depth ht) path-len)
    (set-hash-tree-max-depth! ht path-len))
  (set-value-of-node-tree! last value)
  (void))

(module+ test
  (check-match
   (let ([ht (make-hash-tree)])
     (hash-tree-set! ht (list 'a) 'A)
     (hash-tree-set! ht (list 'a 'b) 'A/B)
     (hash-tree-set! ht (list 'c 'd 'e) 'C/D/E)
     ht)
   (hash-tree
    (hash-table ('a (node-tree
                     (hash-table
                      ('b (node-tree (hash-table) #t 'A/B)))
                     #t 'A))
                ('c
                 (node-tree
                  (hash-table
                   ('d
                    (node-tree
                     (hash-table
                      ('e (node-tree (hash-table) #t 'C/D/E)))
                     #f #f)))
                  #f #f)))
    3)))

(define (hash-tree-ref! ht path to-set)
  (define result (-hash-tree-ref ht path make-default-node hash-ref!))
  (match result
    [(node-tree tree #t value) value]
    [(node-tree tree #f _) (set-value-of-node-tree! result to-set) to-set]))

(module+ test
   (let ([ht (make-hash-tree)]
         [uniq (gensym)])
     (check-equal? (hash-tree-ref! ht (list 'a 'b 'c) 'ABC) 'ABC)
     
     (hash-tree-set! ht (list 'a 'c) uniq)
     (check-equal? (hash-tree-ref! ht (list 'a 'c) #f) uniq)
     ))

(define (hash-tree-set*! ht path value . path-then-value)
  (hash-tree-set! ht path value)
  (for ([sl (in-slice 2 (in-list path-then-value))])
    (match sl
      [(list path value)
       (hash-tree-set! ht path value)])))

(define (hash-tree-update! ht path updater
                          [failure-result (lambda ()
                                            (raise-arguments-error
                                             'hash-tree-update
                                             (format "no value found for path: ~e" path)))])
  (define last (-hash-tree-ref ht path make-default-node hash-ref!))
  (match last
    [(node-tree _ #t value)
     (set-value-of-node-tree! last (updater value))]
    [(node-tree _ #f value)
     (set-value-of-node-tree! last (updater (if (procedure? failure-result) (failure-result) failure-result)))]))

(module+ test
  (let ([ht (make-hash-tree)])
    (hash-tree-update! ht '(1 2 3) values #f)
    (check-equal? (hash-tree-ref ht '(1 2 3)) #f)
    (hash-tree-update! ht '(1 2 3) (lambda (v) 'hello))
    (check-equal? (hash-tree-ref ht '(1 2 3)) 'hello)
    ))

(struct ht-key (key)) ; wrap to save lists since we use flatten in some parts

(define ((append-ht-key [prefix '()]) htk)
  (match htk
    [(ht-key k)
     (ht-key (append prefix k))]))

(define (ht-key* v) (ht-key (list v)))

(define (get-all-keys-at-level ht n)
  (cond [(and n (= n 0))
         (match ht
           [(hash-tree root _)
            (map ht-key* (hash-keys root))]
           [(node-tree tree _ _)
            (map ht-key* (hash-keys tree))])]
        [else
         (define ((get-and-append-to-subkeys h) k)
           (let* ([new-ht (hash-ref h k)]
                  [subkeys (flatten (get-all-keys-at-level new-ht (and n (sub1 n))))])
             (map (append-ht-key (list k)) subkeys)))

         (define (do h)
           (cond [(false? n) ;; means fetch every key
                  (flatten
                   (for/list ([k (in-list (hash-keys h))])
                     (list* (ht-key* k)
                            ((get-and-append-to-subkeys h) k))))]
                 [else
                  (map
                   (get-and-append-to-subkeys h)
                   (hash-keys h))]))
         
         (match ht
           [(hash-tree root _)
            (do root)]
           [(node-tree tree _ _)
            (do tree)])]))
            

(define (hash-tree-keys ht [path 'all])
  (cond
    [(not path)
     (match ht
       [(hash-tree root _)
        (map list (hash-keys root))])]
    [(exact-nonnegative-integer? path)
     (map ht-key-key (flatten (get-all-keys-at-level ht path)))]
    [(eq? path 'all)
     (map ht-key-key (flatten (get-all-keys-at-level ht #f)))]
    [(list? path)
     (define last-hash
       (-hash-tree-ref
                  ht path
                  not-found
                  #:break-when (lambda (v) (eq? not-found v))))
     (if (eq? not-found last-hash)
         '()
         (for/list ([k (in-hash-keys ((if (hash-tree? last-hash) hash-tree-root node-tree-tree) last-hash))])
           (append path (list k))))]))

(module+ test
  (define ht
    (let ([ht (make-hash-tree)])
     (hash-tree-set*! ht
                      (list 'A) 'A
                      (list 'A 'C) 'A/C
                      (list 'B 'C) 'B/C
                      (list 'F 'D 'E) 'F/D/E
                      (list 'L (list 1 2)) 'L/1+2)
     ht))
  
  (check-match
     (hash-tree-keys ht 1)
     (list-no-order '(A C) '(B C) '(F D) '(L (1 2))))
  (check-match
   (hash-tree-keys ht 'all)
   (list-no-order '(A) '(A C) '(B) '(B C) '(F) '(F D) '(F D E) '(L) '(L (1 2)))))
                      


     
         
