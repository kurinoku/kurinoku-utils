#lang racket/base

(require racket/class
         racket/gui/base)

(provide kurinoku:list-box%
         list-box-append*-mixin
         list-box-key-event-callback-mixin)

(define (list-box-append*-mixin lb%)
  (class lb%
    (super-new)
    (inherit append
             get-number
             set-string)

    (define/public (append* data str0 . strs)
      (define index (get-number))
      (append str0 data)
      (for ([s (in-list strs)]
            [i (in-naturals 1)])
        (set-string index s i))
      (void))))

(define (list-box-key-event-callback-mixin lb%)
  (class lb%
    (super-new)
    (init-field [(key-callback on-key-event) #f])

    (define/override (on-subwindow-char receiver event)
      (or (super on-subwindow-char receiver event)
          (and key-callback (key-callback receiver event))))

    ))

(define kurinoku:list-box%
  ((compose
    list-box-key-event-callback-mixin
    list-box-append*-mixin)
   list-box%))
    
