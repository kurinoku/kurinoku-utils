#lang racket/base

(require racket/class
         racket/gui/base)

(provide kurinoku:list-box%)

(define kurinoku:list-box%
  (class list-box%
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
      (void))
    
    ))
      
