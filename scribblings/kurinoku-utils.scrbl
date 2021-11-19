#lang scribble/manual
@require[@for-label[kurinoku
                    kurinoku/hash-tree
                    racket/base
                    racket/contract/base]]

@title{kurinoku-utils}
@author{kurinoku}

@defmodule[kurinoku]

Miscellanous utilities

@defmodule[kurinoku/hash-tree]

@defproc[(hash-tree? [v any/c]) boolean?]
@defproc[(make-hash-tree) hash-tree?]
@defproc[(hash-tree-ref [ht hash-tree?]
                        [path (or list? (and any/c (not/c list?)))]
                        [failure-result failure-result/c #f])
         any/c]
@defproc[(hash-tree-set! [ht hash-tree?]
                         [path (or list? (and any/c (not/c list?)))]
                         [value any/c])
         void?]
@defproc[(hash-tree-set*! [ht hash-tree?]
                          [path (or list? (and any/c (not/c list?)))]
                          [value any/c]
                          ... ...)
         void?]
@defproc[(hash-tree-keys [ht hash-tree?] [path (or/c 'all #f exact-nonnegative-integer? list?)])
         list?]


