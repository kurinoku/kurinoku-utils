#lang info
(define collection "kurinoku")
(define deps '("db-lib"
               "gui-lib"
               "base" "sugar" "static-rename"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/kurinoku-utils.scrbl" ())))
(define pkg-desc "Misc utils by and for kurinoku")
(define version "0.0")
(define pkg-authors '(kurinoku))
