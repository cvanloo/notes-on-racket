#lang racket

(define-syntax ->
  (syntax-rules ()
    ((-> a) a)
    ((-> a (b bs ...) c ...)
     (-> (b a bs ...) c ...))
    ((-> a b c ...) (-> (b a) c ...))))
