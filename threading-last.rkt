#lang racket

(define-syntax ->>
  (syntax-rules ()
    ((->> a) a)
    ((->> a (b bs ...) c ...)
     (->> (b bs ... a) c ...))
    ((->> a b c ...) (->> (b a) c ...))))
