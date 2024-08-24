#lang racket

(provide forth)

;(define-syntax forth
;  (syntax-rules ()
;    [(forth x ...) (forthify x ...)]))

(define-syntax-rule (forth x ...)
  (forthify x ...))

(define (forthify . exprs)
  ;(displayln (format "forthify ~a" exprs) (current-output-port))
  (foldl
   (λ (x acc)
     (if (procedure? x)
       (list (apply x acc))
       (append acc (list x))))
   empty
   exprs))
