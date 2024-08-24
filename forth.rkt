#lang racket

(provide forth dup swap over rot drop- /mod mod)

(define-syntax forth
  (syntax-rules ()
    ((forth x ...) (forthify x ...))))

(define (forthify . exprs)
  (foldl
   (λ (x acc)
     (if (procedure? x)
       (let* ((arity (procedure-arity x))
              (n (cond
                   ((number? arity) arity)
                   ((procedure-arity? arity) 2)
                   (else (error (format "procedure ~a has invalid arity ~a" x arity)))))
              (res (apply (procedure-reduce-arity x n) (take acc n))))
         (append
          (if (list? res) res (list res))
          (drop acc n)))
       (cons x acc)))
   empty
   exprs))

(define (dup top)
  (list top top))

(define (swap a b)
  (list b a))

(define (over a b)
  (list b a b))

(define (rot a b c)
  (list c a b))

(define (drop- top)
  empty)

(define (/mod a b)
  (list (remainder b a) (quotient b a)))

(define (mod a b)
  (remainder b a))
