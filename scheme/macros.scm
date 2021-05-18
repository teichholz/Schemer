(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e0) ...) e1 e2 ...)
       (syntax (syntax-case (list e0 ...) ()
                 [(p ...) (begin e1 e2 ...)]))])))


(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ([p (open-input-file fn)])
          (let f ([x (read p)])
            (if (eof-object? x)
                (begin (close-input-port p) '())
                (cons (datum->syntax k x) (f (read p))))))))
    (syntax-case x ()
      [(k filename)
       (let ([fn (syntax->datum (syntax filename))])
         (with-syntax ([(e ...) (read-file fn (syntax k))])
           (syntax (begin e ...))))])))

;; (include "defs.scm")


(define-syntax alist
  (syntax-rules ()
    ((test (a b) ...) (list (quote (a b)) ...))))


(define-syntax test2
  (syntax-rules ()
    ((test (a (b ...) ...) ...) (list (quote (a (b ...) ...)) ...))))

(define-syntax test3
  (syntax-rules ()
    ((_ a (b ...) ...) (a (b ...) ...))))
