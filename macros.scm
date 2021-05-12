(define-syntax when
  (syntax-rules ()
    ((_ predicate . commands) (if predicate (begin . commands)))))

;; (or) -> #f
;; (or 2) -> (if 2 2)
;; (or 2 2) -> (if 2 2 (or 2))
(define-syntax or
  (syntax-rules ()
    ((_ a a2 ...) (if a a (or a2 ...)))
    ((_ a) a)
    ((_) #f)))

;; (and) -> #t
;; (and 1) -> 1
;; (and 1 2) -> (if 1 (and 2) #f)
(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ a) a)
    ((_ a a2 ...) (if a (and a2 ...) #f))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...)
     (letrec "generate_temp_names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((letrec "generate_temp_names"
       ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     (let ((var1 (if #f #f)) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((letrec "generate_temp_names"
       (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (letrec "generate_temp_names"
       (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))
