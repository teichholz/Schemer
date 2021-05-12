(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define-syntax-rule (hey) "hey")

(define e (syntax-e #'(+ 1 2))) ;; unwraps single layer
(define e2 (syntax-e #'(+ 1 2 (+ 1 3)))) ;; unwraps single layer

(define p  (syntax-rules () [(nothing) something])) ;; => <procedure>

;; syntax-object -> syntax-object
(define-syntax self-as-string
    (lambda (stx)
      (datum->syntax stx
                     (format "~s" (syntax->datum stx)))))

;; geht auch (desugar in obere form)
(define-syntax (self-as-string stx)
    (datum->syntax stx
                   (format "~s" (syntax->datum stx))))

;; syntax-rules benutzt intern exand-e zum deconstructen des inputs
;; und datum->syntax bauen des outputs

(define test (syntax->datum
              (syntax-case #'(+ 1 2) ()
                [(op n1 n2) #'(- n1 n2)])))

(define-syntax (swap' stx)
  (syntax-case stx ()
    [(swap x y) #'(let ([tmp x])
                    (set! x y)
                    (set! y tmp))]))

;; fender könnte in sowas desugarn
(define-syntax (swap'' stx)
  (syntax-case stx ()
    [(swap x y)
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             (if (identifier? #'x)
                                 #'y
                                 #'x)))]))

(define s->l (syntax->list #'(+ 1 2)))
