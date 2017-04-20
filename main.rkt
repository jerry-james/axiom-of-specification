#lang racket

(module+ test
  (require rackunit))

(define tm-set (box (set)))
(define stack  (box (list)))
(define env    (box (hash)))

(define (push! e) (set-box! stack (cons e (unbox stack))))

(define (pop!)
  (define e (first (unbox stack)))
  (set-box! stack (rest (unbox stack))) e)

(define (do/1! q) (push! `(,q ,(pop!))))

(define (do/2! q)
  (begin (define b (pop!)) (define a (pop!))
         (push! `(,a ,q ,b))))

(define (for-all!)
  (begin (define b (pop!)) (define a (pop!))
         (push! `(∀ ,a ,b))))

(define (there-exists!)
  (begin (define b (pop!)) (define a (pop!))
         (push! `(∃ ,a ,b))))

(define (swap!)
  (begin (define b (pop!)) (define a (pop!))
         (push! b) (push! a)))

(define (sto!)
  (begin (define b (pop!)) (define a (pop!))
         (set-box! env (hash-set (unbox env) a b))))

(define (rcl!)
  (push! (hash-ref (unbox env) (pop!))))

(define (do/3! q r)
  (begin (define c (pop!))
         (define b (pop!))
         (define a (pop!))
         (push! `(,a ,q ,b ,r ,c))))

(define (history!) (displayln `(history: ,tm-set)))
; russel's paradox: B x A bt x x bt not st y B bt y A bt y y bt not and iff y swap fa ift

(define (env!) (displayln `(env: ,(unbox env))))

(define (bust!)
  (match (pop!)
    [`(,a = ,b : ,c) (push! a) (push! b) (push! c) (push! '=:)]
    [`(,a ,q ,b) (push! a) (push! b) (push! q)]
    [`(∀ ,a ,b) (push! a) (push! b)]
    [else (push! else)]))

(define (roll!)
  (set-box! stack (append (rest (unbox stack)) (list (first (unbox stack))))))

(define (apply! op)
  (case op
    [(r roll)   (roll!)]
    [(bust)     (bust!)]
    [(h hist)   (history!)]
    [(e env)    (env!)]
    [(sto)      (sto!)]
    [(rcl)      (rcl!)]
    [(⊂ sbs)    (do/2! '⊂)]
    [(⊃ sps)    (do/2! '⊃)]
    [(∈ bt)     (do/2! '∈)]
    [(=)        (do/2! '=)]
    [(∧ and)    (do/2! '∧)]
    [(∨ or)     (do/2! '∨)]
    [(¬ not)    (do/1! '¬)]
    [(→ ift)    (do/2! '→)]
    [(↔ iff)    (do/2! '↔)]
    [(∃ te fs)  (there-exists!)];(do/2! '∃)
    [(∀ fa)     (for-all!)]
    [(=: st)    (do/3! '= ':)]
    [(swap)     (swap!)]
    [(dup)      (let ([a (pop!)]) (push! a) (push! a))]
    [(drop)     (when (not (empty? (unbox stack))) (pop!))]
    [(drop-all) (set-box! stack (list))]
    [else (push! op)]))

(define (dispatch tm*)
  (when (not (empty? tm*))
    (let ([tm (first tm*)])
      (apply! tm)
      (dispatch (rest tm*)))))

(define (driver)
  (displayln `stack:)
  (for-each (λ (x) (displayln x)) (reverse (unbox stack)))
  (define tm (read))
  (dispatch (list tm))
  #| history |#
  (when (not (empty? (unbox stack)))
    (set-box! tm-set (set-add (unbox tm-set) (first (unbox stack)))))
  #| loop? |#
  (when (or (empty? (unbox stack))
            (not (and (symbol? (first (unbox stack)))
                      (symbol=? (first (unbox stack)) 'quit)))) (driver)))

(dispatch '(Russels-Paradox
            B x A bt x x bt not st y B bt y A bt y y bt not and iff y swap fa ift sto e))

(driver)