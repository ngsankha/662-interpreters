#lang racket

(require rackunit "interp.rkt" "parser.rkt" "ast.rkt")

(module+ test
  (check-equal? (interp-err (parse-prog '((+ 42 (sub1 34))))) 75)
  (check-equal? (interp-err (parse-prog '((zero? (- 5 (sub1 6)))))) #t)
  (check-equal? (interp-err (parse-prog '((if (zero? 0) (add1 5) (sub1 5))))) 6)
  (check-equal? (interp-err (parse-prog '((add1 (+ 3 #f)))))
                (Err "+ requires int"))
  (check-equal? (interp-err (parse-prog '((add1 (and #t #t)))))
                (Err "add1 expects int"))
  (check-equal? (interp-err (parse-prog '((/ 5 (sub1 1)))))
                (Err "division by 0 not allowed"))
  (check-equal? (interp-err (parse-prog '((let ((x 1)) (+ x 3))))) 4)
  (check-equal? (interp-err (parse-prog '((let ((x 1))
                                      (let ((y 2))
                                        (+ x y)))))) 3)
  (check-equal? (interp-err (parse-prog '((let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x 2)))))) 6)
  (check-equal? (interp-err (parse-prog '((let ((x (add1 6)))
                                      (let ((x (+ 6 x)))
                                        (/ x y))))))
                (Err "Unbound identifier: y"))

  (check-equal? (interp-err (parse-prog '((define (abs x)
                                            (if (<= x 0) (* -1 x) x))

                                          (abs -42)))) 42)

  (check-equal? (interp-err (parse-prog '((define (true? x)
                                            (and x y))

                                          (let ((y #t))
                                            (true? #t)))))
                (Err "Unbound identifier: y"))

  (check-equal? (interp-err (parse-prog '((define (odd? x)
                                            (if (zero? x) #f
                                                (even? (sub1 x))))

                                          (define (even? x)
                                            (if (zero? x) #t
                                                (odd? (sub1 x))))

                                          (odd? 45)))) #t)

  (check-equal? (interp-err (parse-prog '((let ((foo (λ (x) (+ x 42))))
                                            (foo 3))))) 45)

  (check-equal? (interp-err (parse-prog '((define (foo x)
                                            (- x x))

                                          (let ((foo (λ (x) (+ x 42))))
                                            (foo 3))))) 45)

  (check-equal? (interp-err (parse-prog '((define (bar x)
                                            (- x x))

                                          (let ((foo (λ (x) (+ x 42))))
                                            (bar 3))))) 0)

  (check-equal? (interp-err (parse-prog '((define foo 42)

                                          (+ foo 3)))) 45)

  (check-equal? (interp-err (parse-prog '(((lambda (x) (add1 x)) 4)))) 5)

  (check-equal? (interp-err (parse-prog '((let ([adder (λ (x) (λ (y) (+ x y)))])
                                            ((adder 3) 4))))) 7)

  (check-equal? (interp-err (parse-prog '((let ([adder (λ (x) (λ (y) (+ x y)))])
                                            (let ([adder2 (adder 2)])
                                              (adder2 4)))))) 6)

  (check-equal? (interp-err (parse-prog '((foo 4))))
                (Err "Unbound identifier: foo")))
