#lang racket

(module+ test (require rackunit))

(define (get-initial-state) (cons 1 1))
(module+ test (check-equal? (get-initial-state) (cons 1 1)))

(define (get-end-state rows columns) (cons (- rows 2) (- columns 2)))
(module+ test (check-equal? (get-end-state 10 10) (cons 8 8)))

(define (generate-positions rows columns)
  (define all-positions (for*/list ([i rows][j columns]) (cons i j)))
  (let ([initial-state (get-initial-state)]
        [end-state (get-end-state rows columns)])
  (remove initial-state (remove end-state all-positions))))

(module+ test (test-begin
  (define positions (generate-positions 5 5))
  (check-equal? (length positions) 23)
  (check-false (member (get-initial-state) positions))
  (check-false (member (get-end-state 5 5) positions))))

(define (pick-random list n)
           (for/fold
               ([source list]
                [picked '()])
                ([i n])
           (let* ([r (random (length source))]
                  [value (list-ref source r)])
               (values (remove value source) (cons value picked)))))

(module+ test (test-begin
   (define-values (remaining picked) (pick-random (list 1 2 3) 2))
   (check-equal? (length remaining) 1)
   (check-equal? (length picked) 2)
   (check-not-false (or (member 1 remaining) (member 1 picked)))
   (check-not-false (or (member 2 remaining) (member 2 picked)))
   (check-not-false (or (member 3 remaining) (member 3 picked)))
   ))

(define (generate-room rows columns blocks doors)
  (define-values (free-positions block-positions) (pick-random (generate-positions rows columns) blocks))
  (define-values (final-free-positions door-positions) (pick-random free-positions doors))
  (define empty-list (build-list (* rows columns) (lambda (x) 0)))
  (reverse (for*/fold
                 ([superlist '()]
                  [sublist '()]
                  #:result superlist)
                 ([i rows][j columns])
         (define character
           (cond [(member (cons i j) block-positions) "#"]
                 [(member (cons i j) door-positions) ":"]
                 [(equal? (cons i j) (get-initial-state)) "x"]
                 [(equal? (cons i j) (get-end-state rows columns)) "X"]
                 [else "_"]))                  
         (if (= j (- columns 1))
             (values (cons (reverse (cons character sublist)) superlist) empty)
             (values superlist (cons character sublist))))))
  
