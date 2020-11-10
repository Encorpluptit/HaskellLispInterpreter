(define (posFact x)
  (cond ((eq? x 1) 1)
        (#t (* x (posFact (- x 1))))))

(define (negFact x)
  (cond ((eq? x -1) 1)
        (#t (* x (posFact (- (-x) 1))))))

(define (fact x)
  (cond ((eq? x 0) 1)
        ((< x 0) (negFact x))
        (#t (posFact x))))
