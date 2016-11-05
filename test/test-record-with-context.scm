(import (rnrs (6))
        (prefix (lyonesse testing) test:)
        (lyonesse functional)
        (lyonesse record-with-context))

(define-record-with-context my-vector
  (fields x y))

(define-record-with-context A
  (fields p q))

(define-record-with-context p
  (fields k l m))

(define (my-vector-length v)
  (with-my-vector v (sqrt (+ (* x x) (* y y)))))

(test:unit "record type with context manager"
  ([v (make-my-vector 3 4)])

  (test:that "syntax works"
    (= 5 (my-vector-length v)))

  (test:that "names don't collide"
    (let ([a (make-A (make-p 1 2 3) 4)])
      (= 1 (with-A a (with-p p k)))
      (= 3 (with-A a (with-p p m)))
      (= 4 (with-A a q))))

  (test:that "updating works"
    (let* ([a (make-A (make-p 1 2 3) 4)]
           [b (update-A a (q 8) (p (update-p p (l 42))))])
      (= 8  (with-A a q))
      (= 42 (with-A a (with-p p l)))
      (= 3  (with-A a (with-p p m)))))
)
