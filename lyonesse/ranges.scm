(library (lyonesse ranges)
  (export range for-range map-range reduce-range)
  (import (rnrs base (6))
          (rnrs control (6)))

  (define range
    (case-lambda
      [(b)      (range 0 b 1)]
      [(a b)    (range a b 1)]
      [(a b dx) (let loop ([lst '()]
                           [x a])
                  (if (>= x b)
                    (reverse lst)
                    (loop (cons x lst) (+ x dx))))]))

  (define for-range
    (case-lambda
      [(f b)      (for-range f 0 b 1)]
      [(f a b)    (for-range f a b 1)]
      [(f a b dx) (unless (>= a b)
                    (f a)
                    (for-range f (+ a dx) b dx))]))

  (define map-range
    (case-lambda
      [(f b)      (map-range f 0 b 1)]
      [(f a b)    (map-range f a b 1)]
      [(f a b dx) (let loop ([x a]
                             [result '()])
                    (if (>= x b)
                      (reverse result)
                      (loop (+ x dx) (cons (f x) result))))]))

  (define reduce-range
    (case-lambda
      [(f start b)      (reduce-range f start 0 b 1)]
      [(f start a b)    (reduce-range f start a b 1)]
      [(f start a b dx) (if (>= a b)
                          start
                          (reduce-range f (f start a) (+ a dx) b dx))]))
)
