(library (lyonesse munsch geometry)

  (export g:. g:> g:p-x g:p-y g:p-z g:v-x g:v-y g:v-z
          g:+ g:- g:dot g:cross g:print g:data g:origin
          g:normalize

          g:point->f32array g:f32array->point
          g:vector->f32array g:f32array->vector)

  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs control (6))
          (rnrs records syntactic)
          (rnrs bytevectors (6))

          (srfi :48)

          (lyonesse functional)
          (lyonesse ranges)
          (lyonesse munsch f32array)
          (lyonesse munsch slice)
          (lyonesse munsch linear-algebra))

  (define-record-type (g:point g:make-point g:point?)
    (fields data))

  (define (g:make-vector data)
    (make-l:vector 3 data))

  (define g:vector-data l:vector-data)

  (define (g:vector? v)
    (and (l:vector? v)
         (= 3 (l:vector-length v))))

  (define (g:data p)
    (cond
      [(g:point? p) (g:point-data p)]
      [(g:vector? p) (g:vector-data p)]))

  (define (g:point->f32array p)
    (make-f32array (make-slice '(3)) (g:point-data p)))

  (define (g:f32array->point a)
    (with-f32array a
      (unless (= 1 (slice-dimension slice))
        (error 'g:f32array->point "Array should be one-dimensional." a))
      (with-slice slice
        (g:make-point (f32array-copy-data a)))))

  (define g:f32array->vector f32array->l:vector)
  (define g:vector->f32array l:vector->f32array)

  (define (g:. x y z)
    (g:make-point (f32v x y z)))

  (define (g:> x y z)
    (g:make-vector (f32v x y z)))

  (define (g:p-x p) (f32vector-ref (g:point-data p) 0))
  (define (g:p-y p) (f32vector-ref (g:point-data p) 1))
  (define (g:p-z p) (f32vector-ref (g:point-data p) 2))

  (define (g:v-x p) (f32vector-ref (g:vector-data p) 0))
  (define (g:v-y p) (f32vector-ref (g:vector-data p) 1))
  (define (g:v-z p) (f32vector-ref (g:vector-data p) 2))

  (define (g:print v)
    (cond
      [(g:vector? v) (format #t "[> ~a ~a ~a]"
                             (g:v-x v) (g:v-y v) (g:v-z v))]
      [(g:point? v) (format #t "[. ~a ~a ~a]"
                            (g:p-x v) (g:p-y v) (g:p-z v))]))

  (define (g:* a b)
    (cond
      [(and (number? a) (g:vector? b))
       (g:make-vector (f32vector-map ($ * <> a) (g:vector-data b)))]
      [(and (number? b) (g:vector? a))
       (g:make-vector (f32vector-map ($ * <> b) (g:vector-data a)))]
      [else (error 'g:* "Type error: need a vector and a scalar." a b)]))

  (define g:origin
    (g:. 0 0 0))

  (define g:-
    (case-lambda
      [(a)   (cond
               [(g:vector? a)
                (g:make-vector (f32vector-map - (g:vector-data a)))]
               [else (error 'g:- "Type error: need a vector." a)])]

      [(a b) (cond
               [(and (g:point? a) (g:point? b))
                (g:make-vector (f32vector-map - (g:point-data a)
                                                (g:point-data b)))]

               [(and (g:point? a) (g:vector? b))
                (g:make-point (f32vector-map - (g:point-data a)
                                               (g:vector-data b)))]

               [(and (g:vector? a) (g:vector? b))
                (g:make-vector (f32vector-map - (g:vector-data a)
                                                (g:vector-data b)))]

               [else (error 'g:- "Type error: need point and point 
                                  or vector and vector." a b)])]))

  (define (g:+ a b)
    (cond
      [(and (g:point? a) (g:vector? b))
       (g:make-point (f32vector-map + (g:point-data a)
                                      (g:vector-data b)))]

      [(and (g:vector? a) (g:vector? b))
       (g:make-vector (f32vector-map + (g:vector-data a)
                                       (g:vector-data b)))]
      
      [else (error 'g:+ "Type error: need point and vector 
                   or vector and vector." a b)]))

  (define (g:dot a b)
    (if (and (g:vector? a) (g:vector? b))
      (f32vector-reduce + 0 (f32vector-map * (g:vector-data a)
                                             (g:vector-data b)))
      (error 'g:dot "Type error: Need two vectors." a b)))

  (define (g:cross a b)
    (if (and (g:vector? a) (g:vector? b))
      (g:> (- (* (g:v-y a) (g:v-z b)) (* (g:v-z a) (g:v-y b)))
           (- (* (g:v-z a) (g:v-x b)) (* (g:v-x a) (g:v-z b)))
           (- (* (g:v-x a) (g:v-y b)) (* (g:v-y a) (g:v-x b))))
      (error 'g:cross "Type error: Need two vectors." a b)))

  (define (g:normalize v)
    (g:* (/ 1 (sqrt (g:dot v v))) v))
)
