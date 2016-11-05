(library (lyonesse munsch quaternions)
  
  (export g:make-quat g:quat? g:quat-vector g:quat-scalar
          rotation-quat scalar-quat quat-mul
          quat-conj quat-sqr quat-scale quat-inv
          quat-conjugation)

  (import (rnrs (6))
          (lyonesse functional)
          (lyonesse munsch geometry))

  ;;;======================================================
  ;;; quaternion
  ;;;------------------------------------------------------
  (define-record-type (g:quat g:make-quat g:quat?)
    (fields scalar vector))
   
  (define (g:rotation-quat u theta)
    (g:make-quat (cos (/ theta 2))
                 (g:* (sin (/ theta 2)) u)))
  
  (define (g:scalar-quat s)
    (g:make-quat s (g:> 0 0 0)))
  
  ;;;======================================================
  ;;; multiplication
  ;;;------------------------------------------------------
  (define (g:quat-mul2 q2 q1)
    (g:make-quat 
      (-     (*       (g:quat-scalar q1) (g:quat-scalar q2))
             (g:dot   (g:quat-vector q1) (g:quat-vector q2)))

      (g:+   (g:*     (g:quat-scalar q1) (g:quat-vector q2))
             (g:*     (g:quat-scalar q2) (g:quat-vector q1))
             (g:cross (g:quat-vector q1) (g:quat-vector q2)))))
  
  (define (g:quat:* q . qs)
    (fold-right quat-mul2 q qs))
  
  ;;;======================================================
  ;;; other operations
  ;;;------------------------------------------------------
  (define (g:quat:conj q)
    (g:make-quat (g:quat-scalar q) (g:- (g:quat-vector q))))
  
  (define (g:quat:sqr q)
    (+ (*     (g:quat-scalar q) (g:quat-scalar q))
       (g:dot (g:quat-vector q) (g:quat-vector q))))
  
  (define (g:quat:scale s q)
    (g:make-quat (*   s (g:quat-scalar q))
		         (g:* s (s:quat-vector q))))
  
  (define (g:quat:inv q)
    (g:quat:scale (/ 1 (g:quat:sqr q))
                  (g:quat:conj q)))
  
  ;;;======================================================
  ;;; vector conjugation
  ;;;------------------------------------------------------
  (define (g:quat:vector-conjugate q v)
    (g:quat-vector
      (g:quat:* q (g:make-quat 0.0 v) (g:quat:conj q))))
)

