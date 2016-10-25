(library (lyonesse munsch ivector)
  (export make-ivector ivector? ivector-set! ivector-ref m:iv)
  (import (rnrs base (6))
          (rnrs bytevectors (6)))

  (define bv-i32-ref  bytevector-i32-native-ref)
  (define bv-i32-set! bytevector-i32-native-set!)
 
  #| Vector essentials ===================================================== |#
  (define-record-type ivector
    (fields length data)
    (protocol
      (lambda (new)
        (case-lambda
          [(l) (new l (make-bytevector (* l 4)))]
          [(l data) (new l data)]))))

  (define (ivector-ref v n)
    (bv-i32-ref (ivector-data v) (* n 4)))

  (define (ivector-set! v n x)
    (bv-i32-set! (ivector-data v) (* n 4) x))

  #| Vector construction =================================================== |#
  (define-syntax m:iv
    (lambda (x)
      (syntax-case x ()
        [(_ <xs> ...)
         #`(let* ([v (make-ivector #,(length #'(<xs> ...)))]
                  [d (ivector-data v)])
             #,@(map (lambda (x n)
                       #`(bv-i32-set! d #,(* 4 n) #,x))
                  #'(<xs> ...) 
                  (iota (length #'(<xs> ...))))
             v)])))
)
