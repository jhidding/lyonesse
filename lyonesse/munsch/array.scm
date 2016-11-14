(library (lyonesse munsch array)
  (export array-type? make-array-type array-type-element-type array-type-length array-set!
          array? make-array array-element-type array-data array-offset array-length array-ref
          with-array bytearray array->list)

  (import (rnrs (6))
          (lyonesse ranges)
          (lyonesse functional)
          (lyonesse record-with-context)
          (lyonesse munsch private)

          (only (chezscheme) trace-define-syntax))

  (define-record-type array-type
    (parent type)
    (fields element-type length)
    (protocol
      (lambda (new)
        (lambda (element-type length)
          ((new ; type description
                `(array ,(type-repr element-type))
                ; bytesize
                (* length (type-bytesize element-type))
                ; getter
                (lambda (bv x)
                  (make-array element-type bv x length))
                ; setter
                (lambda (bv x value)
                  (for-range (lambda (i)
                      ((type-set! element-type)
                       bv (+ x (* (type-bytesize element-type) i))
                       ((cond
                          ((vector? value) vector-ref)
                          ((array? value)  array-ref)
                          ((list? value)   list-ref))
                        value i)))
                    length)))
           element-type length)))))

  (define-record-with-context array
    (fields element-type data offset length))

  (define (array-ref a x)
    (with-array a
      (with-type element-type
        (ref data (+ offset (* x bytesize))))))

  (define (array-set! a x v)
    (with-array a
      (with-type element-type
        (set! data (+ offset (* x bytesize)) v))))

  (define (array->list a)
    (with-array a
      (with-type element-type
        (map-range
          (lambda (i) (ref data (+ offset (* i bytesize))))
          length))))

  (define-syntax bytearray
    (lambda (x)
      (syntax-case x ()
        [(bytearray <type> <xs> ...)
         (let* ((type-descr (syntax->datum #'<type>))
                (type       (make-atom-type type-descr))
                (set!       (datum->syntax #'bytearray (atom-set! type-descr)))
                (bytesize   (datum->syntax #'bytearray (atom-size type-descr))))

           (with-syntax ([<length> (length #'(<xs> ...))]
                         [<bytesize> bytesize])
             #`(let* ([d (make-bytevector (* <bytesize> <length>))])
                 #,@(map (lambda (x n)
                           #`(#,set! d #,(* (type-bytesize type) n) #,x))
                       #'(<xs> ...)
                       (iota #'<length>))
               (make-array (make-atom-type '<type>) d 0 <length>))))])))
)
