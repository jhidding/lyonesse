(library (lyonesse munsch private)
  (export atom-size atom-ref atom-set!
          array-size array-ref array-set!
          struct-size struct-ref struct-set!
          gen-size gen-ref gen-set! gen-offsets gen-id)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs bytevectors (6))
          (rnrs syntax-case (6))
          (lyonesse match))

  (define (atom-size symb)
    (let ((x (assq symb '((single . 4) (double . 8)
                          (u8 . 1) (s8 . 1)
                          (u16 . 2) (s16 . 2)
                          (u32 . 4) (s32 . 4)
                          (u64 . 8) (s64 . 8)))))
      (and x (cdr x))))

  (define (atom-set! symb)
    (let ((x (assq symb '((single . bytevector-ieee-single-native-set!)
                          (double . bytevector-ieee-double-native-set!)
                          (u8     . bytevector-u8-native-set!)
                          (u16    . bytevector-u16-native-set!)
                          (u32    . bytevector-u32-native-set!)
                          (u64    . bytevector-u64-native-set!)
                          (s8     . bytevector-s8-native-set!)
                          (s16    . bytevector-s16-native-set!)
                          (s32    . bytevector-s32-native-set!)
                          (s64    . bytevector-s64-native-set!)))))
      (and x (cdr x))))

  (define (atom-ref symb)
    (let ((x (assq symb '((single . bytevector-ieee-single-native-ref)
                          (double . bytevector-ieee-double-native-ref)
                          (u8     . bytevector-u8-native-ref)
                          (u16    . bytevector-u16-native-ref)
                          (u32    . bytevector-u32-native-ref)
                          (u64    . bytevector-u64-native-ref)
                          (s8     . bytevector-s8-native-ref)
                          (s16    . bytevector-s16-native-ref)
                          (s32    . bytevector-s32-native-ref)
                          (s64    . bytevector-s64-native-ref)))))
      (and x (cdr x))))

  (define (array-size type len)
    (* (gen-size type) len))

  (define (array-set! type len)
    (let ((size (array-size type len)))
      (lambda (bv offset value)
        (bytevector-copy! value 0 bv offset size))))

  (define (array-ref type len)
    (let ((size (array-size type len)))
      (lambda (bv offset)
        (let ((tgt (make-bytevector size)))
          (bytevector-copy! bv offset tgt 0 size)
          tgt))))

  (define (struct-size types)
    (apply + (map gen-size types)))

  (define (struct-set! types)
    (let ((size (struct-size types)))
      (lambda (bv offset value)
        (bytevector-copy! value 0 bv offset size))))

  (define (struct-ref types)
    (let ((size (struct-size types)))
      (lambda (bv offset)
        (let ((tgt (make-bytevector size)))
          (bytevector-copy! bv offset tgt 0 size)
          tgt))))

  (define (gen-size type)
    (match type
      ((struct ,types ...)  (struct-size types))
      ((array ,type ,len)   (array-size type len))
      (,type                (atom-size type))))

  (define (gen-ref type)
    (match type
      ((struct ,types ...)  (struct-ref types))
      ((array  ,type ,len)  (array-ref type len))
      (,type                (atom-ref type))))

  (define (gen-set! type)
    (match type
      ((struct ,types ...)  (struct-set! types))
      ((array  ,type ,len)  (array-set! type len))
      (,type                (atom-set! type))))

  (define (gen-offsets types)
    (reverse (cdr
      (fold-right
        (lambda (x lst) (cons (+ (gen-size x) (car lst)) lst))
        '(0) types))))

  (define gen-id
    (lambda (template-id . args)
      (datum->syntax template-id
        (string->symbol
          (apply string-append
            (map (lambda (x)
                   (if (string? x)
                       x
                       (symbol->string (syntax->datum x))))
                 args))))))
)
