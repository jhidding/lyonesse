(library (lyonesse munsch private)
  (export type type? make-type type-ref type-set! type-bytesize type-repr with-type
          atom-ref atom-set!
          atom-type? make-atom-type
          atom-size gen-size gen-id)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs bytevectors (6))
          (rnrs syntax-case (6))
          (rnrs records syntactic (6))

          (lyonesse record-with-context)
          (lyonesse match))

  #| Atomic data access routines, used in syntactic type building ========== |#
  (define (atom-set! symb)
    (let ((x (assq symb '((f32 . bytevector-ieee-single-native-set!)
                          (f64 . bytevector-ieee-double-native-set!)
                          (u8  . bytevector-u8-set!)
                          (u16 . bytevector-u16-native-set!)
                          (u32 . bytevector-u32-native-set!)
                          (u64 . bytevector-u64-native-set!)
                          (s8  . bytevector-s8-set!)
                          (s16 . bytevector-s16-native-set!)
                          (s32 . bytevector-s32-native-set!)
                          (s64 . bytevector-s64-native-set!)))))
      (and x (cdr x))))

  (define (atom-ref symb)
    (let ((x (assq symb '((f32 . bytevector-ieee-single-native-ref)
                          (f64 . bytevector-ieee-double-native-ref)
                          (u8  . bytevector-u8-ref)
                          (u16 . bytevector-u16-native-ref)
                          (u32 . bytevector-u32-native-ref)
                          (u64 . bytevector-u64-native-ref)
                          (s8  . bytevector-s8-ref)
                          (s16 . bytevector-s16-native-ref)
                          (s32 . bytevector-s32-native-ref)
                          (s64 . bytevector-s64-native-ref)))))
      (and x (cdr x))))

  #| Atomic data access routines, used in `soft' data types ================ |#
  (define (atom-set!-fn symb)
    (let ((x (assq symb `((f32 . ,bytevector-ieee-single-native-set!)
                          (f64 . ,bytevector-ieee-double-native-set!)
                          (u8  . ,bytevector-u8-set!)
                          (u16 . ,bytevector-u16-native-set!)
                          (u32 . ,bytevector-u32-native-set!)
                          (u64 . ,bytevector-u64-native-set!)
                          (s8  . ,bytevector-s8-set!)
                          (s16 . ,bytevector-s16-native-set!)
                          (s32 . ,bytevector-s32-native-set!)
                          (s64 . ,bytevector-s64-native-set!)))))
      (and x (cdr x))))

  (define (atom-ref-fn symb)
    (let ((x (assq symb `((f32 . ,bytevector-ieee-single-native-ref)
                          (f64 . ,bytevector-ieee-double-native-ref)
                          (u8  . ,bytevector-u8-ref)
                          (u16 . ,bytevector-u16-native-ref)
                          (u32 . ,bytevector-u32-native-ref)
                          (u64 . ,bytevector-u64-native-ref)
                          (s8  . ,bytevector-s8-ref)
                          (s16 . ,bytevector-s16-native-ref)
                          (s32 . ,bytevector-s32-native-ref)
                          (s64 . ,bytevector-s64-native-ref)))))
      (and x (cdr x))))

  #| The representation of a type in this struct needs a type
   | representation in symbols, a bytesize, a ref function and
   | a set! function. The ref function takes arguments of
   | bytevector and offset, the set! function takes bytevector
   | offset and value.
   |#
  (define-record-with-context type
    (fields repr bytesize ref set!))

  (define-record-type atom-type
    (parent type)
    (protocol
      (lambda (new)
        (lambda (symb)
          ((new symb (atom-size symb)
                (atom-ref-fn symb) (atom-set!-fn symb)))))))

  #| Compute sizes of syntactic data structures ============================ |#
  (define (atom-size symb)
    (let ((x (assq symb '(( u8 . 1) ( s8 . 1)
                          (u16 . 2) (s16 . 2)
                          (u32 . 4) (s32 . 4)
                          (u64 . 8) (s64 . 8)
                          (f32 . 4) (f64 . 8)))))
      (and x (cdr x))))

  (define (gen-size type)
    (match type
      ((array ,type ,len)   (* len (gen-size type)))
      ((struct ,types ...)  (apply + (map gen-size types)))
      (,type                (atom-size type))))

  #| Generate identifier from syntactic components ========================= |#
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
