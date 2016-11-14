(library (lyonesse munsch struct)

  (export define-struct)

  (import (rnrs (6))
          (lyonesse munsch private)
          (lyonesse match)
          (lyonesse munsch array)
          (lyonesse record-with-context)

          (only (chezscheme) trace-define-syntax))

  (define-record-with-context struct
    (fields type-record data offset))

  (define-record-type struct-type
    (parent type)
    (fields type-list)
    (protocol
      (lambda (new)
        (lambda (type-list)
          ((new ; type description
                `(struct ,@(map type-repr type-list))
                ; bytesize
                (apply + (map type-bytesize type-list))
                ; getter
                (lambda (bv x)
                  (make-struct type-list bv x))
                ; setter
                (lambda (bv x value-list)
                  (let loop ((type-list  type-list)
                             (value-list value-list)
                             (offset     x))
                    (unless (null? type-list)
                      (with-type (car type-list)
                        (set! bv offset (car value-list))
                        (loop (cdr type-list) (cdr value-list) (+ offset bytesize)))))))
           type-list)))))

  (define (gen-type type)
    (match type
      ((array  ,type ,len) (make-array-type (gen-type type) len))
      ((struct ,types ...) (make-struct-type (map gen-type types)))
      (,type               (make-atom-type type))))

  (trace-define-syntax define-struct
    (lambda (x)
      (define (cumsum lst)
        (reverse
          (cdr
            (fold-left
              (lambda (lst x) (cons (+ x (car lst)) lst))
              '(0) lst))))

      (syntax-case x ()
        ((define-struct <name> (<field> <type>) ...)

         (let* (;(type-list   (map gen-type (syntax->datum #'(<type> ...))))
                ;(size-lst    (map type-bytesize type-list))
                (size-lst    (map gen-size (syntax->datum #'(<type> ...))))
                (bytesize    (apply + size-lst))
                (<bytesize>  (datum->syntax #'define-struct (apply + size-lst)))
                (offsets     (datum->syntax #'define-struct (cumsum size-lst))))

           (with-syntax ([<with>     (gen-id #'<name> "with-" #'<name>)]
                         [<make>     (gen-id #'<name> "make-" #'<name>)]
                         [<pred>     (gen-id #'<name> #'<name> "?")]
                         [<stype>    (gen-id #'<name> #'<name> "-type")]

                         [(<offset> ...) offsets]

                         ; Each field has an offset within the struct
                         [(<type-name> ...) (map (lambda (x)
                                                   (gen-id x #'<name> "-" x "-type"))
                                                 #'(<field> ...))]

                         ; Define the names of member access functions.
                         [(<access> ...)  (map (lambda (x)
                                                 (gen-id x #'<name> "-" x))
                                               #'(<field> ...))]
                         ; Define names of the setter functions.
                         [(<mutate> ...)  (map (lambda (x)
                                                 (gen-id x #'<name> "-" x "-set!"))
                                               #'(<field> ...))])
             #`(begin
                 (define <type-name>
                   (gen-type '<type>)) ...

                 (define <stype>
                   (make-struct-type (map gen-type '(<type> ...))))

                 (define <make>
                   (case-lambda
                     (()    (<make> 1))
                     ((n)   (make-struct <stype> (make-bytevector (* n #,bytesize)) 0))
                     ((d o) (make-struct <stype> d o))))

                 (define (<pred> o)
                   (and (struct? o) (eq? <stype> (struct-type-record o))))

                 (define (<access> s)
                   (with-type <type-name>
                     (with-struct s
                       (ref data (+ offset <offset>))))) ...

                 (define (<mutate> s v)
                   (with-type <type-name>
                     (with-struct s
                       (set! data (+ offset <offset>) v)))) ...

                 (define-syntax <with>
                   (lambda (x)
                     (syntax-case x ()
                       [(<with> <r> <expr> (... ...))
                        (with-syntax ([<field> (datum->syntax #'<with> '<field>)]
                                      ...)
                          #'(let ([<field> (<access> <r>)] ...)
                              <expr> (... ...)))])))
               ))
          )))))
)
