(library (lyonesse foreign-structs)
  (export define-foreign-struct)
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (only (chezscheme) define-ftype ftype-ref trace-define-syntax))

  (define-syntax define-foreign-struct
    (lambda (x)
      ; Define a new symbol, code from TSPL Chapter 8.
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

      (syntax-case x ()
        [(define-foreign-struct <name> (<f> <type>) ...)
         (with-syntax ([<with>         (gen-id #'<name> "with-" #'<name>)]
                       ; Define the names of member access functions.
                       [(<access> ...) (map (lambda (x)
                                              (gen-id x #'<name> "-" x))
                                            #'(<f> ...))])
           #'(begin
               (define-ftype <name>
                 (struct (<f> <type>) ...))

               (define-syntax <with>
                 (lambda (x)
                   (syntax-case x ()
                     [(<with> <r> <expr> (... ...))
                      (with-syntax ([<f> (datum->syntax #'<with> '<f>)]
                                    ...)
                        #'(let ([<f> (ftype-ref <name> (<f>) <r>)] ...)
                            <expr> (... ...)))])))))])))
)

