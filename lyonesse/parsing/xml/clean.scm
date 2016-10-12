(library (lyonesse parsing xml clean)
  (export xml:clean)
  (import (rnrs base (6))
          (only (srfi :13) string-trim-both)
          (lyonesse match)
          (lyonesse functional))

  (define (xml:tags-match? a b)
    (let* ([a0 (symbol->string a)]
           [b0 (symbol->string b)]
           [b1 (substring b0 1 (string-length b0))])
      (and (char=? (string-ref b0 0) #\/)
           (equal? a0 b1))))

  (define (xml:clean ast)
    (match ast
      [()                      '()]
      [(,a ,[content] ... ,b)  (guard (symbol? a) (symbol? b)
                                      (xml:tags-match? a b))
                               (cons a content)]
      [(? ,a ,[content] ?)     (guard (symbol? a))
                               (list '? a content)]
      [(~ (,attrs ,valus) ...) (guard (and (all? symbol? attrs)
                                           (all? string? valus)))
                               (map cons attrs valus)]
      [(,a ,[content] ...)     (guard (symbol? a))
                               (cons a content)]
      [,a                      (guard (string? a)) (string-trim-both a)]
      [,a                      (error 'xml:clean "XML AST is unclean." a)]))
)
