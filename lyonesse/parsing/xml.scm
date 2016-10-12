(library (lyonesse parsing xml)
  (export xml:read xml:from-string 
          xml:get xml:get-first xml:get-content xml:get* xml:list xml:list*
          xml:get-attr)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs io ports (6))
          (lyonesse match)
          (lyonesse functional)
          (lyonesse parsing xml parser)
          (lyonesse parsing xml clean))

  (define (xml:read filename)
    (map xml:clean
      (xml:ast-from-port
        (open-file-input-port filename 
                              (file-options) 
                              'line 
                              (native-transcoder)))))

  (define (xml:from-string s)
    (map xml:clean (xml:ast-from-port
                     (open-string-input-port s))))

  (define (safe-car obj) (and (pair? obj) (car obj)))
  (define (safe-cdr obj) (and (pair? obj) (cdr obj)))

  (define (alist? lst)
    (and (list? lst)
         (all? (lambda (i) (pair? i)) lst)))

  (define (xml:get data . args)
    (match args
      [(,tag ,attributes)     (guard (and (symbol? tag)
                                          (alist? attributes)))
                              (xml:*get* data tag attributes)]
      [(,tag ,attributes ...) (guard (and (symbol? tag)
                                          (alist? attributes)))
                              (xml:*get* data tag attributes)]
      [,x                     (error 'xml:get "Arguments do not match pattern." args)]))

  (define (xml:*get* data tag attributes)
    (filter (lambda (item)
              (and (pair? item)
                   (eq?  (car item) tag)
                   (all? (lambda (attr)
                           (let ([value (safe-cdr (assq (car attr) (cadr item)))])
                             (if (procedure? (cdr attr))
                               ((cdr attr) value)
                               (equal? (cdr attr) value))))
                         attributes)))
            data))

  (define xml:get-content
    (compose append <- ($ map cddr <>) xml:get))

  (define xml:get-entries
    (compose ($ map (juxt car cadr) <>) xml:get))

  (define (xml:get* data . query)
    (if (null? query)
      data
      (apply xml:get* 
             (apply xml:get-content 
                    data (car query))
             (cdr query))))

  (define xml:get-first
    (compose safe-car <- ($ map cddr <>) xml:get))

  (define (xml:get-attr datum attr)
    (safe-cdr (assq attr (cadr datum))))

  (define (xml:list data . query)
    (cond
      [(null? query) '()]
      [(null? (cdr query))
       (apply xml:get-entries data (car query))]
      [else 
       (apply xml:list (apply xml:get-content data (car query)) (cdr query))]))

  (define (xml:list* data . query)
    (cond
      [(null? query) data]
      [(null? (cdr query))
       (apply xml:get data (car query))]
      [else
       (apply xml:list* (apply xml:get-content data (car query)) (cdr query))]))
)
