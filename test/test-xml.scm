(import (rnrs (6))
        (prefix (lyonesse testing) test:)
        (lyonesse functional)
        (lyonesse parsing xml))

(test:unit "some basic XML"
  ([parse (lambda (s) (xml:sexpr-from-port (open-string-input-port s)))])
   
  (test:that "header is read"
    (equal? (parse "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
                    <body>Hello World!</body>")
            '((?xml (~ (version "1.0") (encoding "UTF-8")) ?)
              (body "Hello World!" /body))))

  (test:that "attributes parse"
    (equal? (parse "<a x=\"1\" y=\"2\"/>")
            '((a (~ (x "1") (y "2"))))))

  (test:that "hierarchy is preserved"
    (equal? (parse "<a x=\"goodbye\"><b/>hello<c/></a>")
            '((a (~ (x "goodbye")) (b) "hello" (c) /a))))

  (test:that "comments are ignored"
    (equal? (parse "<a><!--ignore me--></a>")
            '((a /a))))
)

