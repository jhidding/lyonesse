#| Copyright 2016 Johan Hidding
 |
 | Licensed under the Apache License, Version 2.0 (the "License");
 | you may not use this file except in compliance with the License.
 | You may obtain a copy of the License at
 |
 |    http://www.apache.org/licenses/LICENSE-2.0
 |
 | Unless required by applicable law or agreed to in writing, software
 | distributed under the License is distributed on an "AS IS" BASIS,
 | WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 | See the License for the specific language governing permissions and
 | limitations under the License.
 |#

(library (lyonesse testing)
  (export run-test that unit)

  (import (rnrs (6))
          (rnrs records syntactic (6))
          (only (chezscheme) pretty-print pretty-initial-indent)
          (srfi :48))

  (define-record-type test
    (fields description assertions))

  (define-syntax that
    (syntax-rules ()
      [(_ <description> <expr1> ...)
       (make-test <description>
                  (list (lambda ()
                          (format #t "    \x1b;[30;1m•\x1b;[m ")
                          (pretty-initial-indent 6)
                          (pretty-print '<expr1>)
                          <expr1>) ...))]))

  (define (run-test test)
    (format #t "\x1b;[30;1m◇\x1b;[m ~a~%" (test-description test))
    (for-each (lambda (t) (if (t) (format #t "    \x1b;[32m✔\x1b;[m~%") 
                            (format #t "    \x1b;[31m✘\x1b;[m~%")))
                (test-assertions test)))

  (define-syntax unit
    (syntax-rules ()
      [(_ <description> ([<var> <expr>] ...)
          <test> ...)
       (letrec [(<var> <expr>) ...]
         (let ([tests (list <test> ...)])
           (format #t " - \x1b;[34;1m~a\x1b;[m - ~%" <description>)
           (for-each (lambda (f) (run-test f)) tests)))]))
)
