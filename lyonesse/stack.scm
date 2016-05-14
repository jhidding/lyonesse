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

(library (stack)
  (export make-stack)

  (import (rnrs (6))
          (oop))

  (define-object (make-stack)
    ([lst '()])
    ([empty? (lambda () (null? lst))]
     [push!  (lambda (value) (set! lst (cons value lst)))]
     [top    (lambda () (car lst))]
     [pop!   (lambda () (set! lst (cdr lst)))]))
)

