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

(import (rnrs (6))
        (yasos)
        (prefix (testing) test:)
        (srfi :48))

(define-predicate lvector?)
(define-operation (get-x obj))
(define-operation (get-y obj))
(define-operation (print obj port))
(define-operation (blah obj))

(define (make-lvector x y)
  (object
    ((lvector? self) #t)
    ((get-x self) x)
    ((get-y self) y)
    ((print self port)
       (format port "#<lvector>(~a ~a)" x y))))

(test:unit "lvector object" 
  [(a (make-lvector 3 4))]
  (test:that "lvector type is working" 
    (= 3 (get-x a))
    (= 4 (get-y a))))

