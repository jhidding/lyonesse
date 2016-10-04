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
        (only (srfi :1) unfold)
        (prefix (lyonesse testing) test:)
        (lyonesse aux-keyword)
        (lyonesse functional))

(define (range a b)
  (unfold ($ >= <> b) id ($ + <> 1) a))

(define (polynomial . c)
  (lambda (x)
    (let loop ([y 0]
               [x^n 1]
               [c* c])
      (if (null? c*)
        y
        (loop (+ y (* (car c*) x^n)) 
              (* x^n x) 
              (cdr c*))))))
               
(test:unit "higher order functions"
  ([f (polynomial 0 10 -1)])

  (test:that "identity identifies"
    (= 42 (id 42))
    (= 2.71828 (id 2.71828))
    (equal? "The Love for Three Oranges" 
      (id "The Love for Three Oranges"))
    (eq? car car)
    (not (eq? cons fold-left)))

  (test:that "thunking"
    (= 42 ((thunk (* 6 7))))
    (= 0 ((thunk 0))))

  (test:that "polynomial works"
    (= 16 (f 2))
    (= 25 (f 5)))

  (test:that "range works"
    (equal? '(0 1 2 3 4) (range 0 5)))

  (test:that "compose"
    (= 25 ((compose max <- ($ map f <>)) (range 0 10))))

  (test:that "splice"
    (equal? '(4 -3 9) 
            ((compose list (splice ($ * <> 4) ($ - <> 5) ($ + <> 6))) 
             1 2 3)))

  (test:that "pipe"
    (= 25 (pipe (range 0 10) ($ map f <>) <- max))
    (= 7  (pipe (div-and-mod 19 5) +)))
)

