#| Copyright 1992 Ken Dickey, 2016 Johan Hidding
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
        (lyonesse oop)
        (prefix (lyonesse testing) test:)
        (lyonesse stack))

(test:unit "stack"
  ((S (make-stack)))

  (test:that "stack is empty"
    (send S empty?))

  (test:that "we can push"
    (begin (send S push! 3) (send S push! 4) (send S push! 5) #t))

  (test:that "stack is non-empty"
    (not (send S empty?)))

  (test:that "we can see the top"
    (= (send S top) 5))

  (test:that "we can pop"
    (begin (send S pop!) (= (send S top) 4))
    (begin (send S pop!) (= (send S top) 3))
    (begin (send S pop!) #t))

  (test:that "stack is empty"
    (send S empty?)))

