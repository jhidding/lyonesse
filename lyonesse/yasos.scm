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

#| YASOS, Ken Dickey's 'Yet Another Scheme Object System'. The original
 | paper including most of this source is included in
 |
 |   Ken Dickey, "Scheming with Objects", AI Expert 7(10):24-33, October 1992.  
 |
 | which can be found as `doc/swob.txt` in this distribution. The original
 | code is considered to be in the public domain.
 |#

(library (lyonesse yasos)
  (export 
    instance?
    define-predicate
    define-operation
    object
    object-with-ancestors
    operate-as)

  (import (rnrs (6))
          (rnrs records syntactic (6))

          (srfi :48))
  
  (define-record-type instance
    (fields (immutable dispatcher)))

  (define-syntax define-operation
    (syntax-rules () 
      [(_ (<name> <inst> <arg> ...) <exp1> <exp2> ...)
       (define <name>
         (letrec [(self (lambda (<inst> <arg> ...)
                          (cond 
                            [(and (instance? <inst>)
                                  ((instance-dispatcher <inst>) self))
                             => (lambda (operation) (operation <inst> <arg> ...))]
                            [else <exp1> <exp2> ...])))]
           self))]

      [(_ (<name> <inst> <arg> ...)) ; no body
       (define-operation (<name> <inst> <arg> ...)
         (error '<name> (format #f "Operation not handled: ~s" <inst>)))]
    ))

  (define-syntax define-predicate
    (syntax-rules () 
      [(_ <name>) (define-operation (<name> obj) #f)]))

  (define-syntax object
    (syntax-rules ()
      [(_ ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...)
       (let [(table (list 
                      (cons <name>
                        (lambda (<self> <arg> ...) <exp1> <exp2> ...))
                      ...))]
         (make-instance 
           (lambda (operation)
             (cond
               [(assq operation table) => cdr]
               [else #f]))))]))

  (define-syntax object-with-ancestors
    (syntax-rules ()
      [(_ ([<ancestor1> <init1>] ...) <operation> ...)
       (let ([<ancestor1> <init1>] ...)
         (let ([child (object <operation> ...)])
           (make-instance
             (lambda (operation)
               (or ((instance-dispatcher child) operation)
                   ((instance-dispatcher <ancestor1>) operation) 
                   ...)))))]))

  (define-syntax operate-as
    (syntax-rules ()
      [(_ <component> <operation> <composit> <arg> ...) 
       (((instance-dispatcher <component>) <operation>) <composit> <arg> ...)]))
)

