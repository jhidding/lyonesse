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

(library (lyonesse functional)
  (export $ <> <...> id thunk <- args compose splice juxt
          flip reverse-args
          partial partial* on pipe
          any? all? unique-sorted unfold
          
          inc dec iota receive)

  (import (rnrs (6))
          (only (srfi :1 lists) append-reverse unfold)
          (rename (lyonesse cut) (cut $)))


  (define (inc x) (+ x 1))
  (define (dec x) (- x 1))
  (define (iota n) (unfold ($ = <> n) id inc 0))

  #| identity function
   |   @(param x) any value
   |   @(returns) @(ref x)
   |#
  (define (id x) x)

  #| creates a thunk
   |   @(param x) any value
   |   @(returns) a function returning @(ref x)
   |#
  (define (thunk x) (lambda _ x))

  #| forward result as values
   |   @(param x) a list
   |   @(returns) values in @(ref x)
   |#
  (define (<- x) (apply values x))

  #| make list from values
   |   @(param . X) variadic list
   |   @(returns) @(ref X)
   |#
  (define (args . X) X)

  #| compose functions
   |   @(param f . rest) variadic list of functions
   |   @(returns) functional composite of arguments
   |#
  (define (compose f . rest)
    (if (null? rest)
      f
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values ($ apply g args) f)))))

  #| splice 
   |   @(param . F) variadic list of functions
   |   @(returns) new function returning values for each
   |     of each n-th argument applied to the n-th function in F
   |
   |   ≪── f₁ ── x₁ ─╮
   |   ≪── f₂ ── x₂ ─┼── X
   |   ≪── f₃ ── x₃ ─╯
   |#
  (define (splice . F)
    (compose <- ($ map (lambda (f . X) (apply f X)) F <...>) args))

  #| juxtapose outcomes
   |   @(param . F) variadic list of functions
   |   @(returns) function returning list with result for each 
   |     function applied to its arguments
   |
   |   ≪── f₁ ─╮
   |   ≪── f₂ ─┼── X
   |   ≪── f₃ ─╯
   |#
  (define (juxt . F)
    (lambda X
      (map ($ apply <> X) F)))

  #| apply function after transforming (or selecting) the arguments
   |   @(param f) function (b ...) -> c
   |   @(param g) function a -> b
   |   @(returns) function (a ...) -> c
   |
   | example: suppose we have a type `vector-3` with accessors `get-x`
   | `get-y` and `get-z`, and we want to find the maximum over the x-axis,
   | then we can say::
   |
   |    (define max-x (on max get-x))
   |#
  (define (on f g)
    (lambda X
      (apply f (map g X))))

  (define flip (lambda (f) (lambda (b a) (f a b))))

  (define (reverse-args f)
    (lambda X
      (apply f (reverse X))))

  (define (partial f . X)
    (let ((rev (reverse X)))
      (lambda Y
        (apply f (append-reverse rev Y)))))
  
  (define (partial* f . X)
    (let ((rev (reverse X)))
      (lambda (Y)
        (apply f (append-reverse rev Y)))))

  (define-syntax pipe
    (lambda (x)
      (syntax-case x ()
        [(pipe <in>)
         (syntax <in>)]
        [(pipe <in> <func1> <funcs> ...)
         (syntax (pipe (call-with-values (lambda () <in>) <func1>)
                       <funcs> ...))])))

  (define (any? proc lst)
    (not (find proc lst))) 

  (define (all? proc lst)
    (not (find (compose not proc) lst)))

  (define (unique-sorted eq? input)
    (if (null? input) 
      '()
      (let loop ([input  (cdr input)]
                 [output (list (car input))])
        (cond
          [(null? input)
           (reverse output)]
          [(eq? (car input) (car output))
           (loop (cdr input) output)]
          [else
           (loop (cdr input) (cons (car input) output))]))))

  ;;; (srfi :8 receive)
  (define-syntax receive
   (syntax-rules ()
     ((receive formals expression body ...)
      (call-with-values (lambda () expression)
                        (lambda formals body ...))))) 
)

