(library (lyonesse munsch)
  (export
    #| Byte type system
     | ================
     |
     | We define a binary type on top of the R6RS bytevectors. Each type has
     | a `ref`, `set!` and `bytesize` field. These fields should be overloaded
     | in derived record types to represent composable compound types.
     |
     | The `bytesize` field speaks for itself. The setter and getter method
     | should be modelled after the `bytevector-*-ref` and `bytevector-*-set!`
     | functions, extending their functionality to compound types.
     |#
    type type? make-type type-ref type-set! type-bytesize type-repr with-type

    #| Arrays - array datatype
     | =======================
     |
     | The `array-type` record derives from `type`.
     |#
    array-type? make-array-type array-type-element-type array-type-length

    #| Arrays - array container
     | ------------------------
     |
     | Create an array from an element-type, bytevector, offset and length.
     | `array` is an immutable data type.
     |#
    array? make-array array-element-type array-data array-offset array-length
    with-array

    #| Arrays - higher level routines
     | ------------------------------
     |
     | Example, create a single precision array using the `bytearray` syntax
     | then convert result back to a list:
     |
     |     > (array->list (a (bytearray f32 (sqrt 2) 22/7)))
     |         -> (1.4142135381698608 3.142857074737549)
     |#
    array-ref array-set! bytearray array->list

    #| Structs
     | =======
     |
     | From a byte-data point of view, structs are tuples, and in that sense
     | very similar to a heterogeneous kind of arrays. The only difference
     | is that we name the elements in the structure and generate accessors
     | for them. Let's start with a simple example:
     |
     |     > (define-struct point (x f32) (y f32) (z f32))
     |
     | This defines a lot of functions and values.
     |
     |     > (type-repr point-x-type)
     |         -> f32
     |     > (type-repr point-type)
     |         -> (struct f32 f32 f32)
     |     > (define p (make-point))
     |     > (point? p)
     |         -> #t
     |     > (point-x-set! p (/ (+ (sqrt 5) 1) 2))
     |     > (point-x p)
     |         -> 1.6180340051651
     |     > (point-y-set! p (/ (- (sqrt 5) 1) 2))
     |     > (with-point p
     |         (/ y (/ 1 x)))
     |         -> 1.000000036705515
     |
     | For the moment nested struct definitions have to be defined manually.
     |
     |     > (define-struct color
     |         (red u8) (green u8) (blue u8) (alpha u8))
     |     > (define-struct vertex
     |         (texture (array f32 2))
     |         (color   (struct u8 u8 u8 u8))
     |         (point   (array f32 3)))
     |     > (define v (make-vertex))
     |     > (vertex-color-set! '(255 127 0 255))  ; orange
     |     > (color-green (vertex-color v))
     |         -> 127
     |     > (color? (vertex-color v))
     |         -> #f  ; fix me
     |
     | Also any alignment issues have to be solved by hand. We are limited
     | by the fact that the size of `color-type` is not known at the time
     | when the `(define-struct vertex ...)` syntax is being expanded.
     |#
    define-struct)

  (import (rnrs base (6))
          (lyonesse munsch private)
          (lyonesse munsch array)
          (lyonesse munsch struct))
)
