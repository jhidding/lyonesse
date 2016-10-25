(library (lyonesse munsch random)
  (export random-seed randomize-timer random-integer)
  (import (rnrs base (6))
          (only (chezscheme) load-shared-object foreign-procedure))

  (define librandom (load-shared-object "random.so"))
  
  (define random-seed
    (foreign-procedure "random_seed" (unsigned-64) void))
  
  (define randomize-timer
    (foreign-procedure "randomize_timer" () void))
  
  (define random-integer
    (foreign-procedure "random_integer" (int int) int))
)
