(library (lyonesse parsing xml)
  (export xml:read xml:from-string)

  (import (rnrs base (6))
          (rnrs io ports (6))
          (lyonesse match)
          (lyonesse functional)
          (lyonesse parsing xml parser)
          (lyonesse parsing xml clean))

  (define (xml:read filename)
    (map xml:clean
      (xml:ast-from-port
        (open-file-input-port filename 
                              (file-options) 
                              'line 
                              (native-transcoder)))))
  (define (xml:from-string s)
    (map xml:clean (xml:ast-from-port
                     (open-string-input-port s))))
)
