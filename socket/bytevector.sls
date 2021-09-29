;; bytevector extensions to r6rs.
;; Written by Jerry 2021
;; SPDX-License-Identifier: Unlicense

(library (socket bytevector)
  (export
    bytevector-slice
    bytevector/null->string)
  (import
    (rnrs))

  ;; Copy the first len bytes from src bytevector into a new bytevector.
  (define bytevector-slice
    (lambda (src len)
      (let ([bv (make-bytevector len)])
        (bytevector-copy! src 0 bv 0 len)
        bv)))

  ;; Like bytevector->string except it ends at the first null source byte.
  (define bytevector/null->string
    (lambda (bv)
      (utf8->string
        (let f ([i 0])
          (let ([c (bytevector-u8-ref bv i)])
            (if (fx=? c 0)
              (make-bytevector i)
              (let ([ret (f (fx+ i 1))])
                (bytevector-u8-set! ret i c)
                ret)))))))
  )
