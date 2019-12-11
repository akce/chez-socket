;; Chez scheme specific c/ftypes layer.
;; Written by Akce 2019.
;; SPDX-License-Identifier: Unlicense
(library (socket c)
  (export
   make-client-connection
   make-server-connection
   (rename
    (conn? socket?)
    (connection-accept socket-accept)
    (connection-close socket-close)
    (connection-recv socket-recv)
    (connection-send socket-send)
    (connection-shutdown socket-shutdown))
   AF_INET AF_INET6 AF_UNSPEC
   SOCK_STREAM SOCK_DGRAM
   AI_CANONNAME AI_NUMERICHOST AI_V4MAPPED AI_ALL AI_ADDRCONFIG
   IPPROTO_IP IPPROTO_TCP IPPROTO_UDP
   MSG_PEEK MSG_OOB MSG_WAITALL
   SHUT_RD SHUT_WR SHUT_RDWR
   )
  (import
   (chezscheme)
   (socket ftypes-util))

  (define lib-load
    (load-shared-object (locate-library-object "socket/libsocket.so")))

  ;; [syntax] c-val: extract integer value/s from memory address/es.
  ;;
  ;; eg, (c-val AF_INET AF_INET6) ->
  ;; (begin
  ;;   (define AF_INET (foreign-ref 'int (foreign-entry "c_AF_INET") 0))
  ;;   (define AF_INET6 (foreign-ref 'int (foreign-entry "c_AF_INET6") 0)))
  (define-syntax c-val
    (lambda (stx)
      (syntax-case stx ()
        [(_ name name* ...)
         (with-syntax ([(frefs ...)
                        (map
                         (lambda (n)
                           #`(define #,n
                               (foreign-ref 'int (foreign-entry
                                                  #,(string-append
                                                     "c_"
                                                     (symbol->string (syntax->datum n)))) 0)))
                         #'(name name* ...))])
                      #'(begin
                          frefs ...))])))

  (c-val
   AF_INET AF_INET6 AF_UNSPEC
   SOCK_STREAM SOCK_DGRAM
   AI_CANONNAME AI_NUMERICHOST AI_V4MAPPED AI_ALL AI_ADDRCONFIG
   IPPROTO_IP IPPROTO_TCP IPPROTO_UDP
   MSG_PEEK MSG_OOB MSG_WAITALL
   SHUT_RD SHUT_WR SHUT_RDWR)

  (define-ftype conn
    (struct
      [socketfd int]
      [addrs    void*]
      [addr     void*]))

  (define conn?
    (lambda (c)
      (ftype-pointer? conn c)))

  (c-function
   (make-client-connection (string string int int int int) (* conn))
   (make-server-connection (string int int int) (* conn))
   (connection-accept ((* conn)) (* conn))
   (connection-close ((* conn)) void)
   (connection_recv ((* conn) void* ssize_t int) int)
   (connection_send ((* conn) void* ssize_t int) int)
   (connection-shutdown ((* conn) int) void))

  (define u8*->bv
    (lambda (ptr len)
      (let ([bv (make-bytevector len)])
        (let loop ([i 0])
          (if (fx=? i len)
              bv
              (begin
                (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i))
                (loop (fx+ i 1))))))))

  (define connection-recv
    (case-lambda
      [(conn len)
       (connection-recv conn len 0)]
      [(conn len flags)
       (alloc ([bv &bv unsigned-8 len])
         (let ([rc (connection_recv conn bv len flags)])
           (if (fx=? rc -1)
               #f
               (u8*->bv bv rc))))]))

  (define connection-send
    (case-lambda
      [(conn bv)
       (connection-send conn bv 0)]
      [(conn bv flags)
       (let ([len (bytevector-length bv)])
         (alloc ([buf &buf unsigned-8 len])
           (let loop ([i 0])
             (when (fx<? i len)
               (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i))
               (loop (fx+ i 1))))
           (let ([rc (connection_send conn buf len flags)])
             (if (fx=? rc -1)
                 #f
                 rc))))]))
  )
