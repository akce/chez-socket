;; Chez scheme specific c/ftypes layer.
;; Written by Akce 2019.
;; SPDX-License-Identifier: Unlicense
(library (socket c)
  (export
   make-client-socket
   (rename
    (connection-close socket-close)
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

  (define-ftype conn void*)

  (c-function
   (make-client-connection (string string int int int int) conn)
   (connection-close (conn) void)
   (connection-shutdown (conn int) void))

  (define make-client-socket
    (case-lambda
     [(node service)
      (make-client-socket node service AF_INET)]
     [(node service ai-family)
      (make-client-socket node service ai-family SOCK_STREAM)]
     [(node service ai-family ai-socktype)
      (make-client-socket node service ai-family ai-socktype (bitwise-ior AI_V4MAPPED AI_ADDRCONFIG))]
     [(node service ai-family ai-socktype ai-flags)
      (make-client-socket node service ai-family ai-socktype ai-flags IPPROTO_IP)]
     [(node service ai-family ai-socktype ai-flags ai-protocol)
      (make-client-connection node service ai-family ai-socktype ai-flags ai-protocol)]))
  )
