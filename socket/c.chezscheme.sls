;; Chez scheme specific c/ftypes layer.
;; Written by Akce 2019-2020.
;; SPDX-License-Identifier: Unlicense
;;
;; XXX Just discovered RFCs that specify the BSD IPv6 socket interface!
;; XXX Loading via C (to resolve values and structs) may not be needed. Need to investigate further..
;; XXX RFC3492 Basic Socket Interface Extensions for IPv6 (https://tools.ietf.org/html/rfc3493)
;; XXX RFC3542 Advanced Sockets Application Program Interface (API) for IPv6 (https://tools.ietf.org/html/rfc3542)
;; BSD sockets from the FreeBSD developers handbook:
;; https://www.freebsd.org/doc/en/books/developers-handbook/ipc.html

(library (socket c)
  (export
   make-client-connection
   make-server-connection
   (rename
    (conn? socket?)
    (conn-socketfd socket-fd)
    (connection-accept socket-accept)
    (connection-close socket-close)
    (connection-recv socket-recv)
    (connection-send socket-send)
    (connection-shutdown socket-shutdown))
   *af-inet* *af-inet6* *af-unspec*
   *sock-dgram* *sock-stream*
   *ai-all* *ai-addrconfig* *ai-canonname* *ai-numerichost* *ai-v4mapped*
   *ipproto-ip* *ipproto-tcp* *ipproto-udp*
   *msg-oob* *msg-peek* *msg-waitall*
   *shut-rd* *shut-wr* *shut-rdwr*

   *ai-numericserv* *ai-passive*
   getsockopt setsockopt
   socket-get-int socket-set-int!
   *sol-socket*
   *so-acceptconn* *so-broadcast* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
   *so-protocol* *so-reuseaddr* *so-type*
   )
  (import
   (chezscheme)
   (socket ftypes-util))

  ;; Note that 'load-shared-object' also loads, and makes available, public
  ;; symbols from shared objects linked to the library we're loading here.
  ;; Hence the availability of getsockopt, setsockopt, etc.
  (define lib-load
    (load-shared-object (locate-library-object "socket/libsocket.so")))

  ;; [proc] *sym->c-var-str
  ;; c-const helper function that converts symbols from *word1-wordn..* to "c_WORD1_WORDN.."
  ;; eg, (*sym->c-var-str '*symbol-name*) => "c_SYMBOL_NAME"
  (meta
    define *sym->c-var-str
    (lambda (sym)
      (list->string
        `(#\c #\_
          ,@(filter
              (lambda (c)
                (not (char=? c #\*)))
              (map
                (lambda (c)
                  (cond
                    [(char=? c #\-)
                     #\_]
                    [else
                      (char-upcase c)]))
                (string->list (symbol->string sym))))))))

  ;; [syntax] c-const: extract integer value/s from memory address/es.
  ;;
  ;; eg, (c-const *af-inet* *af-inet6*) ->
  ;; (begin
  ;;   (define *af-inet* (foreign-ref 'int (foreign-entry "c_AF_INET") 0))
  ;;   (define *af-inet6* (foreign-ref 'int (foreign-entry "c_AF_INET6") 0)))
  (define-syntax c-const
    (lambda (stx)
      (syntax-case stx ()
        [(_ name name* ...)
         (with-syntax ([(frefs ...)
                        (map
                         (lambda (n)
                           #`(define #,n
                               (foreign-ref 'int (foreign-entry #,(*sym->c-var-str (syntax->datum n))) 0)))
                         #'(name name* ...))])
                      #'(begin
                          frefs ...))])))

  (c-const
    *af-inet* *af-inet6* *af-unspec*
    *sock-dgram* *sock-stream*
    *ai-all* *ai-addrconfig* *ai-canonname* *ai-numerichost* *ai-v4mapped*
    *ipproto-ip* *ipproto-tcp* *ipproto-udp*
    *msg-oob* *msg-peek* *msg-waitall*
    *shut-rd* *shut-wr* *shut-rdwr*)

  (define-ftype conn
    (struct
      [socketfd int]
      [addrs    void*]
      [addr     void*]))

  (define conn?
    (lambda (c)
      (ftype-pointer? conn c)))

  ;; [proc] conn-socketfd: returns the socket file descriptor for the connection.
  (define conn-socketfd
    (lambda (c)
      (ftype-ref conn (socketfd) c)))

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

  ;;;;;;; Extensions to Basic Sockets (SRFI-106).

  (c-const
    ;; Address info.
    *ai-numericserv* *ai-passive*)

  ;;;;;;; Socket options.
  ;; TODO need access to 'errno' if accessing these functions at scheme level.
  ;; TODO define errno with a c-var syntax transformer that uses 'identifier-syntax'?
  (c-function
    ;; See getsockopt(2)
    ;;     int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
    ;; socklen_t was hard to track down, but it appears to be an int-32 on linux: <bits/types.h>
    (getsockopt (int int int void* (* int)) int)
    ;; See setsockopt(2)
    ;;     int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
    (setsockopt (int int int void* (* int)) int))

  (c-const
    *sol-socket*
    *so-acceptconn* *so-broadcast* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
    *so-protocol* *so-reuseaddr* *so-type*)

  (define socket-get-int
    ;; Inline an int* specific getsockopt define.
    (let ([f (foreign-procedure "getsockopt" (int int int (* int) (* int)) int)])
      (lambda (conn level optname)
        (alloc ([sz &sz int]
                [res &res int])
          (ftype-set! int () &sz (ftype-sizeof int))
          (let ([rc (f (conn-socketfd conn) level optname &res &sz)])
            ;; TODO check rc for error.
            (ftype-ref int () &res 0))))))

  (define socket-set-int!
    ;; Inline an int* specific setsockopt define.
    (let ([f (foreign-procedure "setsockopt" (int int int (* int) int) int)])
      (lambda (conn level optname optval)
        (alloc ([val &val int])
          (ftype-set! int () &val optval)
          (let ([rc (f (conn-socketfd conn) level optname &val (ftype-sizeof int))])
            ;; TODO check rc for error and raise an exception rather than return rc.
            rc)))))
  )
