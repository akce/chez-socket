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
   (rename
    (connection? socket?)
    (connection-socket-fd socket-fd)
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
   *ip-multicast-loop* *ip-multicast-ttl* *ip-multicast-if*
   *sol-socket*
   *so-acceptconn* *so-broadcast* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
   *so-protocol* *so-reuseaddr* *so-type*

   (rename (getaddrinfo* getaddrinfo))
   freeaddrinfo
   gai-strerror
   make-addrinfo-hints
   addrinfo-flags
   addrinfo-family
   addrinfo-socktype
   addrinfo-protocol
   addrinfo-addrlen
   addrinfo-addr
   freeaddrinfo-list

   socket accept close bind connect listen recv send

   connect-socket
   connect-server-socket
   connect-client-socket
   ;;connection-socket-fd connection-addrinfo-list connection-addrinfo
   mcast-add-membership
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

  ;;;; Socket POSIX interface.
  (c-const
    *af-inet* *af-inet6* *af-unspec*
    *sock-dgram* *sock-stream*
    *ai-all* *ai-addrconfig* *ai-canonname* *ai-numerichost* *ai-v4mapped*
    *ai-numericserv* *ai-passive*	; extensions to srfi-106.
    *ipproto-ip* *ipproto-tcp* *ipproto-udp*
    *msg-oob* *msg-peek* *msg-waitall*
    *shut-rd* *shut-wr* *shut-rdwr*

    ;;;; More extensions to srfi-106.
    ;; the maximum number of connections to hold in the listen(2) queue.
    *somaxconn*
    ;; socket options (sockopt) values.
    *sol-socket*
    *so-acceptconn* *so-broadcast* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
    *so-protocol* *so-reuseaddr* *so-type*
    ;; ip multicast values.
    *ip-multicast-loop* *ip-multicast-ttl* *ip-multicast-if*
    *ip-add-membership* *ip-drop-membership*

    ;;;; Local libsocket.so interface.
    ;; sockaddr_storage size
    *s-sizeof-sockaddr*
    )

  ;;;; POSIX types.
  (define-ftype addrinfo* void*)
  (define-ftype sockaddr* void*)
  ;; socklen_t is a bit harder to pin down, but it appears to be an int-32 on linux: <bits/types.h>
  (define-ftype socklen-t integer-32)

  ;; TODO need access to 'errno' if accessing these functions at scheme level.
  ;; TODO define errno with a c-var syntax transformer that uses 'identifier-syntax'?
  (c-function
    ;;;;;; POSIX raw socket API.
    ;; int socket(int domain, int type, int protocol);
    [socket (int int int) int]
    ;; int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
    [accept (int sockaddr* (* socklen-t)) int]
    ;; int close(int fd);
    [close (int) int]
    ;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
    [bind (int sockaddr* socklen-t) int]
    ;; int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
    [connect (int sockaddr* socklen-t) int]
    [listen (int int) int]
    [recv (int void* size_t int) ssize_t]
    [send (int void* size_t int) ssize_t]
    [shutdown (int int) int]

    ;;;; Address info.
    ;; int getaddrinfo(const char *node, const char *service, const struct addrinfo *hints, struct addrinfo **res);
    [getaddrinfo (string string addrinfo* (* addrinfo*)) int]
    ;; void freeaddrinfo(struct addrinfo *res);
    [freeaddrinfo (addrinfo*) void]
    ;; const char *gai_strerror(int errcode);
    [gai-strerror (int) string]

    ;;;; Socket options.
    ;; See getsockopt(2)
    ;;     int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
    [getsockopt (int int int void* (* socklen-t)) int]
    ;; See setsockopt(2)
    ;;     int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
    [setsockopt (int int int void* socklen-t) int]

    ;;;; libsocket.so helper functions.
    [make-addrinfo-hints (int int int int) addrinfo*]
    [addrinfo-flags (addrinfo*) int]
    [addrinfo-family (addrinfo*) int]
    [addrinfo-socktype (addrinfo*) int]
    [addrinfo-protocol (addrinfo*) int]
    [addrinfo-addrlen (addrinfo*) socklen-t]
    [addrinfo-addr (addrinfo*) sockaddr*]
    [addrinfo-next (addrinfo*) addrinfo*]

    ;; multicasting.
    [mcast4-add-membership (int string int) int]
    [mcast6-add-membership (int string int) int]
    )

  (define-record-type connection
    (fields
      socket-fd
      addrinfo-list	; Store result of getaddrinfo(3) in case they need to be freed.
      addrinfo		; addrinfo instance used for the connected socket.
      ))

  (define connect-server-socket
    (lambda (node service family socktype flags protocol)
      (let ([c (connect-socket node service family socktype flags protocol bind)])
        ;; TCP sockets (streams) also need to be listened to.
        (when (and c (eqv? (addrinfo-socktype (connection-addrinfo c)) *sock-stream*))
          ;; TODO handle listen error.
          (listen (connection-socket-fd c) *somaxconn*))
        c)))

  (define connect-client-socket
    (lambda (node service family socktype flags protocol)
      (connect-socket node service family socktype flags protocol connect)))

  (define connection-accept
    (lambda (conn)
      (let* ([sz *s-sizeof-sockaddr*]
             [sock-storage (foreign-alloc sz)])
        (alloc ([sl &sl socklen-t])
          (ftype-set! socklen-t () &sl sz)
          (let ([peerfd (accept (connection-socket-fd conn) sock-storage &sl)])
            (foreign-free sock-storage)
            (case peerfd
              [-1
                #f]
              [else
                ;; TODO store peer address info.
                (make-connection peerfd #f #f)]))))))

  (define connection-recv
    (case-lambda
      [(conn len)
       (connection-recv conn len 0)]
      [(conn len flags)
       (alloc ([bv &bv unsigned-8 len])
         (let ([rc (recv (connection-socket-fd conn) bv len flags)])
           (if (fx=? rc -1)
               (eof-object)
               (u8*->bv bv rc))))]))

  (define connection-send
    (case-lambda
      [(conn bv)
       (connection-send conn bv 0 (bytevector-length bv) 0)]
      [(conn bv flags)
       (connection-send conn bv 0 (bytevector-length bv) flags)]
      [(conn bv start n)
       (connection-send conn bv start n 0)]
      [(conn bv start n flags)
       (alloc ([buf &buf unsigned-8 n])
         (let loop ([i start])
           (when (fx<? i n)
             (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i))
             (loop (fx+ i 1))))
         (let ([rc (send (connection-socket-fd conn) buf n flags)])
           (if (fx=? rc -1)
               ;; TODO raise an exception instead?
               #f
               rc)))]))

  (define connection-close
    (lambda (conn)
      (close (connection-socket-fd conn))))

  (define connection-shutdown
    (lambda (conn how)
      ;; TODO assumes 'how' is valid.
      (shutdown (connection-socket-fd conn) how)))

  (define socket-get-int
    ;; Inline an int* specific getsockopt define.
    (let ([f (foreign-procedure "getsockopt" (int int int (* int) (* int)) int)])
      (lambda (conn level optname)
        (alloc ([sz &sz int]
                [res &res int])
          (ftype-set! int () &sz (ftype-sizeof int))
          (let ([rc (f (connection-socket-fd conn) level optname &res &sz)])
            ;; TODO check rc for error.
            (ftype-ref int () &res 0))))))

  (define socket-set-int!
    ;; Inline an int* specific setsockopt define.
    (let ([f (foreign-procedure "setsockopt" (int int int (* int) int) int)])
      (lambda (conn level optname optval)
        (alloc ([val &val int])
          (ftype-set! int () &val optval)
          (let ([rc (f (connection-socket-fd conn) level optname &val (ftype-sizeof int))])
            ;; TODO check rc for error and raise an exception rather than return rc.
            rc)))))

  ;; Largely follows the example from getaddrinfo(2).
  ;; action must be bind(2) or connect(2).
  ;; Return: connection record on success, #f otherwise.
  (define connect-socket
    (lambda (node service family socktype flags protocol action)
      (let* ([hints (make-addrinfo-hints flags family socktype protocol)]
             [addrinfos (getaddrinfo* node service hints)])
        (freeaddrinfo hints)
        (let loop ([as addrinfos])
          (cond
            [(null? as)
             ;; Unable to connect to any address. Cleanup before exit.
             (freeaddrinfo-list addrinfos)
             #f]
            [else
              (let* ([ai (car as)]
                     [sock (socket (addrinfo-family ai) (addrinfo-socktype ai) (addrinfo-protocol ai))])
                (case sock
                  [-1		; socket creation failed with these ai params, try next.
                    (loop (cdr as))]
                  [else
                    (case (action sock (addrinfo-addr ai) (addrinfo-addrlen ai))
                      [0
                       (make-connection sock addrinfos ai)]
                      [else
                        (close sock)
                        (loop (cdr as))])]))])))))

  ;; getaddrinfo* wraps getaddrinfo(3) so that the result C-style linked list is returned as a scheme list.
  (define getaddrinfo*
    (case-lambda
      [(node service)
       (getaddrinfo* node service 0)]
      [(node service hints)
       (alloc ([res &res addrinfo*])
         (let ([rc (getaddrinfo node service hints &res)])
           (case rc
             [0
              ;; Traverse addrinfo->next, accumulating them in a list.
              (let loop ([next (ftype-ref addrinfo* () &res 0)] [acc '()])
                (cond
                  [(eqv? next 0)
                   (reverse acc)]
                  [else
                    (loop (addrinfo-next next) (cons next acc))]))]
             [else
               (error #f (gai-strerror rc) (list node service))])))]))

  (define freeaddrinfo-list
    (lambda (ais)
      (when (and (list? ais)
                 (not (null? ais)))
        (freeaddrinfo (car ais)))))

  (define mcast-add-membership
    (case-lambda
      [(conn node)
       (mcast-add-membership conn node 0)]
      [(conn node interface)
       (let ([family (addrinfo-family (connection-addrinfo conn))])
         (cond
           [(= family *af-inet*)
            (mcast4-add-membership (connection-socket-fd conn) node interface)]
           [(= family *af-inet6*)
            (mcast6-add-membership (connection-socket-fd conn) node interface)]
           [else
             ;; TODO raise exception?
             #f]))]))

  ;;;; Utility functions.
  (define u8*->bv
    (lambda (ptr len)
      (let ([bv (make-bytevector len)])
        (let loop ([i 0])
          (if (fx=? i len)
              bv
              (begin
                (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i))
                (loop (fx+ i 1))))))))
  )
