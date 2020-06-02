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
    socket? socket-file-descriptor socket-accept socket-close socket-recv socket-send socket-shutdown
    connect-socket connect-server-socket connect-client-socket

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
   *so-acceptconn* *so-broadcast* *so-domain* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
   *so-protocol* *so-reuseaddr* *so-type*

   *ni-namereqd* *ni-dgram* *ni-nofqdn* *ni-numerichost* *ni-numericserv*
   *ni-maxhost* *ni-maxserv*

   (rename
     (getaddrinfo* getaddrinfo)
     (getnameinfo* getnameinfo))
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
    *so-acceptconn* *so-broadcast* *so-domain* *so-dontroute* *so-error* *so-keepalive* *so-linger*
    *so-oobinline* *so-protocol* *so-reuseaddr* *so-type*
    ;; ip multicast values.
    *ip-multicast-loop* *ip-multicast-ttl* *ip-multicast-if*
    *ip-add-membership* *ip-drop-membership*

    ;; getnameinfo(3).
    *ni-namereqd* *ni-dgram* *ni-nofqdn* *ni-numerichost* *ni-numericserv*
    *ni-maxhost* *ni-maxserv*

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
    [accept (int void* void*) int]
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
    ;; int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
    [getpeername (int sockaddr* (* socklen-t)) int]
    ;; int getnameinfo(const struct sockaddr *addr, socklen_t addrlen, char *host, socklen_t hostlen, char *serv, socklen_t servlen, int flags);
    [getnameinfo (sockaddr* socklen-t (* unsigned-8) socklen-t (* unsigned-8) socklen-t int) int]
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

  ;; The socket record type.
  ;; Use the record-name sockobj rather than socket as define-record-type will create syntax for
  ;; it and having it as socket would clash with the c-function 'socket'.
  ;; This record stores only the file descriptor as it's possible to retrieve a number of useful
  ;; properties based on this alone. cf. getnameinfo*, getpeername et al.
  ;; It also reduces foreign memory storage which the Chez docs recommend.
  (define-record-type (sockobj make-socket socket?)
    (fields
      [immutable file-descriptor socket-file-descriptor]))

  (define connect-server-socket
    (lambda (node service family socktype flags protocol)
      (let ([sock (connect-socket node service family socktype flags protocol bind)])
        ;; TCP sockets (streams) also need to be listened to.
        (when (and sock (= (socket-get-int sock *sol-socket* *so-type*) *sock-stream*))
          ;; TODO handle listen error.
          ;; TODO redo as case-lambda for maxconn setting?
          (listen (socket-file-descriptor sock) *somaxconn*))
        sock)))

  (define connect-client-socket
    (lambda (node service family socktype flags protocol)
      (connect-socket node service family socktype flags protocol connect)))

  (define socket-accept
    (lambda (sock)
      (let ([peerfd (accept (socket-file-descriptor sock) 0 0)])
        (case peerfd
          [-1
            ;; TODO examine errno and use strerror(3) for info.
            ;; TODO handle EAGAIN etc.
            (error #f "failed" sock)]
          [else
            (make-socket peerfd)]))))

  (define socket-recv
    (case-lambda
      [(sock len)
       (socket-recv sock len 0)]
      [(sock len flags)
       (alloc ([bv &bv unsigned-8 len])
         (let ([rc (recv (socket-file-descriptor sock) bv len flags)])
           (if (fx=? rc -1)
               (eof-object)
               (u8*->bv bv rc))))]))

  (define socket-send
    (case-lambda
      [(sock bv)
       (socket-send sock bv 0 (bytevector-length bv) 0)]
      [(sock bv flags)
       (socket-send sock bv 0 (bytevector-length bv) flags)]
      [(sock bv start n)
       (socket-send sock bv start n 0)]
      [(sock bv start n flags)
       (alloc ([buf &buf unsigned-8 n])
         (let loop ([i start])
           (when (fx<? i n)
             (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i))
             (loop (fx+ i 1))))
         (let ([rc (send (socket-file-descriptor sock) buf n flags)])
           (if (fx=? rc -1)
               ;; TODO raise an exception instead?
               #f
               rc)))]))

  (define socket-close
    (lambda (sock)
      (close (socket-file-descriptor sock))))

  (define socket-shutdown
    (lambda (sock how)
      ;; TODO assumes 'how' is valid.
      (shutdown (socket-file-descriptor sock) how)))

  (define socket-get-int
    ;; Inline an int* specific getsockopt define.
    (let ([f (foreign-procedure "getsockopt" (int int int (* int) (* int)) int)])
      (lambda (sock level optname)
        (alloc ([sz &sz int]
                [res &res int])
          (ftype-set! int () &sz (ftype-sizeof int))
          (let ([rc (f (socket-file-descriptor sock) level optname &res &sz)])
            ;; TODO check rc for error.
            (ftype-ref int () &res 0))))))

  (define socket-set-int!
    ;; Inline an int* specific setsockopt define.
    (let ([f (foreign-procedure "setsockopt" (int int int (* int) int) int)])
      (lambda (sock level optname optval)
        (alloc ([val &val int])
          (ftype-set! int () &val optval)
          (let ([rc (f (socket-file-descriptor sock) level optname &val (ftype-sizeof int))])
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
                     [sockfd (socket (addrinfo-family ai) (addrinfo-socktype ai) (addrinfo-protocol ai))])
                (case sockfd
                  [-1		; socket creation failed with these ai params, try next.
                    (loop (cdr as))]
                  [else
                    (case (action sockfd (addrinfo-addr ai) (addrinfo-addrlen ai))
                      [0
                       ;; Success.
                       (freeaddrinfo-list addrinfos)
                       (make-socket sockfd)]
                      [else
                        (close sockfd)
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
               ;; TODO alloc needs to guard for exceptions.
               (error 'getaddrinfo (gai-strerror rc) (list node service))])))]))

  (define freeaddrinfo-list
    (lambda (ais)
      (when (and (list? ais)
                 (not (null? ais)))
        (freeaddrinfo (car ais)))))

  (define getnameinfo*
    (case-lambda
      [(sock)
       (getnameinfo* sock 0)]
      [(sock flags)
       (alloc ([saddr &saddr unsigned-8 *s-sizeof-sockaddr*]
               [salen &salen socklen-t 1]
               [host &host unsigned-8 *ni-maxhost*]
               [serv &serv unsigned-8 *ni-maxserv*])
         (ftype-set! socklen-t () &salen *s-sizeof-sockaddr*)
         (let ([rc (getpeername (socket-file-descriptor sock) saddr &salen)])
           ;; TODO check getpeername return.
           (let ([rc (getnameinfo saddr (ftype-ref socklen-t () &salen)
                                  &host *ni-maxhost*
                                  &serv *ni-maxserv*
                                  flags)])
             (cond
               [(= rc 0)
                ;; Hmmm should this be values or list instead of a pair?
                (cons
                  (u8*->string &host)
                  (u8*->string &serv))]
               [else
                 ;; TODO alloc needs to guard for exceptions.
                 (error 'getnameinfo (gai-strerror rc))]
               ))))]
       ))

  (define mcast-add-membership
    (case-lambda
      [(sock node)
       (mcast-add-membership sock node 0)]
      [(sock node interface)
       (let ([domain (socket-get-int sock *sol-socket* *so-domain*)])
         (cond
           [(= domain *af-inet*)
            (mcast4-add-membership (socket-file-descriptor sock) node interface)]
           [(= domain *af-inet6*)
            (mcast6-add-membership (socket-file-descriptor sock) node interface)]
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
