;; Chez scheme specific c/ftypes layer.
;; Written by Jerry 2019-2021.
;; SPDX-License-Identifier: Unlicense

(library (socket c)
  (export
    create-socket-reuseaddr
    socket? socket-file-descriptor socket-accept socket-close
    socket-peerinfo socket-recv socket-recvfrom socket-send socket-shutdown
    connect-server-socket connect-client-socket

    *af-inet* *af-inet6* *af-unspec*
    *sock-dgram* *sock-stream*
    *ai-all* *ai-addrconfig* *ai-canonname* *ai-numerichost* *ai-v4mapped*
    *ipproto-ip* *ipproto-tcp* *ipproto-udp*
    *msg-oob* *msg-peek* *msg-waitall*
    *shut-rd* *shut-wr* *shut-rdwr*

    *ai-numericserv* *ai-passive*
    socket-get-int socket-set-int!
    *ip-multicast-loop* *ip-multicast-ttl* *ip-multicast-if*

    *sol-socket*
    *so-acceptconn* *so-broadcast* *so-domain* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
    *so-protocol* *so-reuseaddr* *so-type*

    *ni-namereqd* *ni-dgram* *ni-nofqdn* *ni-numerichost* *ni-numericserv*
    *ni-maxhost* *ni-maxserv*

    (rename
      (getnameinfo/bv getnameinfo)
      (gethostname* gethostname))

    mcast-add-membership
    )
  (import
   (chezscheme)
   (socket bytevector)
   (socket ftypes-util))

  ;; Note that 'load-shared-object' also loads, and makes available, public
  ;; symbols from shared objects linked to the library we're loading here.
  ;; Hence the availability of getsockopt, setsockopt, etc.
  (define lib-load
    (load-shared-object (locate-library-object "socket/libsocket.so")))

  ;; [parameter] create-socket-reuseaddr: Determines whether connect-socket will turn on SO_REUSEADDR before socket bind/connect action.
  (define create-socket-reuseaddr
    (make-parameter #f))

  ;; [syntax] c-const: extract integer value/s from memory address/es.
  ;;
  ;; eg, (c-const *af-inet* *af-inet6*) ->
  ;; (begin
  ;;   (define *af-inet* (foreign-ref 'int (foreign-entry "c_AF_INET") 0))
  ;;   (define *af-inet6* (foreign-ref 'int (foreign-entry "c_AF_INET6") 0)))
  (define-syntax c-const
    (lambda (stx)
      ;; convert symbols from *word1-wordn..* to "c_WORD1_WORDN.."
      ;; eg, (*sym->c-var-str '*symbol-name*) => "c_SYMBOL_NAME"
      (define (*sym->c-var-str sym)
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
                  (string->list (symbol->string sym)))))))
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
    ;; ssize_t recv(int sockfd, void *buf, size_t len, int flags);
    [recv (int u8* size_t int) ssize_t]
    ;; ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags, struct sockaddr *src_addr, socklen_t *addrlen);
    [recvfrom (int u8* size_t int u8* (* socklen-t)) ssize_t]
    [send (int u8* size_t int) ssize_t]
    [shutdown (int int) int]
    ;; char *strerror(int errnum);
    [strerror (int) string]

    ;;;; Address info.
    ;; int getaddrinfo(const char *node, const char *service, const struct addrinfo *hints, struct addrinfo **res);
    [getaddrinfo (string string addrinfo* (* addrinfo*)) int]
    ;; void freeaddrinfo(struct addrinfo *res);
    [freeaddrinfo (addrinfo*) void]
    ;; int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
    [getpeername (int u8* (* socklen-t)) int]
    ;; int getnameinfo(const struct sockaddr *addr, socklen_t addrlen, char *host, socklen_t hostlen, char *serv, socklen_t servlen, int flags);
    [getnameinfo (u8* socklen-t u8* socklen-t u8* socklen-t int) int]
    ;; const char *gai_strerror(int errcode);
    [gai-strerror (int) string]
    ;; gethostname(2)
    ;; int gethostname(char *name, size_t len);
    [gethostname (u8* size_t) int]

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
  ;; properties based on this alone. cf. socket-peerinfo, getpeername et al.
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
      (let-values ([(peerfd errno) (call-procedure/errno accept (socket-file-descriptor sock) 0 0)])
        (case peerfd
          [(-1)
           ;; TODO handle EAGAIN etc.
           (error 'socket-accept (strerror errno) sock errno)]
          [else
            (make-socket peerfd)]))))

  (define socket-recv
    (case-lambda
      [(sock len)
       (socket-recv sock len 0)]
      [(sock len flags)
       (let ([buf (make-bytevector len)])
         ;; See recv(2).
         (let-values ([(rc errno) (call-procedure/errno recv (socket-file-descriptor sock) buf len flags)])
           (cond
             [(fx>? rc 0)
              (bytevector-slice buf rc)]
             [(fx=? rc 0)	; socket EOF.
              0]
             [else
               (error 'socket-recv (strerror errno) errno)])))]))

  ;; [proc] socket-recvfrom: recv data and sender info.
  ;; [return] (cons data-u8-bytevector sockaddr-u8-bytevector)
  ;; sockaddr is suitable for use with getnameinfo.
  (define socket-recvfrom
    (case-lambda
      [(sock len)
       (socket-recvfrom sock len 0)]
      [(sock len flags)
       (alloc ([salen &salen socklen-t])
         (ftype-set! socklen-t () &salen *s-sizeof-sockaddr*)
         (let*-values ([(buf) (make-bytevector len)]
                       [(saddr) (make-bytevector *s-sizeof-sockaddr*)]
                       [(rc errno) (call-procedure/errno recvfrom (socket-file-descriptor sock) buf len flags saddr &salen)])
           (cond
             [(fx>? rc 0)
              (cons (bytevector-slice buf rc) (bytevector-slice saddr (ftype-ref socklen-t () &salen)))]
             [(fx=? rc 0)	; socket EOF.
              0]
             [else
               (error 'socket-recvfrom (strerror errno) errno)])))]))

  (define socket-send
    (case-lambda
      [(sock bv)
       (socket-send sock bv 0 (bytevector-length bv) 0)]
      [(sock bv flags)
       (socket-send sock bv 0 (bytevector-length bv) flags)]
      [(sock bv start n)
       (socket-send sock bv start n 0)]
      [(sock bv start n flags)
       (if (not (fx=? start 0))
         ;; XXX I'm yet to see a non-zero start value so raise this until i implement a full bytevector-slice.
         (error 'socket-send "non-zero socket-send!!")
         (let-values ([(rc errno) (call-procedure/errno send (socket-file-descriptor sock) bv n flags)])
           (cond
             [(fx>=? rc 0)
              rc]
             [else
               (error 'socket-send (strerror errno) errno)])))]))

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
          (let-values ([(rc errno) (call-procedure/errno f (socket-file-descriptor sock) level optname &res &sz)])
            (cond
              [(fx=? rc -1)
               (error 'socket-get-int (strerror errno) errno)]
              [else
                (ftype-ref int () &res 0)]))))))

  (define socket-set-int!
    ;; Inline an int* specific setsockopt define.
    (let ([f (foreign-procedure "setsockopt" (int int int (* int) int) int)])
      (lambda (sock level optname optval)
        (alloc ([val &val int])
          (ftype-set! int () &val optval)
          (let-values ([(rc errno)
                        (call-procedure/errno
                          f
                          ;; Allow sock to be a socket-file-descriptor already. Used by the
                          ;; connect-socket socket config code.
                          (if (socket? sock)
                            (socket-file-descriptor sock)
                            sock)
                          level optname &val (ftype-sizeof int))])
            (cond
              [(fx=? rc -1)
               (error 'socket-set-int! (strerror errno) errno)]
              [else
                rc]))))))

  ;; Largely follows the example from getaddrinfo(2).
  ;; action must be bind(2) or connect(2).
  ;; Return: connection record on success, error exception otherwise.
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
             (error 'connect-socket "no suitable address found" node service family socktype flags protocol)]
            [else
              (let* ([ai (car as)]
                     [sockfd (socket (addrinfo-family ai) (addrinfo-socktype ai) (addrinfo-protocol ai))])
                (case sockfd
                  [-1		; socket creation failed with these ai params, try next.
                    (loop (cdr as))]
                  [else
                    ;; Optionally configure the socket before action (bind or connect).
                    ;; Using a parameter this way is very limiting. This will change to a more flexible method in future.
                    ;; eg, sockets will be connected via a socket builder (there's too many args in this function ATM!!) or
                    ;; config activity will be exposed as part of public 'action' functions, something like 'bind/reuseaddr'.
                    (when (create-socket-reuseaddr)
                      (socket-set-int! sockfd *sol-socket* *so-reuseaddr* 1))
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

  ;; [proc] getnameinfo/bv: getnameinfo bytevector interface
  ;; [args] sockaddr [flags]
  ;; [return] name string
  ;; `sockaddr` is a bytevector containing the socket address.
  ;; hostname, as a string, is returned or an error is raised.
  (define getnameinfo/bv
    (case-lambda
      [(sockaddr)
       ;; default flags to 0.
       (getnameinfo/bv sockaddr 0)]
      [(sockaddr flags)
       ;; Note: getnameinfo(3) null terminates the strings so we do not need to init bytevectors to null bytes.
       (let* ([host (make-bytevector *ni-maxhost*)]
              [serv (make-bytevector *ni-maxserv*)]
              [rc (getnameinfo
                    sockaddr (bytevector-length sockaddr)
                    host (bytevector-length host)
                    serv (bytevector-length serv)
                    flags)])
         (cond
           [(fx=? rc 0)
            ;; Hmmm should this be values instead of a pair?
            (cons
              (bytevector/null->string host)
              (bytevector/null->string serv))]
           [else
             (error 'getnameinfo (gai-strerror rc))]))]))

  (define gethostname*
    (lambda ()
      (let*-values ([(buf) (make-bytevector *ni-maxhost*)]
                    [(rc errno) (call-procedure/errno gethostname buf (bytevector-length buf))])
        (cond
          [(fx=? rc 0)
           (bytevector/null->string buf)]
          [else
            (error 'gethostname (strerror errno) errno)]))))

  (define socket-peerinfo
    (case-lambda
      [(sock)
       (socket-peerinfo sock 0)]
      [(sock flags)
       (alloc ([salen &salen socklen-t])
         (ftype-set! socklen-t () &salen *s-sizeof-sockaddr*)
         (let* ([saddr (make-bytevector *s-sizeof-sockaddr*)]
                [rc (getpeername (socket-file-descriptor sock) saddr &salen)])
           ;; TODO check getpeername return.
           (getnameinfo/bv (bytevector-slice saddr (ftype-ref socklen-t () &salen)) flags)))]))

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
  )
