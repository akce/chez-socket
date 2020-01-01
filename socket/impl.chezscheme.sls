;; The export list is taken from "Implementation dependent layer -> For others":
;; https://srfi.schemers.org/srfi-106/srfi-106.html
;; Copyright (C) Takashi Kato (2012). All Rights Reserved.
;; Library implementation (such as it is) written by Akce 2019, 2020 Unlicensed.

(library (socket impl)
  (export
   make-client-socket make-server-socket
   socket? call-with-socket
   socket-accept socket-send socket-recv socket-shutdown socket-close
   (rename (bitwise-ior socket-merge-flags)
           (bitwise-xor socket-purge-flags))
   (rename (AF_UNSPEC *af-unspec*)
           (AF_INET   *af-inet*)
           (AF_INET6  *af-inet6*))
   (rename (SOCK_STREAM *sock-stream*)
           (SOCK_DGRAM  *sock-dgram*))
   (rename (AI_CANONNAME   *ai-canonname*)
           (AI_NUMERICHOST *ai-numerichost*)
           (AI_V4MAPPED    *ai-v4mapped*)
           (AI_ALL         *ai-all*)
           (AI_ADDRCONFIG  *ai-addrconfig*))
   (rename (IPPROTO_IP  *ipproto-ip*)
           (IPPROTO_TCP *ipproto-tcp*)
           (IPPROTO_UDP *ipproto-udp*))
   (rename (MSG_PEEK     *msg-peek*)
           (MSG_OOB      *msg-oob*)
           (MSG_WAITALL  *msg-waitall*))
   (rename (SHUT_RD   *shut-rd*)
           (SHUT_WR   *shut-wr*)
           (SHUT_RDWR *shut-rdwr*)))
  (import
   (chezscheme)
   (socket c))

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

  (define make-server-socket
    (case-lambda
      [(service)
       (make-server-socket service AF_INET)]
      [(service ai-family)
       (make-server-socket service ai-family SOCK_STREAM)]
      [(service ai-family ai-socktype)
       (make-server-socket service ai-family ai-socktype IPPROTO_IP)]
      [(service ai-family ai-socktype ai-protocol)
       (make-server-connection service ai-family ai-socktype ai-protocol)]))

  ;; call-with-socket is adapted from the call-with-port example found here:
  ;; https://scheme.com/tspl4/control.html#defn:call-with-port
  (define call-with-socket
    (lambda (conn proc)
      (call-with-values (lambda () (proc conn))
        (case-lambda
          [(val) (socket-close conn) val]
          [val* (socket-close conn) (apply values val*)])))))
