;; Chez-sockets: SRFI-106 basic sockets layer.
;;
;; Written by Akce 2019-2020, Unlicensed.

(library (socket basic)
  (export
   make-client-socket make-server-socket
   socket? socket-accept socket-close socket-recv socket-send socket-shutdown
   socket-merge-flags socket-purge-flags
   call-with-socket
   (rename
     (socket-port socket-input-port)
     (socket-port socket-output-port))
   *af-unspec* *af-inet* *af-inet6*
   *sock-stream* *sock-dgram*
   *ai-canonname* *ai-numerichost*
   *ai-v4mapped* *ai-all* *ai-addrconfig*
   *ipproto-ip* *ipproto-tcp* *ipproto-udp*
   *shut-rd* *shut-wr* *shut-rdwr*
   address-family address-info ip-protocol message-type shutdown-method socket-domain)
  (import
   (rnrs)
   (socket impl))

  (define-enum address-family
    [inet	*af-inet*]
    [inet6	*af-inet6*]
    [unspec	*af-unspec*])

  (define-bits address-info
    [addrconfig		*ai-addrconfig*]
    [all		*ai-all*]
    [canoname		*ai-canonname*]
    [numerichost	*ai-numerichost*]
    [v4mapped		*ai-v4mapped*])

  (define-enum ip-protocol
    [ip		*ipproto-ip*]
    [tcp	*ipproto-tcp*]
    [udp	*ipproto-udp*])

  (define-enum socket-domain
    [stream	*sock-stream*]
    [datagram	*sock-dgram*])

  (define-bits message-type
    [none	0]
    [peek	*msg-peek*]
    [oob	*msg-oob*]
    [wait-all	*msg-waitall*]))
