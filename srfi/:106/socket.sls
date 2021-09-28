;; Chez-sockets: SRFI-106 basic sockets layer.
;;
;; Written by Jerry 2019-2021
;; SPDX-License-Identifier: Unlicense
;;
;; Note that this library mostly serves to restrict (socket impl) to the basic sockets interface.

(library (srfi :106 socket)
  (export
    make-client-socket make-server-socket
    socket? socket-accept socket-close socket-recv socket-send socket-shutdown
    socket-merge-flags socket-purge-flags
    call-with-socket
    (rename
      (open-socket-input-port socket-input-port)
      (open-socket-output-port socket-output-port))

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

  ;; Basic socket (socket impl) overrides.

  (define-bits address-info
    [addrconfig		*ai-addrconfig*]
    [all		*ai-all*]
    [canoname		*ai-canonname*]
    [numerichost	*ai-numerichost*]
    [v4mapped		*ai-v4mapped*])
  )
