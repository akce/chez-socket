;; Chez-socket: Extended Socket interface.
;;
;; Interface extensions over basic sockets (SRFI-106).
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

(library
  (socket extended)
  (export
    socket-fd
    (rename
      ;; Cheating a bit here as the srfi-106 reference implementation creates all ports as input/output.
      ;; Adding an explicit input/output as the underlying implementation may change one day.
      (socket-port socket-input/output-port))

    address-info

    socket-opt-level socket-opt
    socket-get-int socket-set-int!
    ;; XXX For now keep the following defs private. I'd rather not clutter the namespace so much and i prefer
    ;; XXX only one way to access values.
    ;;getsockopt setsockopt
    ;;*sol-socket*
    ;;*so-acceptconn* *so-broadcast* *so-dontroute* *so-error* *so-keepalive* *so-linger* *so-oobinline*
    ;;*so-protocol* *so-reuseaddr* *so-type*
    )
  (import
    (chezscheme)
    (socket impl)
    (only (socket c) socket-fd))
  ;; Export the basic interface, except for things overridden here.
  (export
    (import (except (socket basic) address-info)))

  ;; See netdb.h(0P)
  (define-bits address-info
    [addrconfig		*ai-addrconfig*]	; Return only IPv4 or IPv6 addresses depending on config.
    [all		*ai-all*]		; Find both IPv6 and IPv4 addresses.
    [canoname		*ai-canonname*]		; Returned hostname is canonical.
    [numerichost	*ai-numerichost*]	; Lookup hostname for numeric address.
    [v4mapped		*ai-v4mapped*]		; Failed IPv6 addresses are returned as IPv4 mapped IPv6.
    ;; Extensions to SRFI:
    [numericserv	*ai-numericserv*]	; service (port) is a number: prevents name lookup.
    [passive		*ai-passive*]		; Intent to bind(3) socket.
    )

  (define-enum socket-opt-level
    [socket	*sol-socket*]		; getsockopt(2) SOL_SOCKET.
    [ip		*ipproto-ip*]		; ip(7) IPPROTO_IP.
    [tcp	*ipproto-tcp*]		; tcp(7) IPPROTO_TCP.
    [udp	*ipproto-udp*])		; upd(7) IPPROTO_UDP.

  (define-enum socket-opt
    [acceptconn	*so-acceptconn*]	; bool read-only
    [broadcast	*so-broadcast*]		; bool datagram only
    [dontroute	*so-dontroute*]		; bool
    [error	*so-error*]		; read-only: value cleared after read
    [keepalive	*so-keepalive*]		; bool
    [linger	*so-linger*]		; linger struct
    [oobinline	*so-oobinline*]		; bool
    [protocol	*so-protocol*]		; read-only
    [reuseaddr  *so-reuseaddr*]		; bool
    [type	*so-type*])		; read-only
  )
