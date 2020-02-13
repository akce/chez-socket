;; Chez-socket: Extended Socket interface.
;;
;; Interface extensions over basic sockets (SRFI-106).
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense
;;
;; TODOs
;; - AF_UNIX support
;; ? Numeric IPv4 & IPv6 address builders? eg,
;;   - (ipv4/any) => INADDR_ANY
;;   - (ipv6/any) => IN6ADDR_ANY_INIT
;;   - (ipv6/multicast/link-local fill 1f 10) => "ff:02::1f:10"
;;   - Implicit fill: (ipv6/multicast/site-local ca fe 1) => "ff:05::ca:fe:01"

(library
  (socket extended)
  (export
    address-info socket-opt-level socket-opt
    (rename
      ;; Cheating a bit here as the srfi-106 reference implementation creates all ports as input/output.
      ;; Adding an explicit input/output as the underlying implementation may change one day.
      (socket-port socket-input/output-port)))
  (import
    (chezscheme)
    (socket impl))
  ;; Re-export the implementation interface.
  ;; TODO hide internals at some point, but this is very handy while developing.
  (export
    (import (socket impl)))

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
    [type	*so-type*]		; read-only

    ;; ip(7) multicast options.
    [multicast-loop	*ip-multicast-loop*]	; bool: sender also receives own datagrams.
    [multicast-ttl	*ip-multicast-ttl*]	; int: datagram time-to-live.
    ;; The following 3 multicast opts all take ip_mreq(n) structs. As such, C functions are
    ;; provided to hide the struct details and should be used instead. These are here just in case.
    [multicast-if	*ip-multicast-if*]	; Set local interface for multicast socket.
    [add-membership	*ip-add-membership*]	; Join a multicast group.
    [drop-membership	*ip-drop-membership*]	; Leave a multicast group.
    )
  )
