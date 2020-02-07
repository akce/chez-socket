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
    (import (except (socket basic))))

  (define-enum socket-opt-level
    [socket	*sol-socket*]		; getsockopt(2) SOL_SOCKET.
    [ip		*ipproto-ip*]		; ip(7) IPPROTO_IP.
    [tcp	*ipproto-tcp*]		; tcp(7) IPPROTO_TCP.
    [udp	*ipproto-udp*])		; upd(7) IPPROTO_UDP.

  (define-enum socket-opt
    [acceptconn	*so-acceptconn*]
    [broadcast	*so-broadcast*]
    [dontroute	*so-dontroute*]
    [error	*so-error*]
    [keepalive	*so-keepalive*]
    [linger	*so-linger*]
    [oobinline	*so-oobinline*]
    [protocol	*so-protocol*]
    [reuseaddr  *so-reuseaddr*]
    [type	*so-type*])
  )
