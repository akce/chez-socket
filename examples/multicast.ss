#! /usr/bin/env -S chez-scheme --debug-on-exception --script

;; Multicast example.
;; Written by Jerry 2020-2021.
;; SPDX-License-Identifier: Unlicense

;; A multicast server is much like a regular UDP server, only the sendto address is special.
;; See here for info on IP multicast addresses:
;;   https://en.wikipedia.org/wiki/Multicast_address
;; ie, for link-local ipv6 multicast: ff02::/16 + group-id.
;; For ipv4, it's 224.0.0.0/24 but limited as there exist a number of
;; well-known addresses that shrink availability.

(import
  (rnrs)
  (socket))

(define producer-run
  (lambda (node service afam)
    (display "multicast sending to ")(display node)(display ":")(display service)(newline)
    ;; connect(2) socket to destination address. This allows socket-send to work.
    ;; (Otherwise there would need to be a separate sendto(2) operation.)
    (call-with-socket (connect-client-socket
                        node service afam
                        *sock-dgram*
                        (socket-merge-flags *ai-addrconfig* *ai-numericserv*)
                        *ipproto-udp*)
      (lambda (sock)
        (let ([port (transcoded-port (socket-output-port sock) (native-transcoder))]
              [1-sec (make-time 'time-duration 0 1)]
              [msg '(display "scheme hello")])
          (let loop ()
            (display "multicasting msg: ")(display msg)(newline)
            (write msg port)
            (flush-output-port port)
            (sleep 1-sec)
            (loop)))))))

(define consumer-run
  (lambda (node service afam)
    (display "multicast recv from ")(display node)(display ":")(display service)(newline)
    (call-with-socket (connect-server-socket
                        node service afam
                        *sock-dgram*
                        (socket-merge-flags *ai-addrconfig* *ai-numericserv*)
                        *ipproto-udp*)
      (lambda (sock)
        (display sock)(newline)
        (display "mcast-add-membership: ")(display (mcast-add-membership sock node))(newline)
        (let ([port (transcoded-port (socket-input-port sock) (native-transcoder))])
          (let loop ()
            (define msg (read port))
            (display "received msg: ")(display msg)(newline)
            (loop)))))))

(define help
  (lambda ()
    (display "usage: ")(display (car (command-line)))(display " <c4|p4|c6|p6>")(newline)
    (display "where:")(newline)
    (display " * c4 is an IPv4 consumer")(newline)
    (display " * p4 is an IPv4 producer")(newline)
    (display " * c6 is an IPv6 consumer")(newline)
    (display " * p6 is an IPv6 producer")(newline)
    (newline)
    (exit 1)))

;; RFC4291: 2.7 Multicast Addresses.
;; https://tools.ietf.org/html/rfc4291
;; prefix ff12 = multicast(ff) | transient(1) | site-local(2)
(define node/ipv4 "224.0.0.49")
#;(define node/ipv6 "ff12::50cc:49")
(define node/ipv6 "ff01::1")
(define service "49000")
(let ([args (command-line-arguments)])
  (cond
    [(null? args)
     (help)]
    [else
      (case (string->symbol (car args))
        [(p6) (producer-run node/ipv6 service *af-inet6*)]
        [(c6) (consumer-run node/ipv6 service *af-inet6*)]
        [(p4) (producer-run node/ipv4 service *af-inet*)]
        [(c4) (consumer-run node/ipv4 service *af-inet*)]
        [else
          (help)])]))
