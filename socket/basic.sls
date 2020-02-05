;; Chez-sockets: SRFI-106 basic sockets layer.
;;
;; Written by Akce 2019-2020, Unlicensed.
;;
;; The export list and socket-port is taken from "Interface layer" section:
;; https://srfi.schemers.org/srfi-106/srfi-106.html
;;
;; Copyright (C) Takashi Kato (2012). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(library (socket basic)
  (export
   make-client-socket make-server-socket
   socket?
   (rename (socket-port socket-input-port)
           (socket-port socket-output-port))
   call-with-socket
   socket-merge-flags socket-purge-flags
   socket-accept socket-send socket-recv socket-shutdown socket-close
   *af-unspec* *af-inet* *af-inet6*
   *sock-stream* *sock-dgram*
   *ai-canonname* *ai-numerichost*
   *ai-v4mapped* *ai-all* *ai-addrconfig*
   *ipproto-ip* *ipproto-tcp* *ipproto-udp*
   *shut-rd* *shut-wr* *shut-rdwr*
   address-family socket-domain address-info
   ip-protocol message-type shutdown-method)
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
    [wait-all	*msg-waitall*])

  (define (socket-port socket)
    (define (read! bv start count)
      (let ((r (socket-recv socket count)))
        (bytevector-copy! r 0 bv start (bytevector-length r))
        (bytevector-length r)))
    (define (write! bv start count)
      (let ((buf (make-bytevector count)))
        (bytevector-copy! bv start buf 0 count)
        (socket-send socket buf)))
    (make-custom-binary-input/output-port
              "socket-port" read! write! #f #f #f)))
