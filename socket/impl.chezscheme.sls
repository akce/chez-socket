;; Mostly taken from "Implementation dependent layer -> For others" section:
;; https://srfi.schemers.org/srfi-106/srfi-106.html

;; Only the library name has been changed. (Akce 2019, Unlicensed).

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

(library (socket impl)
  (export
   make-client-socket make-server-socket
   socket? socket-port call-with-socket
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

  (define-syntax define-unsupported
    (syntax-rules ()
      ((_ (name))
       (define (name . _)
         (raise
          (condition (make-implementation-restriction-violation)
                     (make-who-condition 'name)
                     (make-message-condition
                      "This SRFI is not supported on this implementation")))))
      ((_ name)
       (define name #f))))

  (define-unsupported (socket?           ))
  (define-unsupported (socket-port       ))
  (define-unsupported (call-with-socket  ))
  (define-unsupported (socket-send       ))
  (define-unsupported (socket-recv       ))

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
  )
