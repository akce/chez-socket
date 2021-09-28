#! /usr/bin/env -S chez-scheme --debug-on-exception --script

;; Simple Echo Client taken from:
;; https://srfi.schemers.org/srfi-106/srfi-106.html
;; Copyright (C) Takashi Kato (2012). All Rights Reserved.

;; Adapted to run via chez-socket by Jerry 2019-2021 Unlicensed.
;; Also added a number of debug prints.

(import
  (rnrs)
  (srfi :106 socket))

(define host "localhost")
(define port "5000")
(define msg "hello\r\n")

(define client-socket (make-client-socket host port
                                          (address-family inet)
                                          (socket-domain stream)
                                          (address-info v4mapped addrconfig)
                                          (ip-protocol ip)))
(display "client: connected to ")(display host)(display ":")(display port)(newline)

(define sent-count (socket-send client-socket (string->utf8 msg)))
(display "client: sent ")(display sent-count)(display " bytes")(newline)

(define back-message (socket-recv client-socket (string-length msg)))
(display "client: received '")(display (utf8->string back-message))(display "'")(newline)
