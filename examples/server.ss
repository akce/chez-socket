#! /usr/bin/chez-scheme --script

;; Simple Echo Server taken from:
;; https://srfi.schemers.org/srfi-106/srfi-106.html
;; Copyright (C) Takashi Kato (2012). All Rights Reserved.

;; Adapted to run via chez-socket by Akce 2019, 2020 Unlicensed.
;; Also added a number of debug prints.

(import
  (rnrs)
  (socket basic))

(define port "5000")

(define (server-run srv)
  (define (get-line-from-binary-port bin)
    (utf8->string
     (call-with-bytevector-output-port
      (lambda (out)
        (let loop ((b (get-u8 bin)))
          (display "server: recv-char ")(display b)(newline)
          (case b
            ((#!eof)
             (display "server: EOF")(newline)
             #t)
            ((#xA)                ;; newline (\n)
             (display "server: returning")(newline)
             #t)
            ((#xD)
             (loop (get-u8 bin)))       ;; carriage return (\r)
            (else
              (put-u8 out b)
              (loop (get-u8 bin)))))))))
  (display "server: listening on port ")(display port)(newline)
  (display srv)(newline)
  (call-with-socket (socket-accept srv)
    (lambda (sock)
      (display "server: accepted peer connection")(newline)
      (let ((in (socket-input-port sock))
            (out (socket-output-port sock)))
        (let loop ((r (get-line-from-binary-port in)))
          (display "server: read-line '")(display r)(display "'")(newline)
          (unless (string=? r "")
            (put-bytevector out (string->utf8 (string-append r "\r\n")))
            (flush-output-port out)
            (loop (get-line-from-binary-port in))))))))

(server-run (make-server-socket port))
