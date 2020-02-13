;; Chez-socket: Common scheme implementation layer.
;;
;; Written by Akce 2019-2020 Unlicensed.
;;
;; [proc] 'socket-port' is taken from "Interface layer" section:
;; https://srfi.schemers.org/srfi-106/srfi-106.html
;; Copyright (C) Takashi Kato (2012). All Rights Reserved.

(library (socket impl)
  (export
    make-client-socket make-server-socket
    call-with-socket
    (rename
     (socket-port socket-input-port)
     (socket-port socket-output-port)
     (bitwise-ior socket-merge-flags)
     (bitwise-xor socket-purge-flags))
    address-family ip-protocol message-type socket-domain shutdown-method socket-port
    define-bits
    define-enum)
  (import
   (chezscheme)
   (socket c))
  (export (import (socket c)))

  ;; [syntax] define-enum: generates a syntax transformer that evaluates the value of an enum at compile time.
  ;; eg, using trace-define-syntax:
  ;; > (define-enum e [a 1] [b 2] [c 3])
  ;; |(define-enum (define-enum e (a 1) (b 2) (c 3)))
  ;; |(define-syntax e
  ;;    (lambda (x)
  ;;      (syntax-case x ()
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'a)) #'1]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'b)) #'2]
  ;;        [(_ v) (eq? (datum v) (syntax->datum #'c)) #'3])))
  ;; > (e a)
  ;; 1
  ;; > (e b)
  ;; 2
  ;; > (e c)
  ;; 3
  ;; > (e d)
  ;; Exception: invalid syntax (e d)
  ;; Type (debug) to enter the debugger.
  ;; >
  (define-syntax define-enum
    (syntax-rules ()
      [(_ group (var* val*) ...)
       (define-syntax group
         (lambda (x)
           (syntax-case x ()
             [(_ v)
              (eq? (datum v) (syntax->datum #'var*))
              #'val*] ...)))]))

  ;; [syntax] define-bits: creates a syntax generator that bitwise ORs provided flags at compile time.
  ;;
  ;; eg, (with trace-define-syntax)
  ;;
  ;; > (define-bits e [a 1] [c 4] [d 8])
  ;; |(define-bits (define-bits e (a 1) (c 4) (d 8)))
  ;; |(define-syntax e
  ;;    (lambda (x)
  ;;      (define (sym->bits sym)
  ;;        (case sym [a 1] [c 4] [d 8] [else (error 'e "invalid value" sym)]))
  ;;      (syntax-case x ()
  ;;        [(_ v ...)
  ;;         (with-syntax ([bits (apply
  ;;                               bitwise-ior
  ;;                               (map sym->bits (syntax->datum #'(v ...))))])
  ;;           #'bits)])))
  ;; > (e a)
  ;; 1
  ;; > (e c)
  ;; 4
  ;; > (e d)
  ;; 8
  ;; > (e a d)
  ;; 9
  ;; > (e a c d)
  ;; 13
  ;; > (e x)
  ;; Exception in e: invalid value with irritant x
  ;; Type (debug) to enter the debugger.
  (define-syntax define-bits
    (syntax-rules ()
      [(_ group (var* val*) ...)
       (define-syntax group
         (lambda (x)
           (define (sym->bits sym)
             (case sym
               [var* val*]
               ...
               [else
                 (error 'group "invalid value" sym)]))
           ;; escape subsequent ellipsis (...) from enclosing syntax-rules.
           (...
             (syntax-case x ()
               [(_ v ...)
                (with-syntax ([bits (apply bitwise-ior (map sym->bits (syntax->datum #'(v ...))))])
                  #'bits)]))))]))

  (define-enum address-family
    [inet	*af-inet*]
    [inet6	*af-inet6*]
    [unspec	*af-unspec*])

  (define-enum ip-protocol
    [ip		*ipproto-ip*]
    [tcp	*ipproto-tcp*]
    [udp	*ipproto-udp*])

  (define-bits message-type
    [none	0]
    [peek	*msg-peek*]
    [oob	*msg-oob*]
    [wait-all	*msg-waitall*])

  (define-enum socket-domain
    [stream	*sock-stream*]
    [datagram	*sock-dgram*])

  (define-syntax shutdown-method
    (lambda (x)
      (syntax-case x ()
        [(_ v)
         (eq? (datum v) 'read)
         #'*shut-rd*]
        [(_ v)
         (eq? (datum v) 'write)
         #'*shut-wr*]
        [(_ v1 v2)
         (let ([d1 (datum v1)]
               [d2 (datum v2)])
           (and
             (or (eq? d1 'read) (eq? d1 'write))
             (or (eq? d2 'read) (eq? d2 'write))
             (not (eq? d1 d2))))
         #'*shut-rdwr*])))

  (define make-client-socket
    (case-lambda
     [(node service)
      (make-client-socket node service *af-inet*)]
     [(node service ai-family)
      (make-client-socket node service ai-family *sock-stream*)]
     [(node service ai-family ai-socktype)
      (make-client-socket node service ai-family ai-socktype (bitwise-ior *ai-v4mapped* *ai-addrconfig*))]
     [(node service ai-family ai-socktype ai-flags)
      (make-client-socket node service ai-family ai-socktype ai-flags *ipproto-ip*)]
     [(node service ai-family ai-socktype ai-flags ai-protocol)
      (connect-client-socket node service ai-family ai-socktype ai-flags ai-protocol)]))

  (define make-server-socket
    (case-lambda
      [(service)
       (make-server-socket service *af-inet*)]
      [(service ai-family)
       (make-server-socket service ai-family *sock-stream*)]
      [(service ai-family ai-socktype)
       (make-server-socket service ai-family ai-socktype *ipproto-ip*)]
      [(service ai-family ai-socktype ai-protocol)
       (connect-server-socket #f service ai-family ai-socktype (bitwise-ior *ai-v4mapped* *ai-addrconfig*) ai-protocol)]))

  ;; call-with-socket is adapted from the call-with-port example found here:
  ;; https://scheme.com/tspl4/control.html#defn:call-with-port
  (define call-with-socket
    (lambda (conn proc)
      (call-with-values (lambda () (proc conn))
        (case-lambda
          [(val) (socket-close conn) val]
          [val* (socket-close conn) (apply values val*)]))))

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
