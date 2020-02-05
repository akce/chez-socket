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
    )
  (import
    (chezscheme)
    (socket impl)
    (only (socket c) socket-fd))
  ;; Export the basic interface, except for things overridden here.
  (export
    (import (except (socket basic))))
  )
