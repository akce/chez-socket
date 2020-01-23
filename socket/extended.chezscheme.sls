;; Extended Socket interface.
;;
;; Interface extensions over SRFI-106.
;;
;; Written by Akce 2020.
;; SPDX-License-Identifier: Unlicense

(library
  (socket extended)
  (export
    socket-fd)
  (import
    (chezscheme)
    (only (socket c) socket-fd))
  (export
    (import (socket basic))))
