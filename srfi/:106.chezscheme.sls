;; Chez-sockets: SRFI-106 basic sockets layer.
;;
;; Written by Jerry 2019-2021
;; SPDX-License-Identifier: Unlicense
;;
;; Note that this library mostly serves to restrict (socket impl) to the basic sockets interface.

(library (srfi :106)
  (export)
  (import
   (chezscheme)
   (srfi :106 socket))
  (export
    (import (srfi :106 socket)))
  )
