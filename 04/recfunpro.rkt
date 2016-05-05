#lang racket
(require 2htdp/image)
(define (rfc w p)(rectangle w (* w p) "solid" "blue"))

(rfc 7 10)