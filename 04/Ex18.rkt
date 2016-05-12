#lang racket
(require 2htdp/image)
(define (rec_seq n ) (rectangle (expt 2 n) (expt 2 (+ n 1)) "solid" "blue"))
(rec_seq 2)
(rec_seq 4)
(rec_seq 5)
(rec_seq 6)
(rec_seq 7)

;;(define (rec_seq n ) (rectangle (expt 2 n) (expt 2 (+ n 1)) "solid" "blue"))
;;rec_sq: Real -> image
;;Given: sequence of n element
;;Return: image of rectangle according to sequence of n element
