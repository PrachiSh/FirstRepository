#lang racket
(require rackunit)
(require "extras.rkt")
(define (string-first a)
  (1String a))
(string-first "hello")