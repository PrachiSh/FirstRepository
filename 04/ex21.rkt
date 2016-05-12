#lang racket
(define-struct person-image( first-name last-name age height weight)
((beside (rectangle 6  6) "solid" "white")
(circle (/ height 6) "solid" "blue")
(beside/align "top" (rectangle 6 7  "solid" "blue")
(above/align "left" (rectangle 16 7 "solid" "blue")
(rectangle 10  (/ height 2) "solid" "blue")))
(beside (rectangle 6 8  "solid" "white")
        (rectangle 4 (/ height 3)  "solid" "blue")
        (rectangle 1 10  "solid" "white")
        (rectangle 4 (/ height 3)  "solid" "blue"))))

