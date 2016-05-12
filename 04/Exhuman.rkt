#lang racket
(require 2htdp/image)
(beside (rectangle 6  6 "solid" "white")
(circle 5 "solid" "blue"))
(beside/align "top" (rectangle 6 7  "solid" "blue")
(above/align "left" (rectangle 16 7 "solid" "blue")
(rectangle 10  13 "solid" "blue")))
(beside (rectangle 6 8  "solid" "white")
        (rectangle 4 12  "solid" "blue")
        (rectangle 1 10  "solid" "white")
        (rectangle 4 12  "solid" "blue"))
              

