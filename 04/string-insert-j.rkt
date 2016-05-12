;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-insert-j) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (string-insert-j str i)
  (string-append (substring str 0 i) "_" (substring str i (string-length str ))))
  (string-insert-j "prachisharma" 6)