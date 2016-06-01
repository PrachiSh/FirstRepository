;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exm0201) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (edit ed ke)
  (cond
    [(string=? ke "\b")
    (funcb ed)]
    [else (funcnotb ed ke)]))
(define (funcnotb ed ke)
  (string-append ed ke))
(define (funcb ed) 
  (substring ed 0 (- (string-length ed) 1)))
(edit "prachisharma" "\b")