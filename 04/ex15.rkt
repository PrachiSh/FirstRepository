#lang racket
(define-struct student (id name major))
;;A Student is (make-student number string string)
;;It represents id, name and major of a student.
;;Interpretation:
;;  id = Identication of student
;;  name = name of the studenr
;;  major = major of student