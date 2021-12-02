(ql:quickload :str)
(ql:quickload :trivia)
(use-package :str)
(use-package :trivia)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (split " " line :omit-nulls T))))

(defparameter *small* (get-file "small.txt"))
(defparameter *big* (get-file "input.txt"))

(defun rec-solve1 (input horizontal vertical)
  (match (car input)
    ('()
      (* horizontal vertical))
    ((list "forward" cmd)
     (rec-solve1 (cdr input) (+ horizontal (parse-integer cmd)) vertical))
    ((list "up" cmd)
     (rec-solve1 (cdr input) horizontal (- vertical (parse-integer cmd))))
    ((list "down" cmd)
     (rec-solve1 (cdr input) horizontal (+ vertical (parse-integer cmd))))))

(defun part1 (input)
  (print (rec-solve1 input 0 0)))

(defun rec-solve2 (input horizontal vertical aim)
  (match (car input)
    ('()
      (* horizontal vertical))
    ((list "forward" cmd)
     (rec-solve2 (cdr input) (+ horizontal (parse-integer cmd)) (+ vertical (* aim (parse-integer cmd))) aim))
    ((list "up" cmd)
     (rec-solve2 (cdr input) horizontal vertical (- aim (parse-integer cmd))))
    ((list "down" cmd)
     (rec-solve2 (cdr input) horizontal vertical (+ aim (parse-integer cmd))))))

(defun part2 (input)
  (print (rec-solve2 input 0 0 0)))
