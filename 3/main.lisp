(ql:quickload :str)
(ql:quickload :trivia)
(ql:quickload :fiveam)
(use-package :str)
(use-package :trivia)
(use-package :fiveam)

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *small* (get-file "input_small.txt"))
(defparameter *big* (get-file "input.txt"))

(defun get-most-frequent (rest gamma epsilon)
  (cond
    ((string-equal (car rest) "")
     (list (parse-integer (reverse gamma) :radix 2) (parse-integer (reverse epsilon) :radix 2)))
    (T (let* ((prefixes (map 'list #'s-first rest))
           (suffixes (map 'list #'s-rest rest))
           (ones (str:count-substring "1" (apply #'str:concat prefixes)))
           (zeros (- (length prefixes) ones)))
      (if (< ones zeros)
          (get-most-frequent suffixes (str:concat "0" gamma) (str:concat "1" epsilon))
          (get-most-frequent suffixes (str:concat "1" gamma) (str:concat "0" epsilon)))))))

(defun part1 (input)
  (let ((result (get-most-frequent input "" "")))
    (* (car result) (cadr result))))

(defun part2 (input)
  0)


(test tests
  "Test part 1 with the sample input"
  (is (= 198 (part1 *small*)))
  (is (= 4138664 (part1 *big*)))
  (is (= 230 (part2 *small*)))
  (is (= 4273224 (part2 *big*))))

(run!)
