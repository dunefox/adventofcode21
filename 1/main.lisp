(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-integer line))))

(defvar *in2* (get-file "small_input"))
(defvar *in* (get-file "input"))

(defun part1 (lines)
  "Day 1 Part 1 AoC21"
  (let* ((sequence (loop for el in lines collect el))
         (pairs (mapcar #'list sequence (cdr sequence)))
         (diffs (map 'list (lambda (pair) (- (cadr pair) (car pair))) pairs))
         (count (loop for i in diffs when (> i 0) sum 1)))
    (print count)))

(defun part2 (lines)
  (let* ((sums (loop for (a b c) in (mapcar 'list lines (cdr lines) (cddr lines)) collect (+ a b c)))
        (diffs (loop for (a b) in (mapcar 'list sums (cdr sums)) when (> (- b a) 0) sum 1)))
    (print diffs)))

(part1 *in*)
(part2 *in*)
