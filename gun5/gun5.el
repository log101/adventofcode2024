(add-to-list 'load-path ".")
(load "./data.el")

;; Check which updates are in the right order
(defun check-rule (line)
  (if (not line)
      t
    (if (= (length line) 1)
	t
      (let* ((a (car line))
	     (b (cadr line))
	     (with-colon (concat a "|" b)))
	(and (string-match-p with-colon rules)
	     (check-rule (cdr line)))))))

;; Find middle element of lines and convert to number
(defun find-middle-number (line)
  (string-to-number (nth (/ (length line) 2) line)))

;; Find pages that are in correct order and add middle number
(defun middle-of-ordered-lines (lines)
  (if (not lines)
      0
    (let ((splitted (split-string (car lines) ",")))
    (if (check-rule splitted)
        (+ (find-middle-number splitted) (middle-of-ordered-lines (cdr lines)))
      (middle-of-ordered-lines (cdr lines))))))

;; Return corrected lines
(defun correct-line (line)
  (if (not line)
      '()
    (if (= (length line) 1)
	(cons (car line) '())
      (let* ((a (car line))
	     (b (cadr line))
	     (with-colon (concat a "|" b)))
	(if (string-match with-colon rules)
	    (cons a (correct-line (cdr line)))
	  (cons b (correct-line (cons a (cddr line)))))))))

(defun definitely-correct-line (line)
  (if (check-rule line)
      line
    (definitely-correct-line (correct-line line))))
    

(defun middle-of-non-ordered-lines (lines)
  (if (not lines)
      0
    (let ((splitted (split-string (car lines) ",")))
    (if (check-rule splitted)
	(middle-of-non-ordered-lines (cdr lines))
      (+ (find-middle-number (definitely-correct-line splitted)) (middle-of-non-ordered-lines (cdr lines)))))))

(middle-of-ordered-lines pages-lines)

(middle-of-non-ordered-lines pages-lines)
