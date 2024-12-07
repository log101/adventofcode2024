(add-to-list 'load-path ".")
(load "./data.el")

(setq data-lines (split-string data "\n"))

;; Concat numbers as string
(defun concat-number (a b)
  (string-to-number
   (concat (if a (number-to-string a) nil)
	   (if b (number-to-string b) nil))))

;; Check if ref value can be calculated from given number list
(defun calibrate (number-list)
  (let* ((splitted (split-string number-list ":"))
	 (ref (string-to-number (car splitted)))
	 (numbers (mapcar 'string-to-number (split-string (cadr splitted) " " t))))
    (defun calibrate-inner (number-list acc operators)
      (if (not number-list)
	  (if (= acc ref) t nil)
	(or (calibrate-inner (cdr number-list) (+ (car number-list) acc) (concat "+ " operators))
	    (calibrate-inner (cdr number-list) (* (car number-list) (if (= acc 0) 1 acc)) (concat "* " operators))
	    (calibrate-inner (cdr number-list) (concat-number (if (= acc 0) nil acc) (car number-list)) (concat "|| " operators)))))
    (if (calibrate-inner numbers 0 "") ref 0)))


(setq result (seq-reduce #'+ (mapcar 'calibrate data-lines) 0))

