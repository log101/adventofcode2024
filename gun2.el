(load "~/repos/adventofcode/gun2_data.el")
(setq max-lisp-eval-depth 10000)

(defun increasing (reports)
  (if (not reports)
      1
    (if (= (length reports) 1)
	1
      (let ((delta (- (cadr reports) (car reports))))
	(and (and (<= delta 3)
		  (>= delta 1))
	     (increasing (cdr reports)))))))

(defun decreasing (reports)
  (if (not reports)
      1
    (if (= (length reports) 1)
	1
      (let ((delta (- (car reports) (cadr reports))))
	(and (and (<= delta 3)
		  (>= delta 1))
	     (decreasing (cdr reports)))))))

(defun tolerate-increasing (reports)
  (if (not reports)
      1
    (if (= (length reports) 1)
	1
      (let ((delta (- (cadr reports)
		      (car reports))))
	(if (and (<= delta 3)
		 (>= delta 1))
	    (and (car reports) (tolerate-increasing (cdr reports)))
	  (increasing
	   (cons (car reports)
		 (cddr reports))))))))

(defun tolerate-increasing-acc (reports acc)
  (if (not reports)
      (increasing (reverse acc))
    (if (= (length reports) 1)
	(increasing (reverse (cons (car reports) acc)))
      (let ((delta (- (cadr reports)
		      (car reports))))
	(if (and (<= delta 3)
		 (>= delta 1))
	    (tolerate-increasing-acc (cdr reports) (cons (car reports) acc))
	  (or (increasing (append (reverse acc) (cdr reports)))
	      (increasing (append (reverse (cons (car reports) acc)) (cddr reports)))))))))

(defun tolerate-decreasing-acc (reports acc)
  (if (not reports)
      (decreasing (reverse acc))
    (if (= (length reports) 1)
	(decreasing (reverse (cons (car reports) acc)))
      (let ((delta (- (car reports)
		      (cadr reports))))
	(if (and (<= delta 3)
		 (>= delta 1))
	    (tolerate-decreasing-acc (cdr reports) (cons (car reports) acc))
	  (or (decreasing (append (reverse acc) (cdr reports)))
	      (decreasing (append (reverse (cons (car reports) acc)) (cddr reports)))))))))


(defun tolerate-decreasing (reports)
  (if (not reports)
      1
    (if (= (length reports) 1)
	1
      (let ((delta (- (car reports)
		      (cadr reports))))
	(if (and (<= delta 3)
		 (>= delta 1))
	    (and (car reports) (tolerate-decreasing (cdr reports)))
	  (decreasing
	   (cons (car reports)
		 (cddr reports))))))))

(defun list-string-to-number (l)
  (if (not l)
      '()
    (cons (string-to-number (car l)) (list-string-to-number (cdr l)))))

(defun list-list-string-to-number (ll)
  (if (not ll)
      '()
    (cons (list-string-to-number (split-string (car ll) " ")) (list-list-string-to-number (cdr ll)))))

(defun count-safe (reports)
  (if (not reports)
      0
    (if (or (tolerate-increasing-acc (car reports) '())
	    (tolerate-decreasing-acc (car reports) '()))
	(+ 1 (count-safe (cdr reports)))
      (count-safe (cdr reports)))))

(setq data-lines (list-list-string-to-number (split-string data "\n")))

(setq data-lines-sample (list-list-string-to-number (split-string data-sample "\n")))

(count-safe data-lines)

(tolerate-increasing-acc '(100 1 2 3) '())
