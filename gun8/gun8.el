;; 8. Gün
;; Antenler: [a-zA-Z0-9]
;; 1. Antenleri sırayla ara
;; 2. Konumlarını kaydet
;; 3. Aynı hizada olanları bul
;; 4. Antinodların yerini hesapla
;; '(<char> '(<row, column>))
;; '("a" '(12 14))
(setq antenna-regexp "[[:alnum:]]")

(defun extract-antennas (buf)
  (let ((antennas (make-hash-table)))
    (with-current-buffer buf
      (goto-char (point-min))
      (defun extract-antennas-inner ()
	(while (re-search-forward antenna-regexp nil t)
	  (let* ((matched-char     (string-to-char (match-string 0)))
		 (matched-position (list (line-number-at-pos) (- (current-column) 1)))
		 (val (gethash matched-char antennas)))
	    (puthash matched-char (cons matched-position val) antennas)))
	antennas)
      (extract-antennas-inner))))

;; Aynı hizada olan antenleri bul
;; Aynı satırda, sütunda, çapraz (satır ve sütunlarının farkı eşit)
;; '('(<char> '(<row, column>)) '(<char> '(<row, column>)))
;; Aynı hizada olanların arasındaki farkı ilk antenden çıkarıp, ikincisine eklemek gerekiyor
(require 'subr-x)
(setq antenna-freqs (hash-table-keys sample-antenna-locations))

;; comp format: (<type: d || l || c> loc1 loc2 delta)
(defun compare-locations (loc1 loc2)
  "Compare locations of a same frequency"
  (let ((line1 (car loc1))
	(col1 (cadr loc1))
	(line2 (car loc2))
	(col2 (cadr loc2)))
    (cond ((= line1 line2) (list "l" loc1 loc2 (list (abs (- line1  line2))
						     (abs (- col1   col2)))))
	  ((= col1  col2)  (list "c" loc1 loc2 (list (abs (- line1  line2))
						     (abs (- col1   col2)))))
	  (t (list "d" loc1 loc2 (list (abs (- line1  line2))
				       (abs (- col1   col2))))))))

;; list: locations list
(defun filter-resonants (list)
  (if (not list)
      nil
    (let ((ref (car list)))
      (nconc
       (mapcar (lambda (loc) (compare-locations ref loc)) (cdr list))
       (filter-resonants (cdr list))))))

(defun fifty-lists1 (right left delta)
  (defun inner (count)
    (if (= count 0)
	nil
      (let ((new-delta (list (* count (car delta)) (* count (cadr delta)))))
	(cons (list (- (car right) (car new-delta)) (+ (cadr right) (cadr new-delta)))
	      (cons (list (+ (car left) (car new-delta)) (- (cadr left) (cadr new-delta))) (inner (- count 1)))))))
  (inner 50))

(defun fifty-lists2 (right left delta)
  (defun inner (count)
    (if (= count 0)
	nil
      (let ((new-delta (list (* count (car delta)) (* count (cadr delta)))))
	(cons (list (- (car left) (car new-delta)) (- (cadr left) (cadr new-delta)))
	      (cons (list (+ (car right) (car new-delta)) (+ (cadr right) (cadr new-delta))) (inner (- count 1)))))))
  (inner 50))

(fifty-lists2 '(1 1) '(2 2) '(1 1))
  
;; comp format: (<type: d || l || c> loc1:'(<row, column>) loc2 delta)
(defun resonant-to-antinodes (resonants)
  (if (not resonants)
      nil
    (let* ((res  (car resonants))
	   (res-type (car res))
	   (loc1 (nth 1 res))
	   (loc2 (nth 2 res))
	   (left (if (< (cadr loc1) (cadr loc2)) loc1 loc2))
	   (right (if (< (cadr loc1) (cadr loc2)) loc2 loc1))
	   (delta (nth 3 res)))
      (nconc (cond ((string= res-type "d")
		    (if (< (car right) (car left))			
			(fifty-lists1 right left delta)
		      (fifty-lists2 right left delta)))
		   (t nil)) (resonant-to-antinodes (cdr resonants))))))

(defun insert-hash-at-points (point-list buf)
  "Insert a # character before every element in the given list of tuples (row, column)."
  (with-current-buffer buf
    (dolist (point point-list)
      (let* ((row (car point))        ; Extract row from the point tuple
             (col (cadr point))        ; Extract column from the point tuple
             (pos (point-at-bol (+ row 1))))  ; Get the position corresponding to row
	(goto-line row)  ; Move the point to the row
	(move-to-column col)
	(delete-char 1)			       ; Move to the column
	(insert "#")))))

(setq antenna-locations (extract-antennas "data.txt"))

(setq antinodes (resonant-to-antinodes
		 (mapcan 'filter-resonants
			 (hash-table-values antenna-locations))))

(setq max-row 51)

(setq max-col 50)

(setq filtered-antinodes (seq-uniq (seq-filter (lambda (antinode)
						 (and (>= (car antinode) 1)
						      (>= (cadr antinode) 0)
						      (< (car antinode) max-row)
						      (< (cadr antinode) max-col)))
					       antinodes)))


(defun solve ()
  (let* ((sample-antenna-locations (extract-antennas "data.txt"))
	 (antinodes (resonant-to-antinodes
		     (mapcan 'filter-resonants
			     (hash-table-values antenna-locations))))
	 (filtered-antinodes (seq-uniq (seq-filter (lambda (antinode)
						     (and (>= (car antinode) 1)
							  (>= (cadr antinode) 0)
							  (< (car antinode) max-row)
							  (< (cadr antinode) max-col)))
						   antinodes))))
    (length filtered-antinodes)))
