;; Gidilecek nokta buffer içerisinde mi kontrol et
;; t: İçinde ise nil dön
;; Gidilecek noktada # işareti var mı kontrol et
;; Varsa 90 derece sağa dön
;; İlerle
;; Üzerinde bulunduğun noktayı X karakteri ile işaretle
;; Başa dön

(add-to-list 'load-path ".")
(load "./data.el")

(defun new-direction (direction)
  (cond ((string= direction "n") "e")
	((string= direction "e") "s")
	((string= direction "s") "w")
	((string= direction "w") "n")
	(t nil)))

(defun replace-dot ()
  (delete-forward-char 1)
  (insert "X")
  (goto-char (- (point) 1)))

(defun move-south ()
  (let ((delta (- (point) (line-beginning-position))))
    (if (not (= (forward-line 1) 1))
	(goto-char (+ (point) delta))
      (throw 'end t))))

(defun move-north ()
  (let ((delta (- (point) (line-beginning-position))))
    (if (not (= (forward-line -1) -1))
	(goto-char (+ (point) delta))
      (throw 'end t))))

(defun move-east ()
  (interactive)
  (if (not (eolp))
      (goto-char (+ (point) 1))
    (throw 'end t)))

(defun move-west ()
  (if (not (bolp))
      (goto-char (- (point) 1))
    (throw 'end t)))

(setopt max-lisp-eval-depth 10000)

(setq patrol-direction "n")

(defun patrol2 ()
  (interactive)
  (setq patrol-direction "n")
  (catch 'end 
    (while (or (char-equal (char-after) ?#) (char-equal (char-after) ?.) (char-equal (char-after) ?X))
      (patrol-inc))))

(defun patrol (direction)
  (interactive "sDirection:")
  (cond ((string= direction "n")
	 (move-north)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-south)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))	  
	((string= direction "e")
	 (move-east)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-west)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	((string= direction "s")
	 (move-south)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-north)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	((string= direction "w")
	 (move-west)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-east)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	(t nil)))

(defun patrol-inc ()
  (cond ((string= patrol-direction "n")
	 (move-north)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-south)
	       (setq patrol-direction (new-direction patrol-direction)))
	     (replace-dot)))
	((string= patrol-direction "e")
	 (move-east)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-west)
	       (setq patrol-direction (new-direction patrol-direction)))
	   (if (char-equal (char-after) ?\n)
	       (throw 'end t)
	     (replace-dot))))
	((string= patrol-direction "s")
	 (move-south)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-north)
	       (setq patrol-direction (new-direction patrol-direction)))
	     (replace-dot)))
	((string= patrol-direction "w")
	 (move-west)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-east)
	       (setq patrol-direction (new-direction patrol-direction)))
	     (replace-dot)))
	(t nil)))

(defun move-by-position (direction)
  (cond ((string= direction "n")
	 (move-north))	 
	((string= direction "e")
	 (move-east))
	((string= direction "s")
	 (move-south))
	((string= direction "w")
	 (move-west))))

(defun check-point (p)
  (and (char-equal (char-after) ?X)
       (cond ((string= direction "n")
	      (save-excursion
		(move-east)
		(char-equal (char-after) ?X)))
	     ((string= direction "e")
	      (save-excursion
		(move-south)
		(char-equal (char-after) ?X)))
	     ((string= direction "s")
	      (save-excursion
		(move-west)
		(char-equal (char-after) ?X)))
	     ((string= direction "w")
	      (save-excursion
		(move-north)
		(char-equal (char-after) ?X))))))

(defun cont-point (p)
  (cond ((string= direction "n")
	 (save-excursion
	   (move-east)
	   (point)))
	((string= direction "e")
	 (save-excursion
	   (move-south)
	   (point)))
	((string= direction "s")
	 (save-excursion
	   (move-west)
	   (point)))
	((string= direction "w")
	 (save-excursion
	   (move-north)
	   (point)))))

(setq counter 0)

(print counter)

(setq lines-to-check (list
		      (cons 0 (cons "e" '()))
		      (cons 2 (cons "e" '()))
		      (cons 0 (cons "e" '()))
		      (cons 0 (cons "e" '()))
		      (cons 0 (cons "e" '()))
		      (cons 0 (cons "e" '()))))

(dotimes (number 11)
  (cons (cons 0 (cons "e" '())) lines-to-check))

(print (length lines-to-check))

(defun loops ()
  (interactive)
  (let* ((line-tuple (pop lines-to-check))
	 (my-line (car line-tuple))
	 (direction (cadr line-tuple)))
    (goto-char my-line)
    (if (and (string= direction "n") (char-equal (char-after) ?#))
	(setq counter (+ 1 counter))
      (catch 'end
	(while (or (not (char-equal (char-after) ?#)) (char-equal (char-after) ?.) (char-equal (char-after) ?X))
	  (if (check-point (point))
	      (setq data-lines (cons '((cont-point (point)) (new-direction direction)) data-lines)))
	  (move-by-position direction)))
      (if lines-to-check
	  (loops)))))
      
      
  
(defun find-loop (p direction)
  (cond ((string= direction "n")
	 (move-north)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-south)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))	  
	((string= direction "e")
	 (move-east)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-west)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	((string= direction "s")
	 (move-south)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-north)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	((string= direction "w")
	 (move-west)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (move-east)
	       (patrol (new-direction direction)))
	   (progn
	     (replace-dot)
	     (patrol direction))))
	(t nil)))
