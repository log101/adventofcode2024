(previous-logical-line)
;; Gidilecek nokta buffer içerisinde mi kontrol et
;; t: İçinde ise nil dön
;; Gidilecek noktada # işareti var mı kontrol et
;; Varsa 90 derece sağa dön
;; İlerle
;; Üzerinde bulunduğun noktayı X karakteri ile işaretle
;; Başa dön

(defun new-direction (direction)
  (cond ((string= direction "n") "e")
	((string= direction "e") "s")
	((string= direction "s") "w")
	((string= direction "w") "n")
	(t nil)))

(defun patrol (direction)
  (interactive "sDirection:")
  (cond ((string= direction "n")
	 (previous-line)
	 (if (char-equal (char-after) ?#)
	     (progn
	       (next-line)
	       (patrol (new-direction direction)))
	   (progn
	     (delete-forward-char 1)
	     (insert "X")
	     (patrol direction))))	  
	((string= direction "e") "s")
	((string= direction "s") "w")
	((string= direction "w") "n")
	(t nil)))
  
