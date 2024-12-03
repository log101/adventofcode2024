(setq mul "mul([[:digit:]]+,[[:digit:]]+)")

(setq mul-or-do "\\(mul([[:digit:]]+,[[:digit:]]+)\\|do()\\|don't()\\)")

(setq mul-grouped "mul(\\([[:digit:]]+\\),\\([[:digit:]]+\\))")

(setq invalid-muls "don't()[[:ascii:]]*?do()")

(defun extract-mul ()
  (with-current-buffer "data.txt"
    (goto-char 1)
    (while (re-search-forward mul nil t)
      (let ((match (concat (match-string 0) "\n")))
	(append-to-file match nil "muls.txt")))))

(defun extract-muls-dos ()
  (with-current-buffer "data.txt"
    (goto-char 1)
    (while (re-search-forward mul-or-do nil t)
      (let ((match (concat (match-string 0) "\n")))
	(append-to-file match nil "muls_with_dos.txt")))))

(defun calculate-muls ()
  (with-current-buffer "muls_with_dos.txt"
    (goto-char 1)
    (while (re-search-forward mul-grouped nil t)
      (let* ((a (match-string 1))
	     (b (match-string 2))
	     (res (* (string-to-number a) (string-to-number b)))
	     (res-string (concat (number-to-string res) " ")))
	(append-to-file res-string nil "gun3_muls_happy_end.txt")))))
