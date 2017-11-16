;;; odds-test.el --- Tests for odds.el.

;;; Commentary:

;; Tests for the odds.el.

;;; Code:

(ert-deftest odd-open-websites-test ()
  "Visit websites, rendering them."
  (with-sandbox
   (with-temp-buffer
     ;; (w3m-retrieve address-538)
     ;; (w3m-buffer)
     )))

(ert-deftest odds-compare-test ()
  "Compare defenses."
  (let ((def1 '("Hobos" 20.5 23.25))
	(def2 '("Watsons" 18.0 30.0)))
    (should
     (equal (odds-compare def1 def2) def2))))

(ert-deftest odds-teams-test ()
  "Process defenses."
  (let ((s1 '(("Hobos" "44" nil)
	      ("Watsons" nil "-7")
	      ("Rogers" nil "PK")
	      ("Cant" "50" nil)))
	(s2 '(("Cant" nil)
	      ("Rogers" "-1")
	      ("Hobos" nil)
	      ("Watsons" "-6.5"))))
    ))

(ert-deftest odds-winner-test ()
  "Compare defenses."
  (let ((team1 '("Hobos" "44" nil))
	(team2 '("Watsons" nil "-7"))
	(team3 '("Rogers" nil "PK"))
	(team4 '("Cant" "50" nil)))
    (should (equal (odds-winner team1 team2) team2))
    (should (equal (odds-winner team3 team4) team3))))

(provide 'odds-test)
;;; odds-test.el ends here
