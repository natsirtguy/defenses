;;; defenses-test.el --- Tests for defenses.el.

;;; Commentary:

;; Tests for the defenses.el.

;;; Code:

(ert-deftest defenses-compare-test ()
  "Compare defenses."
  (let ((def1 '("Hobos" 20.5 23.25))
	(def2 '("Watsons" 18.0 30.0)))
    (should
     (equal (defenses-compare def1 def2) nil))))

(ert-deftest defenses-teams-test ()
  "Process defenses."
  (let ((s1 '(("Hobos" "44" nil)
	      ("Watsons" nil "-7")
	      ("Rogers" nil "PK")
	      ("Cant" "50" nil)))
	(s2 '(("Cant" "-1")
	      ("Rogers" nil)
	      ("Hobos" nil)
	      ("Watsons" "-6.5"))))
    (should
     (equal
      (defenses-teams s1 s2)
      '(("Rogers" 25.0 25.5) ("Watsons" 18.5 18.75))))))

(ert-deftest defenses-winner-test ()
  "Compare defenses."
  (let ((team1 '("Hobos" "44" nil))
	(team2 '("Watsons" nil "-7"))
	(team3 '("Rogers" nil "PK"))
	(team4 '("Cant" "50" nil)))
    (should (equal (defenses-winner team1 team2) team2))
    (should (equal (defenses-winner team3 team4) team3))))

(ert-deftest defenses-spread-538-test ()
  "Get the spread from 538."
  (let ((winner1 '("Watsons" nil "-7"))
	(loser1 '("Hobos" "44" nil))
	(winner2 '("Cant" nil "-2"))
	(loser2 '("Rogers" "43.5" nil))
	(s2 '(("Cant" nil)
	      ("Rogers" "-1")
	      ("Hobos" nil)
	      ("Watsons" "-6.5"))))
    (should (= (defenses-spread-538 winner1 loser1 s2) -6.5))
    (should (= (defenses-spread-538 winner2 loser2 s2) 1))))

(ert-deftest defenses-results-test ()
  "Display results."
  (let ((s1 '(("Hobos" "44" nil)
	      ("Watsons" nil "-7")
	      ("Rogers" nil "PK")
	      ("Cant" "50" nil)))
	(s2 '(("Cant" "-1")
	      ("Rogers" nil)
	      ("Hobos" nil)
	      ("Watsons" "-6.5"))))
    (defenses-results s1 s2)))

(provide 'defenses-test)
;;; defenses-test.el ends here
