;;; odds.el --- Find the best defense for fantasy football using 538 and Vegas.

;; Copyright (C) 2017 Tristan McKinney

;; Author: Tristan McKinney <natsirtguy@gmail.com>
;; Maintainer: Tristan McKinney <natsirtguy@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience
;; URL:
;; Package-Requires: ((w3m "20171019.1734"))

;;; Commentary:

;;; Code:

(require 'w3m)

(defvar web-vegas
  "http://www.vegasinsider.com/nfl/odds/las-vegas/"
  "The Vegas odds website.")

(defvar web-538
  "https://projects.fivethirtyeight.com/2017-nfl-predictions/games/"
  "The 538 NFL website.")

(defun odds-results (s1 s2)
  "Display buffer with best defenses, sorted, given S1 from Vegas and S2 from 538."
  (with-output-to-temp-buffer "*scores*"
    (dolist (defense (sort (odds-teams s1 s2) 'odds-compare)))))

(defun odds-compare (d1 d2)
  "Return the better defense of D1 and D2 according to Vegas."
  (if (> (cadr d1) (cadr d2))
      d2
    d1))

(defun odds-teams (s1 s2)
  "Return list with elements (name score-vegas score-538) from Vegas in S1 and 538 in S2."
  (let (defenses)
    (while (cdr s1)
      (let* ((team1 (pop s1))
	     (team2 (pop s1))
	     (winner (odds-winner team1 team2))
	     (loser (odds-loser team1 team2))
	     (ou (string-to-number (cadr loser)))
	     (spread (string-to-number (cadr (cdr winner))))
	     (spread-538 (odds-spread-538 winner loser s2)))
	(setq defenses
	      (cons (list (car winner)
		     (/ (+ ou spread) 2.0)
		     (/ (+ ou spread-538) 2.0))
		    defenses))))
    defenses))

(defun odds-spread-538 (winner loser s2)
  "Return spread from 538 given Vegas WINNER and LOSER, and parsed list of teams S2."
  (let (spread)
    (dolist (team s2 spread)
      (if (and (cadr team) (string= (car team) (car winner)))
	  (setq spread (string-to-number (cadr team))))
      (if (and (cadr team) (string= (car team) (car loser)))
	  (setq spread (- (string-to-number (cadr team))))))))

(defun odds-winner (team1 team2)
  "Return TEAM1 if Vegas thinks they will win, TEAM2 otherwise."
  (if (cadr (cdr team1))		;Only the winning team has a spread.
      team1
    team2))

(defun odds-loser (team1 team2)
  "Return TEAM1 if Vegas thinks they will lose, TEAM2 otherwise."
  (if (cadr (cdr team1))		;Only the winning team has a spread.
      team2
    team1))

(provide 'odds)
;;; odds.el ends here
