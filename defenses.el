;;; defenses.el --- Find the best defense for fantasy football using 538 and Vegas.

;; Copyright (C) 2017 Tristan McKinney

;; Author: Tristan McKinney <natsirtguy@gmail.com>
;; Maintainer: Tristan McKinney <natsirtguy@gmail.com>
;; Version: 0.1.0
;; Keywords: games
;; URL: https://github.com/natsirtguy/defenses
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

(defun defenses ()
  "Show the best defenses."
  (interactive)
  (defenses-results (defenses-parse-vegas) (defenses-parse-538)))

(defun defenses-parse-vegas ()
  "Return list with elements (team ou? spread?) by parsing `web-vegas'."
  (let* ((teamname "\\([A-Z][A-OQ-Za-z\\. ]*\\)")
	 (open "\\(?:-?[0-9][0-9]?\\.?5?\\|PK\\)\\(?:u\\| \\)\\(?:+\\|-\\|EV\\)[0-9]?[0-9]? *")
	 (c-spread "\\(-[0-9][0-9]?\\.?5?\\|PK\\)?")
	 (c-ou "\\([0-9][0-9]\\.?5?\\)?")
	 (pattern (concat "[0-9]\\{3\\} " teamname open c-spread c-ou)))
    (with-temp-buffer
      (setq case-fold-search nil)
      (w3m-retrieve web-vegas)
      (w3m-buffer)
      (while (re-search-forward "½" nil t)
	(replace-match ".5"))
      (goto-char (point-min))
      (let (teams)
	(while (re-search-forward pattern nil t)
	  (let* ((team (match-string-no-properties 1))
		 (spread (match-string-no-properties 2))
		 (ou (match-string-no-properties 3)))
	    (setq teams (cons (list (string-trim team) ou spread) teams))))
	teams))))

(defun defenses-parse-538 ()
  "Return list with elements (team spread?) by parsing `web-538'."
  (let* ((teamname "\\([A-Z][A-OQ-Za-z\\. ]*\\)")
	 (c-spread "\\(-[0-9][0-9]?\\.?5?\\|PK\\)?")
	 (pattern (concat "[A-Z]\\{2,3\\}-logo *" teamname c-spread)))
    (with-temp-buffer
      (setq case-fold-search nil)
      (w3m-retrieve web-538)
      (w3m-buffer)
      (re-search-forward "Week [0-9][0-9]?\n")
      (let (teams)
	(while (re-search-forward pattern nil t)
	  (let* ((team (match-string-no-properties 1))
		 (spread (match-string-no-properties 2)))
	    (setq teams (cons (list (string-trim team) spread) teams))))
	(reverse teams)))))

(defun defenses-results (s1 s2)
  "Display buffer with best defenses, sorted, given S1 from Vegas and S2 from 538."
  (with-output-to-temp-buffer "*defenses*"
    (princ "Team                 Vegas         538\n")
    (princ "----------------------------------------\n")
    (dolist (defense (sort (defenses-teams s1 s2) 'defenses-compare))
      (princ (concat
	      (format "%-20s %-13.2f %.2f"
		      (car defense)
		      (if (cadr defense)
			  (cadr defense)
			0)
		      (if (cadr (cdr defense))
			  (cadr (cdr defense))
			0))
	      "\n")))))

(defun defenses-compare (d1 d2)
  "Return t if D1 is better than D2 according to Vegas."
  (if (and (cadr d1) (cadr d2)) 	;Check if non-nil
      (if (< (cadr d1) (cadr d2)) t nil)
    (if (cadr d1) t nil)))

(defun defenses-teams (s1 s2)
  "Return list with elements (name score-vegas score-538) from Vegas in S1 and 538 in S2; values are nil if info is missing."
  (let (defenses)
    (while (cdr s1)
      (let* ((team1 (pop s1))
	     (team2 (pop s1))
	     (winner (defenses-winner team1 team2))
	     (loser (defenses-loser team1 team2))
	     (ou
	      (if (cadr loser)
		  (string-to-number (cadr loser))
		nil))
	     (spread
	      (if (cadr (cdr winner))
		  (string-to-number (cadr (cdr winner)))
		nil))
	     (spread-538 (defenses-spread-538 winner loser s2)))
	(setq defenses
	      (cons (list (car winner)
			  (if (and ou spread)
			      (/ (+ ou spread) 2.0)
			    nil)
			  (if (and ou spread-538)
			      (/ (+ ou spread-538) 2.0)
			    nil))
		    (cons (list (car loser)
				(if (and ou spread)
				    (/ (- ou spread) 2.0)
				  nil)
				(if (and ou spread-538)
				    (/ (- ou spread-538) 2.0)
				  nil))
			  defenses)))))
    defenses))

(defun defenses-spread-538 (winner loser s2)
  "Return spread from 538 given Vegas WINNER and LOSER, and parsed list of teams S2."
  (let (spread)
    (dolist (team s2 spread)
      (if (and (cadr team) (string= (car team) (car winner)))
	  (setq spread
		(if (cadr team)
		    (string-to-number (cadr team))
		  nil)))
      (if (and (cadr team) (string= (car team) (car loser)))
	  (setq spread
		(if (cadr team)
		    (- (string-to-number (cadr team)))
		  nil))))))

(defun defenses-winner (team1 team2)
  "Return TEAM1 if Vegas thinks they will win, TEAM2 otherwise."
  (if (cadr (cdr team1))		;Only the winning team has a spread.
      team1
    team2))

(defun defenses-loser (team1 team2)
  "Return TEAM1 if Vegas thinks they will lose, TEAM2 otherwise."
  (if (cadr (cdr team1))		;Only the winning team has a spread.
      team2
    team1))

(provide 'defenses)
;;; defenses.el ends here
