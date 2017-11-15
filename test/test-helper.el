;;; test-helper.el --- Helper for odds-test.

;;; Commentary:
;; Helper for odds-test.

;; Thanks to http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

;;; Code:

(require 'f)
(require 'cl)

(defvar odds-test-path
  (f-dirname (f-this-file)))

(defvar odds-code-path
  (f-parent odds-test-path))

(defvar odds-sandbox-path
  (f-expand "sandbox" odds-test-path))

(defvar odds-sandbox-flyks-path
  (f-expand "flyks" odds-sandbox-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory odds-sandbox-path))
     (when (f-dir? odds-sandbox-path)
       (f-delete odds-sandbox-path :force))
     (f-mkdir odds-sandbox-path)
     (f-mkdir odds-sandbox-flyks-path)
     ;; Make odds believe that it lives in the sandbox.
     (let ((odds-dir odds-sandbox-path))
       ,@body)))

(defmacro kill-leftover-buffers (&rest body)
  "Kill any new buffers created during evaluation of BODY."
  `(let ((oldbufs (buffer-list)))
     (unwind-protect
	 (progn
	   ,@body)
       (dolist (extrabufs (set-difference (buffer-list) oldbufs))
	 (kill-buffer extrabufs)))))

(require 'odds (f-expand "odds.el" odds-code-path))

(provide 'test-helper)
;;; test-helper.el ends here
