;;; test-helper.el --- Helper for defenses-test.

;;; Commentary:
;; Helper for defenses-test.

;; Thanks to http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

;;; Code:

(require 'f)

(defvar defenses-test-path
  (f-dirname (f-this-file)))

(defvar defenses-code-path
  (f-parent defenses-test-path))

(defvar defenses-sandbox-path
  (f-expand "sandbox" defenses-test-path))

(defvar defenses-sandbox-flyks-path
  (f-expand "flyks" defenses-sandbox-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory defenses-sandbox-path))
     (when (f-dir? defenses-sandbox-path)
       (f-delete defenses-sandbox-path :force))
     (f-mkdir defenses-sandbox-path)
     (f-mkdir defenses-sandbox-flyks-path)
     ;; Make defenses believe that it lives in the sandbox.
     (let ((defenses-dir defenses-sandbox-path))
       ,@body)))

(require 'defenses (f-expand "defenses.el" defenses-code-path))

(provide 'test-helper)
;;; test-helper.el ends here
