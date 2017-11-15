;;; odds-test.el --- Tests for odds.el.

;;; Commentary:

;; Tests for the odds.el.

;;; Code:

;; Open a buffer with a brand new .flyk file.
(ert-deftest flykey-open-flyk-test ()
  ;; Since this will create files, work in a sandbox directory.
  (with-sandbox
   (kill-leftover-buffers
    )))

;; Use helper macro kill-leftover-buffers.
(ert-deftest kill-leftover-buffers-test ()
  (with-temp-buffer
    (let ((buflist (buffer-list)))
      (kill-leftover-buffers
       (get-buffer-create "someotherbuffer"))
      (should (equal buflist (buffer-list))))))

;; Kill various flykey buffers.
(ert-deftest flykey-kill-buffer-test ()
  (with-flykey-running
   (kill-buffer flykey-flybuf))
  (with-flykey-running
   (kill-buffer flykey-insertbuf))
  (with-flykey-running
   (kill-buffer)))

;; Use FlyKey with a file in fundamental mode (no keymap).
(ert-deftest flykey-fundamental-mode-test ()
  (with-sandbox
   (kill-leftover-buffers)))

(provide 'odds-test)
;;; odds-test.el ends here
