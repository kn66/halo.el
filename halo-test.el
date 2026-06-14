;;; halo-test.el --- Tests for halo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-only

;;; Code:

(require 'ert)
(require 'seq)
(require 'halo)

(define-derived-mode halo-test-derived-special-mode special-mode
  "HaloTestSpecial"
  "Derived mode used by halo tests.")

(ert-deftest halo-step-for-distance-respects-radius-and-steps ()
  (let ((halo-radius 2)
        (halo-steps 3))
    (should-not (halo--step-for-distance 0))
    (should-not (halo--step-for-distance 2))
    (should (= 1 (halo--step-for-distance 3)))
    (should (= 3 (halo--step-for-distance 20)))))

(ert-deftest halo-alpha-for-step-blends-to-min-alpha ()
  (let ((halo-steps 4)
        (halo-min-alpha 0.5)
        (halo-falloff 'linear))
    (should (= 0.875 (halo--alpha-for-step 1)))
    (should (= 0.5 (halo--alpha-for-step 4)))))

(ert-deftest halo-alpha-for-step-supports-smoothstep-falloff ()
  (let ((halo-steps 4)
        (halo-min-alpha 0.5)
        (halo-falloff 'smoothstep))
    (should (= 0.921875 (halo--alpha-for-step 1)))
    (should (= 0.5 (halo--alpha-for-step 4)))))

(ert-deftest halo-face-cache-key-includes-falloff ()
  (let ((halo-steps 4)
        (halo-min-alpha 0.5)
        (halo-falloff 'linear))
    (should-not (equal (halo--face-cache-key)
                       (let ((halo-falloff 'smoothstep))
                         (halo--face-cache-key))))))

(ert-deftest halo-face-foreground-reads-plist-and-face-lists ()
  (should (equal "red" (halo--face-foreground '(:foreground "red"))))
  (should (equal "blue"
                 (halo--face-foreground
                  '((:weight bold) (:foreground "blue"))))))

(ert-deftest halo-face-foreground-ignores-unknown-face-symbols ()
  (should-not (halo--face-foreground 'halo-test-missing-face)))

(ert-deftest halo-display-line-index-finds-containing-or-following-line ()
  (let ((lines '((1 . 5) (8 . 12) (12 . 20))))
    (should (= 0 (halo--display-line-index 3 lines)))
    (should (= 1 (halo--display-line-index 6 lines)))
    (should (= 2 (halo--display-line-index 18 lines)))
    (should (= 2 (halo--display-line-index 40 lines)))))

(ert-deftest halo-center-line-respects-center-fraction ()
  (let ((buffer (get-buffer-create " *halo-test-center-fraction*"))
        (halo-center-fraction 0.25))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (should (= (round (* (1- (window-body-height (selected-window)))
                               halo-center-fraction))
                     (halo--center-line (selected-window)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-center-cursor-includes-eshell-by-default ()
  (let ((buffer (get-buffer-create " *halo-test-center-excluded*"))
        (halo-center-cursor t))
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (should (halo--center-cursor-enabled-p))
          (setq major-mode 'shell-mode)
          (should (halo--center-cursor-enabled-p))
          (setq major-mode 'eshell-mode)
          (should (halo--center-cursor-enabled-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-after-change-center-includes-eshell-by-default ()
  (let ((buffer (get-buffer-create " *halo-test-after-change-center-excluded*"))
        (halo-center-cursor t))
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (should (halo--after-change-center-enabled-p))
          (setq major-mode 'eshell-mode)
          (should (halo--after-change-center-enabled-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-after-change-center-can-exclude-eshell ()
  (let ((buffer (get-buffer-create " *halo-test-after-change-center-excluded*"))
        (halo-center-cursor t)
        (halo-after-change-center-excluded-modes '(eshell-mode)))
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (should (halo--after-change-center-enabled-p))
          (setq major-mode 'eshell-mode)
          (should-not (halo--after-change-center-enabled-p)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-center-window-removes-margin-in-excluded-modes ()
  (let ((buffer (get-buffer-create " *halo-test-center-excluded-margin*"))
        (halo-center-cursor t)
        (halo-center-excluded-modes '(eshell-mode))
        (halo-virtual-top-margin t))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (setq major-mode 'eshell-mode)
          (let ((overlay (make-overlay (point-min) (point-min)
                                       (current-buffer) nil nil)))
            (overlay-put overlay 'halo-virtual-top-margin t)
            (halo--set-virtual-top-margin-overlay (selected-window) overlay))
          (halo--center-window (selected-window))
          (should-not (halo--virtual-top-margin-overlay (selected-window))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-display-lines-before-point-can-be-capped ()
  (let ((buffer (get-buffer-create " *halo-test-display-lines-cap*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (forward-line 40)
          (should (= 40 (halo--display-lines-before-point
                         (selected-window))))
          (should (= 5 (halo--display-lines-before-point
                        (selected-window) 5)))
          (goto-char (point-min))
          (forward-line 3)
          (should (= 3 (halo--display-lines-before-point
                        (selected-window) 5))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-skips-unchanged-state ()
  (let ((buffer (get-buffer-create " *halo-test-refresh*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 40)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (let ((overlays halo--overlays)
                (state halo--refresh-state))
            (should overlays)
            (halo--update-now (selected-window))
            (should (eq overlays halo--overlays))
            (should (equal state halo--refresh-state))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-overlays-are-window-local ()
  (let ((buffer (get-buffer-create " *halo-test-window-local*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        first-window
        second-window)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 60)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (setq first-window (selected-window))
          (setq second-window (split-window-right))
          (halo-mode 1)
          (should halo--overlays)
          (dolist (overlay halo--overlays)
            (should (eq first-window (overlay-get overlay 'window))))
          (select-window second-window)
          (with-current-buffer buffer
            (goto-char (point-min))
            (halo-refresh t)
            (should halo--overlays)
            (should (seq-some
                     (lambda (overlay)
                       (eq first-window (overlay-get overlay 'window)))
                     halo--overlays))
            (should (seq-some
                     (lambda (overlay)
                       (eq second-window (overlay-get overlay 'window)))
                     halo--overlays))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-all-keeps-overlays-for-each-window ()
  (let ((buffer (get-buffer-create " *halo-test-refresh-all*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        first-window
        second-window)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 60)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (setq first-window (selected-window))
          (setq second-window (split-window-right))
          (set-window-buffer second-window buffer)
          (halo-mode 1)
          (halo-refresh-all t)
          (should (seq-some
                   (lambda (overlay)
                     (eq first-window (overlay-get overlay 'window)))
                   halo--overlays))
          (should (seq-some
                   (lambda (overlay)
                     (eq second-window (overlay-get overlay 'window)))
                   halo--overlays)))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-state-is-window-local ()
  (let ((buffer (get-buffer-create " *halo-test-window-state*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        first-window
        second-window)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (setq first-window (selected-window))
          (setq second-window (split-window-right))
          (set-window-buffer second-window buffer)
          (halo-mode 1)
          (halo-refresh-all t)
          (should (hash-table-p halo--refresh-states))
          (should (gethash first-window halo--refresh-states))
          (should (gethash second-window halo--refresh-states))
          (should (eq first-window
                      (car (gethash first-window halo--refresh-states))))
          (should (eq second-window
                      (car (gethash second-window halo--refresh-states)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-virtual-top-margin-is-window-local ()
  (let ((buffer (get-buffer-create " *halo-test-virtual-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t)
        first-window
        second-window)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "top\nsecond\nthird\n")
          (goto-char (point-min))
          (setq first-window (selected-window))
          (setq second-window (split-window-right))
          (set-window-buffer second-window buffer)
          (halo-mode 1)
          (select-window second-window)
          (with-current-buffer buffer
            (goto-char (point-min))
            (halo-refresh t))
          (should (hash-table-p halo--virtual-top-margin-overlays))
          (should (gethash first-window halo--virtual-top-margin-overlays))
          (should (gethash second-window halo--virtual-top-margin-overlays))
          (should halo--virtual-top-margin-overlay)
          (should (eq first-window
                      (overlay-get
                       (gethash first-window halo--virtual-top-margin-overlays)
                       'window)))
          (should (eq second-window
                      (overlay-get
                       (gethash second-window halo--virtual-top-margin-overlays)
                       'window))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-virtual-top-margin-centers-empty-and-single-line-buffers ()
  (let ((buffer (get-buffer-create " *halo-test-short-buffer-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (halo-mode 1)
          (should halo--virtual-top-margin-overlay)
          (should halo--virtual-top-margin-overlays)
          (should (= 0 (halo--center-recenter-line (selected-window))))
          (insert "dddd")
          (goto-char (point-max))
          (halo-refresh t)
          (should halo--virtual-top-margin-overlay)
          (should halo--virtual-top-margin-overlays)
          (should (= 0 (halo--center-recenter-line (selected-window))))
          (should (string-match-p "\n" (overlay-get halo--virtual-top-margin-overlay
                                                    'before-string))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-virtual-top-margin-ignores-existing-margin-when-recomputing ()
  (let ((buffer (get-buffer-create " *halo-test-stable-short-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "prompt")
          (goto-char (point-max))
          (halo-mode 1)
          (let ((initial-lines
                 (halo--virtual-top-margin-lines (selected-window))))
            (should (< 0 initial-lines))
            (halo-refresh t)
            (should (= initial-lines
                       (halo--virtual-top-margin-lines (selected-window))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-center-recenter-line-uses-center-without-virtual-margin ()
  (let ((buffer (get-buffer-create " *halo-test-recenter-line*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-max))
          (halo-mode 1)
          (should (= (halo--center-line (selected-window))
                     (halo--center-recenter-line (selected-window)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-center-window-uses-visible-lines-with-invisible-text ()
  (let ((buffer (get-buffer-create " *halo-test-visible-center*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin nil)
        (halo-radius 99)
        (halo-live-update t)
        hide-start
        hide-end
        point-position)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (setq buffer-invisibility-spec t)
          (insert "visible top\n")
          (setq hide-start (point))
          (dotimes (index 30)
            (insert (format "hidden %d\n" index)))
          (setq hide-end (point))
          (dotimes (index 50)
            (insert (format "visible %d\n" index))
            (when (= index 24)
              (setq point-position (line-beginning-position))))
          (let ((overlay (make-overlay hide-start hide-end
                                       (current-buffer) nil t)))
            (overlay-put overlay 'invisible t))
          (goto-char point-position)
          (halo-mode 1)
          (let* ((window (selected-window))
                 (lines (halo--visible-display-lines window))
                 (point-index (halo--display-line-index (point) lines)))
            (should (= (halo--center-line window) point-index))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-virtual-top-margin-removes-orphan-overlays ()
  (let ((buffer (get-buffer-create " *halo-test-orphan-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (let ((orphan (make-overlay (point-min) (point-min)
                                      (current-buffer) nil nil)))
            (overlay-put orphan 'halo-virtual-top-margin t)
            (overlay-put orphan 'window (selected-window))
            (overlay-put orphan 'before-string (make-string 15 ?\n))
            (overlay-put orphan 'halo-virtual-top-margin-lines 15))
          (halo-mode 1)
          (halo-refresh t)
          (let ((virtual-overlays
                 (seq-filter
                  (lambda (overlay)
                    (overlay-get overlay 'halo-virtual-top-margin))
                  (overlays-in (point-min) (point-min)))))
            (should (= 1 (length virtual-overlays)))
            (should (eq halo--virtual-top-margin-overlay
                        (car virtual-overlays)))
            (should (= (halo--center-line (selected-window))
                       (overlay-get (car virtual-overlays)
                                    'halo-virtual-top-margin-lines)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-virtual-top-margin-keeps-window-start-at-buffer-start ()
  (let ((buffer (get-buffer-create " *halo-test-margin-window-start*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 99)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (forward-line 1)
          (halo-mode 1)
          (set-window-start (selected-window) (point) t)
          (halo-refresh t)
          (should (< 0 (halo--virtual-top-margin-lines (selected-window))))
          (should (= (point-min) (window-start (selected-window)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-force-rebuilds-overlays ()
  (let ((buffer (get-buffer-create " *halo-test-refresh-api*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 40)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (let ((overlays halo--overlays))
            (should overlays)
            (halo-refresh t)
            (should halo--overlays)
            (should-not (eq overlays halo--overlays))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-global-enable-respects-derived-exclusions ()
  (let ((buffer (get-buffer-create " *halo-test-global-derived*"))
        (halo-global-excluded-modes '(special-mode))
        (halo-global-exclude-predicate nil))
    (unwind-protect
        (with-current-buffer buffer
          (halo-test-derived-special-mode)
          (halo--global-enable)
          (should-not halo-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-global-enable-respects-exclude-predicate ()
  (let ((buffer (get-buffer-create " *halo-test-global-predicate*"))
        (halo-global-excluded-modes nil)
        (halo-global-exclude-predicate (lambda () t)))
    (unwind-protect
        (with-current-buffer buffer
          (fundamental-mode)
          (halo--global-enable)
          (should-not halo-mode))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-after-change-skips-command-loop-refresh ()
  (let ((buffer (get-buffer-create " *halo-test-after-change*")))
    (unwind-protect
        (with-current-buffer buffer
          (halo-mode 1)
          (setq halo--in-command t)
          (halo--schedule-after-change)
          (should-not halo--change-timer))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(provide 'halo-test)

;;; halo-test.el ends here
