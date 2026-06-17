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

(ert-deftest halo-refresh-skips-dimming-when-point-is-not-centered ()
  (let ((buffer (get-buffer-create " *halo-test-noncentered*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor t)
        (halo-virtual-top-margin nil)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (forward-line 40)
          (halo-mode 1)
          (should halo--overlays)
          (goto-char (point-min))
          (set-window-start (selected-window) (point-min) t)
          (halo-refresh t)
          (should-not halo--overlays))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-treats-virtual-top-margin-as-centered ()
  (let ((buffer (get-buffer-create " *halo-test-margin-centered*"))
        (halo-radius 0)
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (should (< 0 (halo--virtual-top-margin-lines (selected-window))))
          (should halo--overlays))
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

(ert-deftest halo-visible-display-lines-skip-invisible-folds ()
  (let ((buffer (get-buffer-create " *halo-test-invisible-folds*"))
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        (inhibit-modification-hooks t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "visible 1\nfolded 1\nfolded 2\nvisible 2\nvisible 3\n")
          (let ((fold (make-overlay (save-excursion
                                      (goto-char (point-min))
                                      (forward-line 1)
                                      (point))
                                    (save-excursion
                                      (goto-char (point-min))
                                      (forward-line 3)
                                      (point))
                                    buffer)))
            (overlay-put fold 'invisible t)
            (goto-char (point-min))
            (halo-mode 1)
            (let ((visible-text
                   (mapcar
                    (lambda (range)
                      (buffer-substring-no-properties
                       (car range)
                       (min (cdr range)
                            (save-excursion
                              (goto-char (car range))
                              (line-end-position)))))
                    (halo--visible-display-lines (selected-window)))))
              (should (member "visible 1" visible-text))
              (should (member "visible 2" visible-text))
              (should-not (member "folded 1" visible-text))
              (should-not (member "folded 2" visible-text)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-after-change-update-centers-process-output-like-buffers ()
  (let ((buffer (get-buffer-create " *halo-test-process-output*"))
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
          (insert "process output\n")
          (goto-char (point-max))
          (halo--after-change-update buffer)
          (should halo--virtual-top-margin-overlay)
          (should (< 0 (halo--virtual-top-margin-lines (selected-window))))
          (should (= (point-min) (window-start (selected-window)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-mouse-wheel-scroll-keeps-point-centered ()
  (let ((buffer (get-buffer-create " *halo-test-mouse-wheel*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-radius 0)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (let ((wheel-start (save-excursion
                               (goto-char (point-min))
                               (forward-line 10)
                               (point))))
            (set-window-start (selected-window) wheel-start t)
            (let ((expected-point
                   (save-excursion
                     (goto-char wheel-start)
                     (forward-line (halo--center-line (selected-window)))
                     (point))))
              (let ((this-command 'mwheel-scroll))
                (halo--post-command))
              (should (= expected-point (point))))
            (should (= wheel-start (window-start (selected-window))))
            (should halo--overlays)))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-mouse-wheel-text-scale-does-not-move-point ()
  (let ((buffer (get-buffer-create " *halo-test-mouse-wheel-text-scale*"))
        (halo-center-cursor nil)
        (halo-radius 0)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (let ((wheel-start (save-excursion
                               (goto-char (point-min))
                               (forward-line 10)
                               (point)))
                (point-start (save-excursion
                               (goto-char (point-min))
                               (forward-line 30)
                               (point))))
            (set-window-start (selected-window) wheel-start t)
            (goto-char point-start)
            (let ((this-command 'mouse-wheel-text-scale))
              (halo--post-command))
            (should (= point-start (point)))))
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
