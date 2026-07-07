;;; halo-test.el --- Tests for halo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; SPDX-License-Identifier: GPL-3.0-only

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'halo)

(define-derived-mode halo-test-derived-special-mode special-mode
  "HaloTestSpecial"
  "Derived mode used by halo tests.")

(defun halo-test-halo-overlay-at-p (position)
  "Return non-nil when a halo contrast overlay covers POSITION."
  (seq-some
   (lambda (overlay)
     (and (overlay-buffer overlay)
          (overlay-get overlay 'halo)
          (<= (overlay-start overlay) position)
          (< position (overlay-end overlay))))
   halo--overlays))

(ert-deftest halo-step-for-line-index-respects-focus-band-and-steps ()
  (let ((halo-focus-band '(0.25 . 0.75))
        (halo-steps 4))
    (should (= 4 (halo--step-for-line-index 0 9)))
    (should (= 2 (halo--step-for-line-index 1 9)))
    (should-not (halo--step-for-line-index 2 9))
    (should-not (halo--step-for-line-index 4 9))
    (should-not (halo--step-for-line-index 6 9))
    (should (= 2 (halo--step-for-line-index 7 9)))
    (should (= 4 (halo--step-for-line-index 8 9)))))

(ert-deftest halo-step-for-line-index-honors-obsolete-radius ()
  (let ((halo-focus-band (halo--default-focus-band))
        (halo-radius 2)
        (halo-steps 4)
        (halo--legacy-radius-warning-shown t))
    (should (= 4 (halo--step-for-line-index 0 9)))
    (should (= 2 (halo--step-for-line-index 1 9)))
    (should-not (halo--step-for-line-index 2 9))
    (should-not (halo--step-for-line-index 6 9))
    (should (= 2 (halo--step-for-line-index 7 9)))
    (should (= 4 (halo--step-for-line-index 8 9)))))

(ert-deftest halo-step-for-line-index-prefers-explicit-focus-band ()
  (let ((halo-focus-band '(0.0 . 1.0))
        (halo-radius 0)
        (halo-steps 4)
        (halo--legacy-radius-warning-shown t))
    (should-not (halo--step-for-line-index 0 9))
    (should-not (halo--step-for-line-index 8 9))))

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

(ert-deftest halo-make-line-overlays-coalesces-equivalent-dim-faces ()
  (let ((buffer (get-buffer-create " *halo-test-coalesce-overlays*"))
        (halo-min-alpha 0.5))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "abc")
          (put-text-property (point-min) (1+ (point-min))
                             'face '(:foreground "red"))
          (put-text-property (1+ (point-min)) (point-max)
                             'face '(:foreground "red" :weight bold))
          (setq halo--overlays nil)
          (halo--make-line-overlays (point-min) (point-max)
                                    1 (selected-window))
          (should (= 1 (length halo--overlays)))
          (should (= (point-min) (overlay-start (car halo--overlays))))
          (should (= (point-max) (overlay-end (car halo--overlays)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-delete-overlays-cleans-orphans-only-when-requested ()
  (let ((buffer (get-buffer-create " *halo-test-delete-orphans*")))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "abc\n")
          (setq halo--overlays nil)
          (halo--make-overlay (point-min) (1+ (point-min))
                              '(:foreground "red") (selected-window))
          (let ((orphan (make-overlay (point-min) (1+ (point-min))
                                      (current-buffer) nil t)))
            (overlay-put orphan 'halo t)
            (overlay-put orphan 'window (selected-window))
            (halo--delete-overlays (point-min) (point-max)
                                   (selected-window))
            (should-not halo--overlays)
            (should (overlay-buffer orphan))
            (halo--delete-overlays (point-min) (point-max)
                                   (selected-window) t)
            (should-not (overlay-buffer orphan))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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
            (forward-line 1)
            (halo--update-now (selected-window))
            (should (eq overlays halo--overlays))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-notices-face-text-property-changes ()
  (let ((buffer (get-buffer-create " *halo-test-refresh-face-change*"))
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        (halo-focus-band '(1.0 . 1.0))
        (halo-steps 1)
        (halo-min-alpha 1.0)
        (halo-falloff 'linear))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "abc\n")
          (goto-char (point-min))
          (halo-mode 1)
          (let ((overlays halo--overlays))
            (should overlays)
            (put-text-property (point-min) (point-max)
                               'face '(:foreground "#123456"))
            (halo-refresh)
            (should-not (eq overlays halo--overlays))
            (should (equal (halo--blend-color
                            "#123456"
                            (halo--default-background)
                            (halo--alpha-for-step 1 1 1.0 'linear))
                           (plist-get (overlay-get (car halo--overlays) 'face)
                                      :foreground)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-skips-visible-line-scan-when-input-state-unchanged ()
  (let ((buffer (get-buffer-create " *halo-test-refresh-fast-skip*"))
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor nil)
        (halo-virtual-top-margin nil)
        scanned)
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 40)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (halo-mode 1)
          (cl-letf (((symbol-function 'halo--visible-display-lines)
                     (lambda (&rest _)
                       (setq scanned t)
                       (error "unexpected display-line scan"))))
            (halo--refresh (current-buffer) (selected-window)))
          (should-not scanned))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-overlays-are-window-local ()
  (let ((buffer (get-buffer-create " *halo-test-window-local*"))
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

(ert-deftest halo-refresh-input-state-is-window-local ()
  (let ((buffer (get-buffer-create " *halo-test-window-state*"))
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
          (should (hash-table-p halo--refresh-input-states))
          (should (gethash first-window halo--refresh-input-states))
          (should (gethash second-window halo--refresh-input-states))
          (should (eq first-window
                      (car (gethash first-window halo--refresh-input-states))))
          (should (eq second-window
                      (car (gethash second-window halo--refresh-input-states)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-dims-from-viewport-when-point-is-not-centered ()
  (let ((buffer (get-buffer-create " *halo-test-noncentered*"))
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
          (should halo--overlays))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-dims-with-virtual-top-margin ()
  (let ((buffer (get-buffer-create " *halo-test-margin-centered*"))
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
          (should halo--overlays)
          (should-not (halo-test-halo-overlay-at-p (point-min))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-refresh-keeps-buffer-end-normal-when-centered ()
  (let ((buffer (get-buffer-create " *halo-test-end-centered*"))
        (halo-steps 2)
        (halo-min-alpha 0.5)
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-live-update t)
        last-line-start)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 80)
            (insert (format "line %d%s"
                            index
                            (if (< index 79) "\n" ""))))
          (goto-char (point-max))
          (setq last-line-start (line-beginning-position))
          (halo-mode 1)
          (should halo--overlays)
          (should-not (halo-test-halo-overlay-at-p last-line-start)))
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

(ert-deftest halo-virtual-boundary-markers-use-fringe-tildes ()
  (let* ((halo-virtual-boundary-markers t)
         (line (halo--virtual-boundary-line)))
    (should (equal (face-attribute 'halo-virtual-boundary-marker :inherit)
                   'default))
    (should (equal halo--virtual-boundary-marker-bitmap-array
                   [#b00000000
                    #b00000000
                    #b00000000
                    #b01110001
                    #b11011011
                    #b10001110
                    #b00000000
                    #b00000000]))
    (should (get-text-property 0 'display line))
    (should (equal (get-text-property 0 'display line)
                   '(left-fringe halo--virtual-boundary-marker
                                 halo-virtual-boundary-marker)))))

(ert-deftest halo-virtual-boundary-markers-are-off-by-default ()
  (let ((halo-virtual-boundary-markers nil))
    (should-not (get-text-property 0 'display
                                   (halo--virtual-boundary-line)))))

(ert-deftest halo-empty-line-indicators-use-fringe-tildes ()
  (let ((buffer (get-buffer-create " *halo-test-bottom-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-virtual-boundary-markers t)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird")
          (goto-char (point-max))
          (halo-mode 1)
          (should indicate-empty-lines)
          (should (equal (alist-get 'empty-line fringe-indicator-alist)
                         'halo--virtual-boundary-marker)))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-empty-line-indicators-are-off-by-default ()
  (let ((buffer (get-buffer-create " *halo-test-bottom-off*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-virtual-boundary-markers nil)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (halo-mode 1)
          (should-not indicate-empty-lines))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-empty-line-indicators-restore-previous-state ()
  (let ((buffer (get-buffer-create " *halo-test-empty-line-restore*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-virtual-boundary-markers t)
        (halo-live-update t)
        (original-fringe '((empty-line . empty-line))))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (setq-local indicate-empty-lines nil)
          (setq-local fringe-indicator-alist original-fringe)
          (halo-mode 1)
          (should indicate-empty-lines)
          (should (equal (alist-get 'empty-line fringe-indicator-alist)
                         'halo--virtual-boundary-marker))
          (halo-mode -1)
          (should-not indicate-empty-lines)
          (should (equal fringe-indicator-alist original-fringe)))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-center-recenter-line-uses-center-without-virtual-margin ()
  (let ((buffer (get-buffer-create " *halo-test-recenter-line*"))
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

(ert-deftest halo-center-window-removes-virtual-top-margin-when-disabled ()
  (let ((buffer (get-buffer-create " *halo-test-center-disable-margin*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (halo-mode 1)
          (should (halo--virtual-top-margin-overlay (selected-window)))
          (setq halo-center-cursor nil)
          (halo-refresh t)
          (should-not (halo--virtual-top-margin-overlay (selected-window))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-center-window-skips-unchanged-state ()
  (let ((buffer (get-buffer-create " *halo-test-center-skip*"))
        (halo-center-cursor t)
        (halo-virtual-top-margin t)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (halo-mode 1)
          (cl-letf (((symbol-function 'halo--center-cursor-active-p)
                     (lambda (&rest _)
                       (error "unexpected centering recomputation"))))
            (halo--center-window (selected-window))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-image-display-suspends-centering-only ()
  (let ((buffer (get-buffer-create " *halo-test-image-display*"))
        (halo-center-cursor t)
        (halo-center-cursor-display-fallback t)
        (halo-virtual-top-margin t)
        (halo-live-update t)
        before-position
        image-position)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "far\n")
          (setq before-position (point))
          (insert "before\n")
          (setq image-position (point))
          (insert (propertize
                   "i"
                   'display
                   '(image :type pbm :data "P1\n1 1\n0\n")))
          (insert "\nafter\n")
          (goto-char (point-min))
          (halo-mode 1)
          (should (halo--center-cursor-active-p (selected-window)))
          (goto-char before-position)
          (halo-refresh t)
          (should (halo--nearby-display-line-has-image-display-p
                   (selected-window)))
          (should-not (halo--center-cursor-active-p (selected-window)))
          (goto-char image-position)
          (halo-refresh t)
          (should (halo--nearby-display-line-has-image-display-p
                   (selected-window)))
          (should-not (halo--center-cursor-active-p (selected-window)))
          (should-not (halo--virtual-top-margin-overlay (selected-window)))
          (should halo--overlays))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-image-display-spec-handles-cyclic-lists ()
  (let ((plain-cycle (list 'space))
        (image-cycle (list '(image :type pbm :data "P1\n1 1\n0\n"))))
    (setcdr plain-cycle plain-cycle)
    (setcdr image-cycle image-cycle)
    (should-not (halo--image-display-spec-p plain-cycle))
    (should (halo--image-display-spec-p image-cycle))))

(ert-deftest halo-image-display-fallback-notices-text-property-changes ()
  (let ((buffer (get-buffer-create " *halo-test-image-display-text-cache*"))
        (halo-center-cursor t)
        (halo-center-cursor-display-fallback t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (should (halo--center-cursor-active-p (selected-window)))
          (put-text-property
           (point-min) (1+ (point-min))
           'display '(image :type pbm :data "P1\n1 1\n0\n"))
          (should-not (halo--center-cursor-active-p (selected-window))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest halo-center-window-rechecks-image-overlay-changes ()
  (let ((buffer (get-buffer-create " *halo-test-image-overlay-cache*"))
        (halo-center-cursor t)
        (halo-center-cursor-display-fallback t)
        (halo-virtual-top-margin t)
        (halo-live-update t))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (halo-mode 1)
          (should (halo--virtual-top-margin-overlay (selected-window)))
          (let ((overlay (make-overlay (point-min) (1+ (point-min))
                                       (current-buffer) nil t)))
            (overlay-put overlay 'display
                         '(image :type pbm :data "P1\n1 1\n0\n"))
            (halo--center-window (selected-window))
            (should-not (halo--virtual-top-margin-overlay
                         (selected-window)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-image-display-fallback-removes-existing-virtual-margin ()
  (let ((buffer (get-buffer-create " *halo-test-image-display-margin*"))
        (halo-center-cursor t)
        (halo-center-cursor-display-fallback t)
        (halo-virtual-top-margin t)
        (halo-live-update t)
        before-image-position)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (insert "first\nsecond\nthird\n")
          (goto-char (point-min))
          (halo-mode 1)
          (should (halo--virtual-top-margin-overlay (selected-window)))
          (goto-char (point-max))
          (insert "before image\n")
          (setq before-image-position (line-beginning-position 0))
          (insert (propertize
                   "i"
                   'display
                   '(image :type pbm :data "P1\n1 1\n0\n")))
          (goto-char before-image-position)
          (set-window-start (selected-window) (point-min) t)
          (halo-refresh t)
          (should-not (halo--virtual-top-margin-overlay (selected-window))))
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

(ert-deftest halo-mouse-click-on-link-does-not-recenter-point ()
  (let ((buffer (get-buffer-create " *halo-test-mouse-link*"))
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
          (forward-line 10)
          (let ((link-position (point)))
            (put-text-property link-position
                               (line-end-position)
                               'shr-url
                               "https://example.com")
            (goto-char (point-min))
            (halo-mode 1)
            (set-window-start (selected-window) (point-min) t)
            (goto-char link-position)
            (let ((last-input-event
                   (list 'mouse-1
                         (posn-at-point link-position (selected-window))))
                  (this-command 'mouse-set-point))
              (halo--post-command))
            (should (= link-position (point)))))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
        (kill-buffer buffer)))))

(ert-deftest halo-mouse-wheel-text-scale-does-not-move-point ()
  (let ((buffer (get-buffer-create " *halo-test-mouse-wheel-text-scale*"))
        (halo-center-cursor nil)
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

(ert-deftest halo-refresh-force-removes-contrast-orphan-overlays ()
  (let ((buffer (get-buffer-create " *halo-test-refresh-orphan*"))
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
          (let ((orphan (make-overlay (point-min) (1+ (point-min))
                                      (current-buffer) nil t)))
            (overlay-put orphan 'halo t)
            (overlay-put orphan 'window (selected-window))
            (halo-refresh t)
            (should-not (overlay-buffer orphan))))
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

(ert-deftest halo-after-change-skips-refresh-after-visible-window ()
  (let ((buffer (get-buffer-create " *halo-test-after-change-after-window*"))
        (halo-center-cursor nil)
        (halo-live-update t)
        updated)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer buffer)
          (erase-buffer)
          (dotimes (index 120)
            (insert (format "line %d\n" index)))
          (goto-char (point-min))
          (set-window-start (selected-window) (point-min) t)
          (halo-mode 1)
          (setq halo--pending-change-range (cons (point-max) (point-max)))
          (cl-letf (((symbol-function 'halo--update-now)
                     (lambda (&rest _)
                       (setq updated t)))
                    ((symbol-function 'window-end)
                     (lambda (&rest _)
                       (point-min))))
            (halo--after-change-update buffer))
          (should-not updated))
      (delete-other-windows)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when halo-mode
            (halo-mode -1)))
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
