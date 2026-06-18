;;; halo.el --- Gradually dim lines away from point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nobuyuki Kamimoto

;; Author: kn66
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, faces
;; SPDX-License-Identifier: GPL-3.0-only

;;; Commentary:

;; halo is a small experimental minor mode that keeps the
;; neighborhood around point at normal contrast while making farther visible
;; lines quieter.  It is intended for calm, focus-oriented editing setups.
;;
;; Usage:
;;   (require 'halo)
;;   (halo-mode 1)

;;; Code:

(require 'color)

(defgroup halo nil
  "Quietly reduce contrast away from point."
  :group 'faces
  :group 'convenience
  :prefix "halo-")

(defcustom halo-radius 12
  "Number of lines around point that remain at normal contrast.
The current line and this many lines above and below it are left untouched."
  :type 'natnum
  :safe #'natnump
  :group 'halo)

(defcustom halo-min-alpha 0.360
  "Minimum foreground alpha used for the farthest dimmed lines.
A value of 1.0 means no dimming.  Lower values move the foreground closer to
the default background color."
  :type '(restricted-sexp :match-alternatives
                          ((lambda (value)
                             (and (numberp value)
                                  (<= 0.0 value)
                                  (<= value 1.0)))))
  :safe (lambda (value)
          (and (numberp value) (<= 0.0 value) (<= value 1.0)))
  :group 'halo)

(defcustom halo-steps 6
  "Number of contrast steps between normal and minimum contrast."
  :type '(restricted-sexp :match-alternatives
                          ((lambda (value)
                             (and (integerp value) (< 0 value)))))
  :safe (lambda (value)
          (and (integerp value) (< 0 value)))
  :group 'halo)

(defcustom halo-falloff 'smoothstep
  "Curve used to reduce contrast outside `halo-radius'.
`linear' changes contrast at a constant rate.  `smoothstep' changes contrast
more gently near the focus boundary and near the farthest dimmed lines."
  :type '(choice (const :tag "Linear" linear)
                 (const :tag "Smoothstep" smoothstep))
  :safe (lambda (value)
          (memq value '(linear smoothstep)))
  :group 'halo)

(defcustom halo-idle-delay 0.06
  "Idle delay in seconds before refreshing halo overlays."
  :type 'number
  :safe (lambda (value)
          (and (numberp value) (<= 0.0 value)))
  :group 'halo)

(defcustom halo-live-update t
  "When non-nil, update contrast overlays immediately after point movement.
When nil, updates are coalesced through `halo-idle-delay'."
  :type 'boolean
  :safe #'booleanp
  :group 'halo)

(defcustom halo-center-cursor t
  "When non-nil, keep point near the vertical center while scrolling.
This calls `recenter' after point movement instead of relying on Emacs'
automatic scroll margins."
  :type 'boolean
  :safe #'booleanp
  :group 'halo)

(defcustom halo-center-cursor-display-fallback t
  "When non-nil, suspend cursor centering near image display lines.
Large display properties, such as images in EWW buffers, can occupy many
screen lines while still representing only one buffer position.  In that case
keeping point centered can prevent natural movement through the displayed
content, so halo falls back to normal window scrolling while point is on or
next to that display line and keeps dimming active."
  :type 'boolean
  :safe #'booleanp
  :group 'halo)

(defcustom halo-center-fraction 0.5
  "Vertical window fraction where point should rest when centering is enabled.
A value of 0.5 means the visual center.  Smaller values place point higher in
the window, leaving more preview context below point."
  :type '(restricted-sexp :match-alternatives
                          ((lambda (value)
                             (and (numberp value)
                                  (<= 0.0 value)
                                  (<= value 1.0)))))
  :safe (lambda (value)
          (and (numberp value) (<= 0.0 value) (<= value 1.0)))
  :group 'halo)

(defcustom halo-virtual-top-margin t
  "When non-nil, add display-only space before the first buffer line.
This lets `halo-center-cursor' keep point near the vertical center even at
the beginning of the buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'halo)

(defcustom halo-virtual-boundary-markers nil
  "When non-nil, mark virtual buffer edges with fringe tildes.
The markers are shown in display-only virtual lines before `point-min' and
after `point-max', similar to vi-style empty-line tildes."
  :type 'boolean
  :safe #'booleanp
  :group 'halo)

(defcustom halo-global-excluded-modes
  '(minibuffer-mode)
  "Major modes where `halo-global-mode' should not enable itself.
Derived modes are also excluded."
  :type '(repeat symbol)
  :group 'halo)

(defcustom halo-global-exclude-predicate nil
  "Predicate called in each buffer before `halo-global-mode' enables `halo-mode'.
When the predicate returns non-nil, `halo-mode' is not enabled in that buffer."
  :type '(choice (const :tag "No predicate" nil)
                 function)
  :group 'halo)

(defvar-local halo--overlays nil
  "Overlays currently owned by halo in this buffer.")

(defvar-local halo--timer nil
  "Idle timer used to coalesce halo refreshes.")

(defvar-local halo--change-timer nil
  "Timer used to recenter after non-command buffer changes.")

(defvar-local halo--in-command nil
  "Non-nil while halo is handling an interactive command.")

(defvar-local halo--window nil
  "Window most recently used to compute overlays for this buffer.")

(defvar-local halo--face-cache-key nil
  "Key describing the currently cached dimming faces.")

(defvar-local halo--face-cache nil
  "Hash table of cached dimming face specs.")

(defvar-local halo--virtual-top-margin-overlay nil
  "Most recently used display-only top margin overlay.")

(defvar-local halo--virtual-top-margin-overlays nil
  "Hash table of display-only top margin overlays keyed by window.")

(defvar-local halo--refresh-state nil
  "Most recently used overlay refresh state.")

(defvar-local halo--refresh-states nil
  "Hash table of overlay refresh states keyed by window.")

(defvar-local halo--empty-line-indicator-state nil
  "Previous empty-line fringe indicator state for this buffer.")

(defconst halo--mouse-wheel-commands
  '(mwheel-scroll
    pixel-scroll-precision
    pixel-scroll-interpolate-down
    pixel-scroll-interpolate-up
    pixel-scroll-up
    pixel-scroll-down)
  "Commands that scroll the window through mouse wheel input.")

(defvar halo-mode)

(defconst halo--virtual-boundary-marker-bitmap-array
  [#b00000000
   #b00000000
   #b00000000
   #b01110001
   #b11011011
   #b10001110
   #b00000000
   #b00000000]
  "Bitmap array used for virtual buffer-edge fringe markers.")

(defface halo-virtual-boundary-marker
  '((t :inherit default))
  "Face used for virtual buffer-edge fringe markers."
  :group 'halo)

(define-fringe-bitmap 'halo--virtual-boundary-marker
  halo--virtual-boundary-marker-bitmap-array
  nil nil 'center)

(defun halo--clamp (value min-value max-value)
  "Clamp VALUE between MIN-VALUE and MAX-VALUE."
  (min max-value (max min-value value)))

(defun halo--face-color (face attribute)
  "Return FACE ATTRIBUTE as a usable color string, or nil."
  (let ((color (face-attribute face attribute nil 'default)))
    (unless (or (null color) (eq color 'unspecified))
      color)))

(defun halo--default-foreground ()
  "Return the default foreground color."
  (or (halo--face-color 'default :foreground)
      (frame-parameter nil 'foreground-color)
      "black"))

(defun halo--default-background ()
  "Return the default background color."
  (or (halo--face-color 'default :background)
      (frame-parameter nil 'background-color)
      "white"))

(defun halo--blend-channel (foreground background alpha)
  "Blend FOREGROUND and BACKGROUND integer channels by ALPHA."
  (round (+ (* alpha foreground) (* (- 1.0 alpha) background))))

(defun halo--blend-color (foreground background alpha)
  "Return FOREGROUND blended toward BACKGROUND by ALPHA.
If either color cannot be decoded, return FOREGROUND unchanged."
  (let ((fg (color-values foreground))
        (bg (color-values background)))
    (if (and fg bg)
        (format "#%04x%04x%04x"
                (halo--blend-channel (nth 0 fg) (nth 0 bg) alpha)
                (halo--blend-channel (nth 1 fg) (nth 1 bg) alpha)
                (halo--blend-channel (nth 2 fg) (nth 2 bg) alpha))
      foreground)))

(defun halo--plist-face-p (face)
  "Return non-nil when FACE is a face property list."
  (and (consp face) (keywordp (car face))))

(defun halo--face-foreground (face)
  "Return the effective foreground color for FACE, or nil."
  (cond
   ((null face)
    nil)
   ((symbolp face)
    (when (facep face)
      (halo--face-color face :foreground)))
   ((halo--plist-face-p face)
    (or (let ((foreground (plist-get face :foreground)))
          (unless (or (null foreground) (eq foreground 'unspecified))
            foreground))
        (halo--face-foreground (plist-get face :inherit))))
   ((consp face)
    (catch 'foreground
      (dolist (entry face)
        (let ((foreground (halo--face-foreground entry)))
          (when foreground
            (throw 'foreground foreground))))
      nil))))

(defun halo--step-for-distance (distance)
  "Return contrast step for line DISTANCE from point, or nil when untouched."
  (let* ((radius (max 0 halo-radius))
         (steps (max 1 halo-steps)))
    (when (> distance radius)
      (min steps (max 1 (- distance radius))))))

(defun halo--alpha-for-step (step)
  "Return foreground alpha for contrast STEP."
  (let* ((steps (max 1 halo-steps))
         (min-alpha (halo--clamp halo-min-alpha 0.0 1.0))
         (linear-progress (/ (float step) steps))
         (progress (if (eq halo-falloff 'smoothstep)
                       (* linear-progress linear-progress
                          (- 3.0 (* 2.0 linear-progress)))
                     linear-progress)))
    (- 1.0 (* progress (- 1.0 min-alpha)))))

(defun halo--face-cache-key ()
  "Return a key for the current face cache."
  (list (halo--default-background)
        (max 1 halo-steps)
        (halo--clamp halo-min-alpha 0.0 1.0)
        halo-falloff))

(defun halo--dim-face (face step)
  "Return a cached dimming face spec for FACE at contrast STEP."
  (let ((key (halo--face-cache-key)))
    (unless (equal key halo--face-cache-key)
      (setq halo--face-cache-key key
            halo--face-cache (make-hash-table :test #'equal)))
    (let* ((foreground (or (halo--face-foreground face)
                           (halo--default-foreground)))
           (cache-key (list face foreground step))
           (cached (gethash cache-key halo--face-cache)))
      (or cached
          (let ((dim-face
                 `(:foreground ,(halo--blend-color
                                 foreground
                                 (halo--default-background)
                                 (halo--alpha-for-step step))
                               :extend t)))
            (puthash cache-key dim-face halo--face-cache)
            dim-face)))))

(defun halo--text-face-at (position)
  "Return the text face at POSITION."
  (or (get-text-property position 'face)
      (get-text-property position 'font-lock-face)))

(defun halo--next-face-change (position limit)
  "Return the next face property change after POSITION before LIMIT."
  (min (or (next-single-property-change position 'face nil limit) limit)
       (or (next-single-property-change position 'font-lock-face nil limit) limit)))

(defun halo--delete-overlays (&optional start end window)
  "Delete contrast overlays owned by halo.
When START and END are non-nil, also remove orphan contrast overlays in that
range.  Without START and END, remove orphan contrast overlays in the whole
buffer.  When WINDOW is non-nil, only delete overlays scoped to WINDOW."
  (let ((range-start (or start (point-min)))
        (range-end (or end (point-max))))
    (setq halo--overlays
          (delq nil
                (mapcar
                 (lambda (overlay)
                   (cond
                    ((not (overlay-buffer overlay))
                     nil)
                    ((or (null window)
                         (eq window (overlay-get overlay 'window)))
                     (delete-overlay overlay)
                     nil)
                    (t
                     overlay)))
                 halo--overlays)))
    (if window
        (dolist (overlay (overlays-in range-start range-end))
          (when (and (overlay-get overlay 'halo)
                     (eq window (overlay-get overlay 'window)))
            (delete-overlay overlay)))
      (remove-overlays range-start range-end 'halo t)
      (setq halo--overlays nil))))

(defun halo--make-overlay (start end face window)
  "Create a dimming overlay from START to END using FACE in WINDOW."
  (let ((overlay (make-overlay start end (current-buffer) nil t)))
    (overlay-put overlay 'halo t)
    (overlay-put overlay 'window window)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'priority -100)
    (overlay-put overlay 'face face)
    (push overlay halo--overlays)))

(defun halo--make-line-overlays (start end step window)
  "Create dimming overlays from START to END for contrast STEP in WINDOW."
  (let ((position start))
    (while (< position end)
      (let* ((next (halo--next-face-change position end))
             (face (halo--text-face-at position)))
        (halo--make-overlay position next (halo--dim-face face step) window)
        (setq position next)))))

(defun halo--center-line (window)
  "Return the target center line index for WINDOW."
  (round (* (1- (max 1 (window-body-height window)))
            (halo--clamp halo-center-fraction 0.0 1.0))))

(defun halo--window-table ()
  "Return a weak hash table suitable for window keyed state."
  (make-hash-table :test #'eq :weakness 'key))

(defun halo--refresh-states ()
  "Return the per-window refresh state table for this buffer."
  (or halo--refresh-states
      (setq halo--refresh-states (halo--window-table))))

(defun halo--virtual-top-margin-overlays ()
  "Return the per-window virtual top margin overlay table for this buffer."
  (or halo--virtual-top-margin-overlays
      (setq halo--virtual-top-margin-overlays (halo--window-table))))

(defun halo--refresh-state-for-window (window)
  "Return cached refresh state for WINDOW."
  (gethash window (halo--refresh-states)))

(defun halo--set-refresh-state-for-window (window state)
  "Set cached refresh STATE for WINDOW."
  (setq halo--refresh-state state)
  (puthash window state (halo--refresh-states)))

(defun halo--virtual-top-margin-overlay (window)
  "Return display-only top margin overlay for WINDOW."
  (gethash window (halo--virtual-top-margin-overlays)))

(defun halo--set-virtual-top-margin-overlay (window overlay)
  "Set display-only top margin OVERLAY for WINDOW."
  (setq halo--virtual-top-margin-overlay overlay)
  (puthash window overlay (halo--virtual-top-margin-overlays)))

(defun halo--mouse-wheel-command-p ()
  "Return non-nil when the current command came from mouse wheel scrolling."
  (memq this-command halo--mouse-wheel-commands))

(defun halo--image-display-spec-p (display)
  "Return non-nil when DISPLAY contains an image display specification."
  (cond
   ((and (consp display)
         (eq (car display) 'image))
    t)
   ((consp display)
    (catch 'image
      (dolist (entry display)
        (when (halo--image-display-spec-p entry)
          (throw 'image t)))
      nil))
   (t
    nil)))

(defun halo--display-line-has-image-display-p (start end)
  "Return non-nil when text between START and END has an image display."
  (let ((position start)
        found)
    (when (<= end start)
      (setq end (min (point-max) (1+ start))))
    (while (and (< position end)
                (not found))
      (setq found
            (halo--image-display-spec-p
             (get-char-property position 'display)))
      (setq position
            (let ((next (next-single-char-property-change
                         position 'display nil end)))
              (if (> next position)
                  next
                (1+ position)))))
    found))

(defun halo--nearby-display-line-has-image-display-p (window)
  "Return non-nil when point's display line or a neighbor has an image."
  (let* ((current-start (save-excursion
                          (vertical-motion 0 window)
                          (point)))
         (current-end (save-excursion
                        (vertical-motion 1 window)
                        (point)))
         (previous-start (save-excursion
                           (goto-char current-start)
                           (vertical-motion -1 window)
                           (point)))
         (next-end (save-excursion
                     (goto-char current-end)
                     (vertical-motion 1 window)
                     (point))))
    (when (<= current-end current-start)
      (setq current-end (min (point-max) (1+ current-start))))
    (when (<= next-end current-end)
      (setq next-end (min (point-max) (1+ current-end))))
    (or (and (< previous-start current-start)
             (halo--display-line-has-image-display-p previous-start
                                                     current-start))
        (halo--display-line-has-image-display-p current-start current-end)
        (and (< current-end next-end)
             (halo--display-line-has-image-display-p current-end next-end)))))

(defun halo--center-cursor-active-p (window)
  "Return non-nil when cursor centering should be active in WINDOW."
  (and halo-center-cursor
       (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (not (and halo-center-cursor-display-fallback
                 (halo--nearby-display-line-has-image-display-p window)))))

(defun halo--move-point-to-window-center (window)
  "Move point to the display line nearest WINDOW's configured center."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let* ((lines (halo--visible-display-lines window))
           (index (min (max 0 (1- (length lines)))
                       (halo--center-line window)))
           (range (nth index lines)))
      (when range
        (goto-char (car range))))))

(defun halo--delete-virtual-top-margin (&optional window)
  "Delete display-only top margin overlays.
When WINDOW is non-nil, delete only that window's overlay."
  (if window
      (let ((overlay (and halo--virtual-top-margin-overlays
                          (gethash window halo--virtual-top-margin-overlays))))
        (when overlay
          (delete-overlay overlay)
          (remhash window halo--virtual-top-margin-overlays))
        (when (eq overlay halo--virtual-top-margin-overlay)
          (setq halo--virtual-top-margin-overlay nil))
        (halo--delete-virtual-top-margin-orphans window))
    (when halo--virtual-top-margin-overlays
      (maphash (lambda (_window overlay)
                 (delete-overlay overlay))
               halo--virtual-top-margin-overlays)
      (clrhash halo--virtual-top-margin-overlays))
    (when halo--virtual-top-margin-overlay
      (delete-overlay halo--virtual-top-margin-overlay))
    (halo--delete-virtual-top-margin-orphans)
    (setq halo--virtual-top-margin-overlay nil)))

(defun halo--delete-virtual-top-margin-orphans (&optional window keep)
  "Delete untracked display-only top margin overlays.
When WINDOW is non-nil, delete only overlays scoped to WINDOW.  KEEP, when
non-nil, is preserved."
  (dolist (overlay (overlays-in (point-min) (point-min)))
    (when (and (not (eq overlay keep))
               (overlay-get overlay 'halo-virtual-top-margin)
               (or (null window)
                   (eq window (overlay-get overlay 'window))))
      (delete-overlay overlay))))

(defun halo--virtual-boundary-line ()
  "Return one display-only virtual boundary marker line."
  (concat
   (if halo-virtual-boundary-markers
       (propertize " " 'display
                   '(left-fringe halo--virtual-boundary-marker
                                 halo-virtual-boundary-marker))
     "")
   "\n"))

(defun halo--virtual-boundary-lines (line-count)
  "Return LINE-COUNT display-only virtual boundary marker lines."
  (mapconcat #'identity
             (make-list (max 0 line-count)
                        (halo--virtual-boundary-line))
             ""))

(defun halo--enable-empty-line-indicators ()
  "Use the halo fringe marker for real empty lines after `point-max'."
  (unless halo--empty-line-indicator-state
    (setq halo--empty-line-indicator-state
          (list (local-variable-p 'indicate-empty-lines)
                indicate-empty-lines
                (local-variable-p 'fringe-indicator-alist)
                fringe-indicator-alist)))
  (setq-local indicate-empty-lines halo-virtual-boundary-markers)
  (setq-local fringe-indicator-alist
              (cons '(empty-line . halo--virtual-boundary-marker)
                    (assq-delete-all 'empty-line
                                     (copy-sequence fringe-indicator-alist)))))

(defun halo--restore-empty-line-indicators ()
  "Restore empty-line fringe indicator state saved by halo."
  (when halo--empty-line-indicator-state
    (let ((had-local-indicate (nth 0 halo--empty-line-indicator-state))
          (indicate-value (nth 1 halo--empty-line-indicator-state))
          (had-local-fringe (nth 2 halo--empty-line-indicator-state))
          (fringe-value (nth 3 halo--empty-line-indicator-state)))
      (if had-local-indicate
          (setq-local indicate-empty-lines indicate-value)
        (kill-local-variable 'indicate-empty-lines))
      (if had-local-fringe
          (setq-local fringe-indicator-alist fringe-value)
        (kill-local-variable 'fringe-indicator-alist)))
    (setq halo--empty-line-indicator-state nil)))

(defun halo--display-lines-before-point (window &optional limit)
  "Return display lines from `point-min' to point in WINDOW.
When LIMIT is non-nil, stop counting after that many display lines."
  (let ((display-line-start
         (save-excursion
           (vertical-motion 0 window)
           (point))))
    (if (null limit)
        (max 0 (count-screen-lines (point-min) display-line-start nil window))
      (let ((max-lines (max 0 limit))
            (count 0))
        (save-excursion
          (goto-char (point-min))
          (halo--skip-invisible 1)
          (while (and (< (point) display-line-start)
                      (< count max-lines))
            (let ((before (point)))
              (vertical-motion 1 window)
              (halo--skip-invisible 1)
              (if (> (point) before)
                  (setq count (1+ count))
                (goto-char display-line-start)))))
        count))))

(defun halo--update-virtual-top-margin (window)
  "Update display-only space before the first buffer line for WINDOW."
  (if (and (halo--center-cursor-active-p window)
           halo-virtual-top-margin
           (window-live-p window)
           (eq (window-buffer window) (current-buffer)))
      (let* ((center-line (halo--center-line window))
             (line-count (max 0 (- center-line
                                   (halo--display-lines-before-point
                                    window center-line))))
             (before-string (halo--virtual-boundary-lines line-count))
             (overlay (halo--virtual-top-margin-overlay window)))
        (unless overlay
          (setq overlay
                (make-overlay (point-min) (point-min) (current-buffer) nil nil))
          (overlay-put overlay 'halo-virtual-top-margin t)
          (overlay-put overlay 'priority -101)
          (halo--set-virtual-top-margin-overlay window overlay))
        (move-overlay overlay
                      (point-min) (point-min) (current-buffer))
        (overlay-put overlay 'window window)
        (overlay-put overlay 'before-string before-string)
        (overlay-put overlay 'halo-virtual-top-margin-lines line-count)
        (halo--delete-virtual-top-margin-orphans window overlay)
        (setq halo--virtual-top-margin-overlay overlay))
    (halo--delete-virtual-top-margin window)))

(defun halo--center-recenter-line (window)
  "Return the `recenter' line that keeps point stable in WINDOW."
  (let ((overlay (halo--virtual-top-margin-overlay window)))
    (if (and overlay
             (< 0 (or (overlay-get overlay 'halo-virtual-top-margin-lines)
                      0)))
        0
      (halo--center-line window))))

(defun halo--virtual-top-margin-lines (window)
  "Return active display-only top margin line count for WINDOW."
  (let ((overlay (halo--virtual-top-margin-overlay window)))
    (if overlay
        (or (overlay-get overlay 'halo-virtual-top-margin-lines) 0)
      0)))

(defun halo--center-window (window)
  "Recenter WINDOW around point when cursor centering is enabled."
  (when (and halo-center-cursor
             (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (not (minibufferp (current-buffer))))
    (if (halo--center-cursor-active-p window)
        (progn
          (halo--update-virtual-top-margin window)
          (let ((selected (selected-window)))
            (unwind-protect
                (progn
                  (select-window window 'norecord)
                  (if (< 0 (halo--virtual-top-margin-lines window))
                      (set-window-start window (point-min) t)
                    (recenter (halo--center-recenter-line window))))
              (when (window-live-p selected)
                (select-window selected 'norecord)))))
      (halo--delete-virtual-top-margin window))))

(defun halo--skip-invisible (direction)
  "Move out of invisible text in DIRECTION when point is invisible."
  (when (invisible-p (point))
    (if (> direction 0)
        (goto-char (next-single-char-property-change
                    (point) 'invisible nil (point-max)))
      (goto-char (previous-single-char-property-change
                  (point) 'invisible nil (point-min))))))

(defun halo--visible-display-lines (window)
  "Return visible display line ranges for WINDOW.
Each element is a cons cell (START . END).  The scan advances with
`vertical-motion', so folded text is skipped in display order instead of being
visited line by line."
  (let ((end (window-end window t))
        (limit (+ (max 1 (window-body-height window)) 2))
        (count 0)
        lines)
    (save-excursion
      (goto-char (window-start window))
      (halo--skip-invisible 1)
      (while (and (< (point) end)
                  (< count limit))
        (let ((line-start (point))
              line-end)
          (vertical-motion 1 window)
          (halo--skip-invisible 1)
          (setq line-end (if (> (point) line-start)
                             (min (point) (point-max))
                           (line-beginning-position 2)))
          (push (cons line-start line-end) lines)
          (setq count (1+ count))
          (if (> line-end line-start)
              (goto-char line-end)
            (goto-char (point-max))))))
    (nreverse lines)))

(defun halo--display-line-index (position lines)
  "Return display line index for POSITION in LINES."
  (let ((index 0)
        found
        previous-end)
    (while (and lines (not found))
      (let ((range (car lines)))
        (cond
         ((and (<= (car range) position)
               (< position (cdr range)))
          (setq found index))
         ((and previous-end
               (<= previous-end position)
               (< position (car range)))
          ;; Point can sit in a display gap around empty/invisible text or at a
          ;; boundary produced by `vertical-motion'.  Use the nearest following
          ;; display line instead of falling back to the top of the window.
          (setq found index)))
        (setq previous-end (cdr range))
        (setq index (1+ index)
              lines (cdr lines))))
    (or found
        (max 0 (1- index)))))

(defun halo--refresh-state (window lines point-index)
  "Return the refresh state for WINDOW, LINES, and POINT-INDEX."
  (list window
        (window-start window)
        (window-end window t)
        (window-body-height window)
        (window-body-width window)
        (buffer-chars-modified-tick)
        point-index
        (length lines)
        (max 0 halo-radius)
        (max 1 halo-steps)
        (halo--clamp halo-min-alpha 0.0 1.0)
        halo-falloff
        (halo--clamp halo-center-fraction 0.0 1.0)
        (halo--default-foreground)
        (halo--default-background)))

(defun halo--point-centered-p (window point-index)
  "Return non-nil when point is visually at WINDOW's configured center.
When virtual top margin is active, include its display-only lines because they
shift point downward without changing its visible buffer line index."
  (or (not (halo--center-cursor-active-p window))
      (= (halo--center-line window)
         (+ point-index (halo--virtual-top-margin-lines window)))))

(defun halo--refresh (buffer window &optional force)
  "Refresh halo overlays for BUFFER in WINDOW."
  (when (and (buffer-live-p buffer)
             (window-live-p window)
             (eq (window-buffer window) buffer))
    (with-current-buffer buffer
      (setq halo--timer nil)
      (when halo-mode
        (setq halo--window window)
        (let* ((lines (halo--visible-display-lines window))
               (point-index (halo--display-line-index (point) lines))
               (state (halo--refresh-state window lines point-index))
               (index 0))
          (unless (and (not force)
                       (equal state (halo--refresh-state-for-window window)))
            (halo--set-refresh-state-for-window window state)
            (if lines
                (halo--delete-overlays
                 (caar lines) (cdar (last lines)) window)
              (halo--delete-overlays nil nil window))
            (when (halo--point-centered-p window point-index)
              (dolist (range lines)
                (let* ((line-start (car range))
                       (line-end (cdr range))
                       (distance (abs (- index point-index)))
                       (step (halo--step-for-distance distance)))
                  (when step
                    (halo--make-line-overlays line-start line-end step window))
                  (setq index (1+ index)))))))))))

(defun halo--schedule (&optional window)
  "Schedule a delayed halo refresh for the current buffer.
When WINDOW is non-nil, refresh that window instead of the selected window."
  (when halo-mode
    (when halo--timer
      (cancel-timer halo--timer))
    (let ((target-window (or window (selected-window))))
      (setq halo--window target-window)
      (setq halo--timer
            (run-with-idle-timer halo-idle-delay nil
                                 #'halo--refresh
                                 (current-buffer)
                                 target-window)))))

(defun halo--schedule-after-change (&rest _)
  "Schedule centering after non-command buffer changes such as process output."
  (when (and halo-mode
             (not halo--in-command))
    (when halo--change-timer
      (cancel-timer halo--change-timer))
    (setq halo--change-timer
          (run-at-time 0 nil
                       #'halo--after-change-update
                       (current-buffer)))))

(defun halo--after-change-update (buffer)
  "Update centering and overlays for BUFFER after a buffer change."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq halo--change-timer nil)
      (when (and halo-mode
                 (eq (window-buffer (selected-window)) buffer))
        (if halo-live-update
            (halo--update-now (selected-window) t)
          (progn
            (halo--center-window (selected-window))
            (halo--schedule)))))))

(defun halo--pre-command ()
  "Mark that command-loop changes will be handled by `halo--post-command'."
  (setq halo--in-command t))

(defun halo--post-command ()
  "Refresh overlays after commands when the selected window shows this buffer."
  (unwind-protect
      (when (eq (window-buffer (selected-window)) (current-buffer))
        (let ((window (selected-window)))
          (if (halo--mouse-wheel-command-p)
              (progn
                (when (halo--center-cursor-active-p window)
                  (halo--move-point-to-window-center window))
                (if halo-live-update
                    (halo--update-now window)
                  (progn
                    (halo--center-window window)
                    (halo--schedule window))))
            (if halo-live-update
                (halo--update-now window)
              (progn
                (halo--center-window window)
                (halo--schedule window))))))
    (setq halo--in-command nil)))

(defun halo--window-scroll (window _display-start)
  "Schedule refresh when WINDOW scrolls."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when halo-mode
        (halo--schedule window)))))

(defun halo--update-now (window &optional force)
  "Update centering and contrast overlays immediately for WINDOW."
  (halo--cancel-timer)
  (halo--center-window window)
  (halo--refresh (current-buffer) window force))

(defun halo--cancel-timer ()
  "Cancel the pending halo refresh timer."
  (when halo--timer
    (cancel-timer halo--timer)
    (setq halo--timer nil)))

(defun halo--cancel-change-timer ()
  "Cancel the pending post-change centering timer."
  (when halo--change-timer
    (cancel-timer halo--change-timer)
    (setq halo--change-timer nil)))

(defun halo--global-enable ()
  "Enable `halo-mode' in the current buffer when appropriate."
  (unless (or (minibufferp)
              (apply #'derived-mode-p halo-global-excluded-modes)
              (and halo-global-exclude-predicate
                   (funcall halo-global-exclude-predicate)))
    (halo-mode 1)))

;;;###autoload
(defun halo-refresh (&optional force)
  "Refresh halo overlays in the selected window.
With FORCE non-nil, rebuild overlays even when the cached refresh state appears
unchanged."
  (interactive "P")
  (when halo-mode
    (halo--update-now (selected-window) force)))

;;;###autoload
(defun halo-refresh-all (&optional force)
  "Refresh halo overlays in all live windows showing buffers with `halo-mode'.
With FORCE non-nil, rebuild overlays even when cached refresh states appear
unchanged."
  (interactive "P")
  (dolist (window (window-list nil 'no-minibuf))
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (when halo-mode
          (halo--update-now window force))))))

;;;###autoload
(define-minor-mode halo-mode
  "Dim visible lines progressively as they get farther from point.
The mode only processes the selected window's visible range.  It uses overlays,
so disabling the mode removes all visual changes made by this package."
  :init-value nil
  :lighter " Halo"
  :group 'halo
  (if halo-mode
      (progn
        (add-hook 'pre-command-hook #'halo--pre-command nil t)
        (add-hook 'post-command-hook #'halo--post-command nil t)
        (add-hook 'after-change-functions #'halo--schedule-after-change nil t)
        (add-hook 'window-scroll-functions #'halo--window-scroll nil t)
        (halo--enable-empty-line-indicators)
        (if halo-live-update
            (halo--update-now (selected-window))
          (progn
            (halo--center-window (selected-window))
            (halo--schedule))))
    (remove-hook 'pre-command-hook #'halo--pre-command t)
    (remove-hook 'post-command-hook #'halo--post-command t)
    (remove-hook 'after-change-functions #'halo--schedule-after-change t)
    (remove-hook 'window-scroll-functions #'halo--window-scroll t)
    (halo--cancel-timer)
    (halo--cancel-change-timer)
    (halo--delete-virtual-top-margin)
    (halo--restore-empty-line-indicators)
    (halo--delete-overlays)
    (setq halo--window nil
          halo--in-command nil
          halo--refresh-state nil
          halo--refresh-states nil
          halo--virtual-top-margin-overlays nil)))

;;;###autoload
(define-globalized-minor-mode halo-global-mode
  halo-mode
  halo--global-enable
  :group 'halo)

(provide 'halo)

;;; halo.el ends here
