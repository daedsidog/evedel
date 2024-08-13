;;; coderel.el --- Instructed LLM programmer -*- lexical-binding: t; -*-

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (transient "0.4.0") (compat "29.1.4.1"))
;; URL: https://github.com/daedsidog/coderel

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; coderel is a gptel extension Emacs package specialized for ergonomic programming with an LLM.  It
;; aims to eliminate manual aspects of software development by relegating them to an LLM.  The
;; programmer acts as an overseer over their LLM by creating various instruction overlays across
;; their project.

;;; Code:

(require 'cl-lib)

(defcustom ce-reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'coderel)

(defcustom ce-directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'coderel)

(defcustom ce-result-color "cyan"
  "Color to be used as a tint for result overlays."
  :type 'string
  :group 'coderel)

(defcustom ce-instruction-bg-tint-intensity 0.1
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'coderel)

(defcustom ce-instruction-label-tint-intensity 0.5
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'coderel)

(defvar ce--instructions (make-hash-table))
(defvar ce--default-instruction-priority -99)

(defun ce--tint (source-color-name tint-color-name &optional intensity)
  "Return hex string color of SOURCE-COLOR-NAME tinted with TINT-COLOR-NAME.

INTENSITY controls the tinting intensity, where 0 means no tinting and 1 means
that the resulting color is the same as the TINT-COLOR-NAME color."
  (let* ((tint (color-name-to-rgb tint-color-name))
         (color (color-name-to-rgb source-color-name))
         (result (cl-mapcar (lambda (color tint)
                              (+ (* (- 1.0 intensity) color)
                                 (* intensity tint)))
                            color
                            tint)))
    (apply 'color-rgb-to-hex `(,@result 2))))

(defun ce--highest-priority-instruction (instructions)
  "Return the instruction with the highest priority from the INSTRUCTIONS list."
  (cl-reduce (lambda (acc instruction)
               (if (and (overlay-get instruction 'ce-instruction)
                        (or (not acc)
                            (> (or (overlay-get instruction 'priority)
                                   ce--default-instruction-priority))
                            (or (overlay-get acc 'priority)
                                ce--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun ce--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay."
  (if-let ((type (overlay-get instruction 'ce-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun ce--create-instruction-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (with-current-buffer buffer
    (save-excursion
      (let ((partially-contained-instructions
             (ce--partially-contained-instructions buffer start end)))
        (dolist (ov partially-contained-instructions)
          (setq start (min start (overlay-start ov)))
          (setq end (max end (overlay-end ov))))
        (mapc #'ce--delete-instruction partially-contained-instructions)
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'ce-instruction t)
          (puthash overlay t ce--instructions)
          overlay)))))

(defun ce--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'ce-instruction))

(defun ce--parent-instruction (instruction)
  "Return the parent of the given INSTRUCTION overlay."
  (let ((buf (overlay-buffer instruction))
        (start (overlay-start instruction))
        (end (overlay-end instruction))
        (smallest-overlay nil))
    (with-current-buffer buf
      (let ((min-start (point-min))
            (max-end (point-max)))
        (dolist (ov (overlays-in start end))
          (let ((ov-start (overlay-start ov))
                (ov-end (overlay-end ov)))
            (when (and (not (eq ov instruction))
                       (overlay-get ov 'ce-instruction)
                       (<= min-start ov-start start)
                       (>= max-end ov-end start))
              (setq smallest-overlay ov
                    min-start ov-start
                    max-end ov-end))))))
    smallest-overlay))

(defun ce--child-instructions (instruction)
  "Return the child instructions of the given INSTRUCTION overlay."
  (let ((children (remove instruction
                          (ce--wholly-contained-instructions (overlay-buffer instruction)
                                                             (overlay-start instruction)
                                                             (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (ce--child-instructions child))))
    children))

(defun ce--create-reference-in-region (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (ce--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'ce-instruction-type 'reference)
    (ce--update-instruction-overlay ov t)
    ov))

;;;###autoload
(defun ce-create-or-delete-reference ()
  "Create a reference instruction within the selected region.

If no region is selected, deletes the reference at the current point."
  (interactive)
  (if (use-region-p)
      (let ((instruction (ce--create-reference-in-region (current-buffer)
                                                         (region-beginning)
                                                         (region-end))))
        (deactivate-mark)
        instruction)
    (when-let ((instruction (car (ce--instructions-at-point (point) 'reference))))
      (ce--delete-instruction instruction))))

(defun ce--delete-instruction (instruction)
  "Delete the INSTRUCTION overlay."
  (let ((children (ce--child-instructions instruction)))
    (remhash instruction ce--instructions)
    (delete-overlay instruction)
    (dolist (child children)
      (if (eq (overlay-get child 'ce-instruction-type)
              (overlay-get instruction 'ce-instruction-type))
          (progn
            (overlay-put child 'ce-bg-color (overlay-get instruction 'ce-bg-color))
            (overlay-put child 'ce-label-color (overlay-get instruction 'ce-label-color)))
        (overlay-put child 'ce-bg-color 'default)
        (overlay-put child 'ce-label-color 'default))
      (ce--update-instruction-overlay child t))))

(defun ce--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text, and the
background of the instruction overlay.  This function should be called every
time there is a hierarchy or status change in the instruction overlay that we
wish to reflect.

Also updates the child instructions of the INSTRUCTION, if UPDATE-CHILDREN is
non-nil."
  (unless (ce--instruction-p instruction)
    (error "%s is not an instruction overlay" instruction))
  (cl-labels
      ((aux (instruction &optional update-children priority (parent nil))
         (let ((instruction-type (ce--instruction-type instruction))
               (color)
               (label))
           ;; Instruction-specific updates
           (pcase instruction-type
             ('reference ; REFERENCE
              (setq color ce-reference-color)
              (if (and parent
                       (eq (ce--instruction-type parent) 'reference))
                  (setq label "SUBREFERENCE")
                (setq label   "REFERENCE")))
             ('directive ; DIRECTIVE
              (setq color ce-directive-color
                    label     "DIRECTIVE"))
             ('result    ; RESULT
              (setq color ce-result-color
                    label     "RESULT")))
           (let* ((parent-label-color
                  (if parent
                      (overlay-get parent 'ce-label-color)
                    (face-foreground 'default)))
                 (parent-bg-color
                  (if parent
                      (overlay-get parent 'ce-bg-color)
                    (face-background 'default)))
                 (label-color (ce--tint parent-label-color
                                        color
                                        ce-instruction-label-tint-intensity))
                 (bg-color (ce--tint parent-bg-color color ce-instruction-bg-tint-intensity)))
             (overlay-put instruction 'ce-bg-color bg-color)
             (overlay-put instruction 'ce-label-color label-color)
             (overlay-put instruction 'priority priority)
             (overlay-put instruction
                          'before-string
                          (concat "\n"
                                  (propertize
                                   (concat label "\n")
                                   'face (list :extend t
                                               :foreground label-color
                                               :background bg-color))))
             (overlay-put instruction
                          'after-string
                          (propertize "\n" 'face `(:extend t :background ,bg-color)))
             (overlay-put instruction
                          'face
                          `(:extend t :background ,bg-color))))
         (when update-children
           (dolist (child (ce--child-instructions instruction))
             (aux child update-children (1+ priority) instruction)))))
    (let ((parent (ce--parent-instruction instruction)))
      (let ((priority (if parent
                          (1+ (overlay-get parent 'priority))
                        ce--default-instruction-priority)))
        (aux instruction update-children priority parent)))))

(defun ce-delete-instruction-at-point ()
  "Delete the instruction instruction at point."
  (interactive)
  (let* ((instructions (seq-filter (lambda (ov) (overlay-get ov 'ce-instruction))
                               (overlays-at (point))))
         (target (ce--highest-priority-instruction instructions)))
    (when target
      (ce--delete-instruction target))))

(defun ce--wholly-contained-instructions (buffer start end)
  "Return coderel overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'ce-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun ce--instructions-at-point (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'ce-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'ce-instruction-type)
                                        type))
                               t)))
                    (overlays-at point)))

(defun ce--partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety the region."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'ce-instruction)
                             (or (< (overlay-start ov) start)
                                 (> (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun ce-save-instructions (path)
  "Save instructions overlays to a file PATH specified by the user."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (let* ((overlays (hash-table-keys ce--instructions))
         (saved-instructions ()))
    (dolist (ov overlays)
      (when (overlay-get ov 'ce-instruction-type)
        (let* ((buf (overlay-buffer ov))
               (file (buffer-file-name buf))
               (buf-name (buffer-name buf)))
          (when buf
            (push (list :file file
                        :buffer buf-name
                        :overlay-start (overlay-start ov)
                        :overlay-end (overlay-end ov)
                        :properties (overlay-properties ov))
                  saved-instructions)))))
    (with-temp-file path
      (prin1 saved-instructions (current-buffer))
      (message "Saved coderel instructions to %s" path))))

(defun ce-load-instructions (path)
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (cl-loop for ov being the hash-keys of ce--instructions
           unless (and (overlay-buffer ov)
                       (overlay-get ov 'ce-instruction-type))
           do (remhash ov ce--instructions))
  (when (and (not (zerop (hash-table-count ce--instructions)))
             (called-interactively-p 'interactive))
    (unless (y-or-n-p "Discard existing coderel instructions? ")
      (user-error "Aborted")))
  (let* ((loaded-instructions (with-temp-buffer
                                (insert-file-contents path)
                                (read (current-buffer)))))
    (unless (listp loaded-instructions)
      (user-error "Malformed coderel instruction list"))
    (maphash (lambda (ov _val) (delete-overlay ov)) ce--instructions)
    (setq ce--instructions (make-hash-table))
    (let ((total (length loaded-instructions))
          (restored 0))
      (dolist (instr loaded-instructions)
        (cl-destructuring-bind (&key file buffer overlay-start overlay-end properties) instr
          (cond
           ((file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (ce--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored))
           ((get-buffer buffer)
            (with-current-buffer (get-buffer buffer)
              (ce--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored)))))
      (message "Restored %d out of %d coderel instructions" restored total))))

(defun ce--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    (puthash new-ov t ce--instructions)))

(provide 'coderel)

;; Local Variables:
;; read-symbol-shorthands: (("ce-" . "coderel-"));
;; End:
;;; coderel.el ends here.
