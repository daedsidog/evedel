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
                            (> (or (overlay-get instruction 'priority) 0)
                               (or (overlay-get acc 'priority) 0))))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun ce--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay."
  (if-let ((type (overlay-get instruction 'instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun ce--create-instruction-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (with-current-buffer buffer
    (save-excursion
      (let ((start (progn
                     (goto-char start)
                     (pos-bol)))
            (end (progn
                   (goto-char end)
                   (goto-char (pos-eol))
                   (forward-char)
                   (point))))
        (let ((partially-contained-instructions
               (ce--partially-contained-instructions buffer start end)))
          (dolist (ov partially-contained-instructions)
            (setq start (min start (overlay-start ov)))
            (setq end (max end (overlay-end ov))))
          (mapc #'ce--delete-instruction partially-contained-instructions)
          (let ((overlay (make-overlay start end))
                (bg-color (face-background 'default))
                (label-color (face-foreground 'default)))
            (overlay-put overlay 'ce-instruction t)
            (overlay-put overlay 'ce-bg-color bg-color)
            (overlay-put overlay 'ce-label-color label-color)
            (puthash overlay t ce--instructions)
            overlay))))))

(defun ce--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'ce-instruction))

(defun ce--parent-instruction (instruction)
  "Return the parent of the given INSTRUCTION overlay."
  (let ((buf (overlay-buffer instruction))
        (start (overlay-start instruction))
        (end (overlay-end instruction)))
    (with-current-buffer buf
      (catch 'found
        (dolist (ov (overlays-in start end))
          (when (and (not (eq ov instruction))
                     (> (overlay-start ov) start)
                     (< (overlay-end ov) end))
            (throw 'found ov)))
        nil))))

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

(defun ce--create-reference-in-region (name buffer start end)
  "Create a region reference from START to END with the given NAME in BUFFER."
  (let ((ov (ce--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'ce-reference-name name)
    (overlay-put ov 'ce-instruction-type 'reference)
    (ce--update-instruction-overlay ov t)
    ov))

;;;###autoload
(defun ce-create-reference (name)
  "Create a reference instruction with the given NAME."
  (interactive "sReference name: ")
  (let ((instruction (if (use-region-p)
                         (progn
                           (ce--create-reference-in-region name
                                                           (current-buffer)
                                                           (region-beginning)
                                                           (region-end))
                           (deactivate-mark))
                       (ce--create-reference-in-region name
                                                       (current-buffer)
                                                       (point-min)
                                                       (point-max)))))
    instruction))

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
      ((aux (instruction &optional update-children (priority 0) (parent nil))
         (let ((label-color
                (if parent
                    (overlay-get parent 'ce-label-color)
                  (overlay-get instruction 'ce-label-color)))
               (bg-color
                (if parent
                    (overlay-get parent 'ce-bg-color)
                  (overlay-get instruction 'ce-bg-color))))
           (let ((instruction-type (overlay-get instruction 'ce-instruction-type))
                 (color))
             ;; Instruction-specific updates
             (pcase instruction-type
               ('reference ; REFERENCE
                (setq color ce-reference-color)
                (overlay-put instruction 'before-string "REFERENCE\n"))
               ('directive ; DIRECTIVE
                (setq color ce-directive-color)
                (overlay-put instruction 'before-string "DIRECTIVE\n"))
               ('result    ; RESULT
                (setq color ce-result-color)
                (overlay-put instruction 'before-string "RESULT\n")))
             (let ((label-tint (ce--tint label-color color ce-instruction-label-tint-intensity))
                   (bg-tint (ce--tint bg-color color ce-instruction-bg-tint-intensity)))
               (overlay-put instruction
                            'before-string
                            (propertize (or (overlay-get instruction 'before-string) "")
                                        'face
                                        `(:extend t :foreground ,label-tint :background ,bg-tint)))
               (overlay-put instruction
                            'face
                            `(:extend t :background ,bg-tint)))))
         (when update-children
           (dolist (child (ce--child-instructions instruction))
             (aux child update-children (1+ priority) instruction)))))
    (aux instruction update-children (overlay-get instruction 'priority))))

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

(provide 'coderel)
;;; coderel.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("ce-" . "coderel-"));
;; End:
