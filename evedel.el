;;; evedel.el --- Instructed LLM programmer -*- lexical-binding: t; -*-

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (transient "0.4.0") (compat "29.1.4.1"))
;; URL: https://github.com/daedsidog/evedel

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

;; Evedel is a gptel extension Emacs package specialized for ergonomic programming with an LLM.  It
;; aims to eliminate manual aspects of software development by relegating them to an LLM.  The
;; programmer acts as an overseer over their LLM by creating various instruction overlays across
;; their project.

;;; Code:

(require 'cl-lib)

(defcustom eel-reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'evedel)

(defcustom eel-directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'evedel)

(defcustom eel-result-color "cyan"
  "Color to be used as a tint for result overlays."
  :type 'string
  :group 'evedel)

(defcustom eel-instruction-bg-tint-intensity 0.1
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'evedel)

(defcustom eel-instruction-label-tint-intensity 0.5
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'evedel)

(defvar eel--instructions (make-hash-table))
(defvar eel--default-instruction-priority -99)

(defmacro eel--foreach-instruction (instruction-binding &rest body)
  "Iterate over `eel--instructions' with INSTRUCTION-BINDING as the binding.

Executes BODY inside a `cl-loop' form.

The purpose of this macro is to be able to iterate over instructions while also
making sure that the iterated instructions are valid, i.e. have an associated
buffer to the overlay.

This macro is the preferred way to iterate over instructions, as it handles all
the internal bookkeeping."
  (declare (indent 1))
  (cl-with-gensyms (marked-for-deletion malformed-inst)
    `(let ((,marked-for-deletion ()))
       (prog1 (cl-loop for ,instruction-binding being the hash-keys of eel--instructions
                       if (overlay-buffer ,instruction-binding)
                       ,@body
                       else do (push ,instruction-binding ,marked-for-deletion)
                       end)
         (cl-loop for ,malformed-inst in ,marked-for-deletion
                  do (remhash ,malformed-inst eel--instructions))))))

;;;###autoload
(defun eel-create-or-delete-reference ()
  "Create a reference instruction within the selected region.

If no region is selected, deletes the reference at the current point, if any."
  (interactive)
  (if (use-region-p)
      (let ((instruction (eel--create-reference-in-region (current-buffer)
                                                         (region-beginning)
                                                         (region-end))))
        (deactivate-mark)
        instruction)
    (when-let ((instruction (car (eel--instructions-at-point (point) 'reference))))
      (eel--delete-instruction instruction))))

;;;###autoload
(defun eel-save-instructions (path)
  "Save instructions overlays to a file PATH specified by the user."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (let ((saved-instructions (eel--foreach-instruction inst
                              collect (list :file (buffer-file-name (overlay-buffer inst))
                                            :buffer (buffer-name (overlay-buffer inst))
                                            :overlay-start (overlay-start inst)
                                            :overlay-end (overlay-end inst)
                                            :properties (overlay-properties inst)))))
    (if saved-instructions
        (with-temp-file path
          (prin1 saved-instructions (current-buffer))
          (message "Saved %d Evedel instruction%s to %s"
                   (length saved-instructions)
                   (if (= 1 (length saved-instructions)) "" "s")
                   path))
      (message "No Evedel instructions to save"))))

;;;###autoload
(defun eel-load-instructions (path)
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (when (and (eel--instructions)
             (called-interactively-p 'interactive))
    (unless (y-or-n-p "Discard existing Evedel instructions? ")
      (user-error "Aborted")))
  (let* ((loaded-instructions (with-temp-buffer
                                (insert-file-contents path)
                                (read (current-buffer)))))
    (unless (listp loaded-instructions)
      (user-error "Malformed Evedel instruction list"))
    (eel-delete-all-instructions)
    (let ((total (length loaded-instructions))
          (restored 0))
      (dolist (instr loaded-instructions)
        (cl-destructuring-bind (&key file buffer overlay-start overlay-end properties) instr
          (cond
           ((file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (eel--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored))
           ((get-buffer buffer)
            (with-current-buffer (get-buffer buffer)
              (eel--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored)))))
      (message "Restored %d out of %d Evedel instructions" restored total))))

;;;###autoload
(defun eel-create-or-delete-directive ()
  "Create a directive instruction within the selected region.

If no region is selected, deletes the directive at the current point, if any.

If no region is selected and no directive is found at the point to remove, then
then a bodyless directive will be created at the current point."
  (interactive)
  (if (use-region-p)
      (let ((instruction (eel--create-directive-in-region (current-buffer)
                                                          (region-beginning)
                                                          (region-end))))
        (deactivate-mark)
        instruction)
    (when-let ((instruction (car (eel--instructions-at-point (point) 'directive))))
      (eel--delete-instruction instruction))))

(defun eel-delete-instruction-at-point ()
  "Delete the instruction instruction at point."
  (interactive)
  (let* ((instructions (seq-filter (lambda (ov) (overlay-get ov 'eel-instruction))
                               (overlays-at (point))))
         (target (eel--highest-priority-instruction instructions)))
    (when target
      (eel--delete-instruction target))))

(defun eel-delete-all-instructions ()
  "Delete all Evedel instructions."
  (interactive)
  (let ((instructions (eel--instructions))
        buffers)
    (when (or (not (called-interactively-p 'interactive))
              (and (called-interactively-p 'interactive)
                   instructions
                   (y-or-n-p "Are you sure you want to delete all instructions?")))
      (when (called-interactively-p 'interactive)
        (setq buffers (cl-remove-duplicates (mapcar #'overlay-buffer instructions))))
      (mapc #'eel--delete-instruction instructions)
      (clrhash eel--instructions)
      (when (and instructions buffers)
        (let ((instruction-count (length instructions))
              (buffer-count (length buffers)))
          (message "Deleted %d Evedel instruction%s in %d buffer%s"
                   instruction-count
                   (if (= 1 instruction-count) "" "s")
                   buffer-count
                   (if (= 1 buffer-count) "" "s")))))))

(defun eel--tint (source-color-name tint-color-name &optional intensity)
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

(defun eel--highest-priority-instruction (instructions)
  "Return the instruction with the highest priority from the INSTRUCTIONS list."
  (cl-reduce (lambda (acc instruction)
               (if (and (overlay-get instruction 'eel-instruction)
                        (or (not acc)
                            (> (or (overlay-get instruction 'priority)
                                   eel--default-instruction-priority))
                            (or (overlay-get acc 'priority)
                                eel--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun eel--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay."
  (if-let ((type (overlay-get instruction 'eel-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun eel--create-instruction-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (with-current-buffer buffer
    (save-excursion
      (let ((partially-contained-instructions
             (eel--partially-contained-instructions buffer start end)))
        (dolist (ov partially-contained-instructions)
          (setq start (min start (overlay-start ov)))
          (setq end (max end (overlay-end ov))))
        (mapc #'eel--delete-instruction partially-contained-instructions)
        (let ((overlay (make-overlay start end)))
          (overlay-put overlay 'eel-instruction t)
          (overlay-put overlay 'evaporate t)
          (puthash overlay t eel--instructions)
          overlay)))))

(defun eel--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'eel-instruction))

(defun eel--parent-instruction (instruction)
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
                       (overlay-get ov 'eel-instruction)
                       (<= min-start ov-start start)
                       (>= max-end ov-end start))
              (setq smallest-overlay ov
                    min-start ov-start
                    max-end ov-end))))))
    smallest-overlay))

(defun eel--child-instructions (instruction)
  "Return the child instructions of the given INSTRUCTION overlay."
  (let ((children (remove instruction
                          (eel--wholly-contained-instructions (overlay-buffer instruction)
                                                             (overlay-start instruction)
                                                             (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (eel--child-instructions child))))
    children))

(defun eel--create-reference-in-region (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (eel--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'eel-instruction-type 'reference)
    (eel--update-instruction-overlay ov t)
    ov))

(defun eel--create-directive-in-region (buffer start end)
  "Create a region directive from START to END in BUFFER."
  (let ((ov (eel--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'eel-instruction-type 'directive)
    (eel--update-instruction-overlay ov t)
    ov))

(defun eel--delete-instruction (instruction)
  "Delete the INSTRUCTION overlay."
  (let ((children (eel--child-instructions instruction)))
    (remhash instruction eel--instructions)
    (delete-overlay instruction)
    (dolist (child children)
      (if (eq (overlay-get child 'eel-instruction-type)
              (overlay-get instruction 'eel-instruction-type))
          (progn
            (overlay-put child 'eel-bg-color (overlay-get instruction 'eel-bg-color))
            (overlay-put child 'eel-label-color (overlay-get instruction 'eel-label-color)))
        (overlay-put child 'eel-bg-color 'default)
        (overlay-put child 'eel-label-color 'default))
      (eel--update-instruction-overlay child t))))

(defun eel--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text, and the
background of the instruction overlay.  This function should be called every
time there is a hierarchy or status change in the instruction overlay that we
wish to reflect.

Also updates the child instructions of the INSTRUCTION, if UPDATE-CHILDREN is
non-nil."
  (unless (eel--instruction-p instruction)
    (error "%s is not an instruction overlay" instruction))
  (cl-labels
      ((aux (instruction &optional update-children priority (parent nil))
         (let ((instruction-type (eel--instruction-type instruction))
               (color)
               (label))
           ;; Instruction-specific updates
           (pcase instruction-type
             ('reference ; REFERENCE
              (setq color eel-reference-color)
              (if (and parent
                       (eq (eel--instruction-type parent) 'reference))
                  (setq label "SUBREFERENCE")
                (setq label "REFERENCE")))
             ('directive ; DIRECTIVE
              (setq color eel-directive-color)
              (if (and parent
                       (eq (eel--instruction-type parent) 'directive))
                  (setq label "SUBDIRECTIVE")
                (setq label "DIRECTIVE")))
             ('result    ; RESULT
              (setq color eel-result-color
                    label "RESULT")))
           (let* ((parent-label-color
                  (if parent
                      (overlay-get parent 'eel-label-color)
                    (face-foreground 'default)))
                 (parent-bg-color
                  (if parent
                      (overlay-get parent 'eel-bg-color)
                    (face-background 'default)))
                 (label-color (eel--tint parent-label-color
                                        color
                                        eel-instruction-label-tint-intensity))
                 (bg-color (eel--tint parent-bg-color color eel-instruction-bg-tint-intensity)))
             (overlay-put instruction 'eel-bg-color bg-color)
             (overlay-put instruction 'eel-label-color label-color)
             (overlay-put instruction 'priority priority)
             (overlay-put instruction
                          'before-string (propertize (concat label "\n")
                                                     'face (list :extend t
                                                                 :foreground label-color
                                                                 :background bg-color)))
             (overlay-put instruction
                          'face
                          `(:extend t :background ,bg-color))))
         (when update-children
           (dolist (child (eel--child-instructions instruction))
             (aux child update-children (1+ priority) instruction)))))
    (let ((parent (eel--parent-instruction instruction)))
      (let ((priority (if parent
                          (1+ (overlay-get parent 'priority))
                        eel--default-instruction-priority)))
        (aux instruction update-children priority parent)))))

(defun eel--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    (puthash new-ov t eel--instructions)))

(defun eel--wholly-contained-instructions (buffer start end)
  "Return Evedel overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'eel-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun eel--instructions-at-point (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'eel-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'eel-instruction-type)
                                        type))
                               t)))
                    (overlays-at point)))

(defun eel--partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety the region."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'eel-instruction)
                             (or (< (overlay-start ov) start)
                                 (> (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun eel--instructions ()
  "Return a list of all Evedel instructions."
  (eel--foreach-instruction inst collect inst))

(defun eel--recreate-instructions ()
  "Recreate all instructions.  Used for debugging purposes."
  (interactive)
  (let ((instructions (eel--foreach-instruction inst
                        collect (list :start (overlay-start inst)
                                      :end (overlay-end inst)
                                      :buffer (overlay-buffer inst)
                                      :type (evedel--instruction-type inst))))
        (recreated 0))
    (eel-delete-all-instructions)
    (dolist (instruction instructions)
      (cl-destructuring-bind (&key start end buffer type) instruction
        (pcase type
          ('reference
           (eel--create-reference-in-region buffer start end)
           (cl-incf recreated))
          ('directive
           (eel--create-directive-in-region buffer start end)
           (cl-incf recreated)))))
    (when (called-interactively-p 'interactive)
      (message "Recreated %d out of %d Evedel instructions"
               recreated
             (length instructions)))))

(provide 'evedel)

;; Local Variables:
;; read-symbol-shorthands: (("eel-" . "evedel-"));
;; End:

;;; evedel.el ends here.
