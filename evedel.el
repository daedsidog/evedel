;;; evedel.el --- Instructed LLM programmer -*- lexical-binding: t; -*-

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0") (transient "0.4.0"))
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
          (let ((buffer-count (length (cl-remove-duplicates (mapcar (lambda (inst)
                                                                      (plist-get :buffer inst))
                                                                    saved-instructions)))))
            (message "Saved %d Evedel instruction%s from %d buffer%s to %s"
                     (length saved-instructions)
                     (if (= 1 (length saved-instructions)) "" "s")
                     buffer-count
                     (if (= 1 buffer-count) "" "s")
                     path)))
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

(defun eel--create-or-delete-instruction (type)
  "Create or delete an instruction of the given TYPE within the selected region.

If no region is selected, deletes the instruction at the current point, if any.

If a region is selected but partially covers an existing instruction, then the
function will resize it.  See either `evedel-create-or-delete-reference' or
`evedel-create-or-delete-directive' for details on how the resizing works."
  (if (use-region-p)
      (if-let ((instructions
                (cl-remove-if-not (lambda (inst)
                                    (eq (eel--instruction-type inst) type))
                                  (eel--partially-contained-instructions (current-buffer)
                                                                         (region-beginning)
                                                                         (region-end)))))
          (progn
            (dolist (instruction instructions)
              (if (< (overlay-start instruction) (point) (overlay-end instruction))
                  (if (< (mark) (point))
                      (setf (overlay-start instruction) (point))
                    (setf (overlay-end instruction) (point)))
                (if (> (mark) (point))
                    (setf (overlay-start instruction) (point))
                  (setf (overlay-end instruction) (point)))))
            (when instructions
              (deactivate-mark)))
        (let* ((buffer (current-buffer))
               (instruction (if (eq type 'reference)
                               (eel--create-reference-in-region buffer
                                                                (region-beginning)
                                                                (region-end))
                             (eel--create-directive-in-region buffer
                                                              (region-beginning)
                                                              (region-end)))))
          (with-current-buffer buffer (deactivate-mark))
          instruction))
    (when-let ((instruction (eel--highest-priority-instruction
                             (eel--instructions-at-point (point) type))))
      (eel--delete-instruction instruction))))

;;;###autoload
(defun eel-create-or-delete-reference ()
  "Create a reference instruction within the selected region.

If no region is selected, deletes the reference at the current point, if any.

If a region is selected but partially covers an existing reference, then the
command will instead resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is located
    OUTSIDE the reference) then the reference will be expanded to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is located
    INSIDE the reference) then the reference will be shrunk to the point."
  (interactive)
  (eel--create-or-delete-instruction 'reference))

;;;###autoload
(defun eel-create-or-delete-directive ()
  "Create a directive instruction within the selected region.

If no region is selected, deletes the directive at the current point, if any.

If a region is selected but partially covers an existing directive, then the
command will instead resize the directive in the following manner:

  - If the mark is located INSIDE the directive (i.e., the point is located
    OUTSIDE the directive) then the directive will be expanded to the point.
  - If the mark is located OUTSIDE the directive (i.e., the point is located
    INSIDE the directive) then the directive will be shrunk to the point."
  (interactive)
  (eel--create-or-delete-instruction 'directive))

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

(defun eel--update-instruction-newline-prefix (instruction)
  "Update the INSTRUCTION label prefix so it appears at the start of the line."
  (let ((before-string (overlay-get instruction 'before-string)))
    (with-current-buffer (overlay-buffer instruction)
      (save-excursion
        (goto-char (overlay-start instruction))
        (if (not (eq (point) (line-beginning-position)))
            (unless (string-prefix-p "\n" before-string)
              (overlay-put instruction 'before-string (concat "\n" before-string)))
          (when (string-prefix-p "\n" before-string)
            (overlay-put instruction 'before-string (substring before-string 1))))))))

(defun eel--update-instruction-newline-prefixes-in-region (beg end _len)
  "Fix up instruction overlay newline prefixes between BEG and END in the buffer."
  (let ((beg (max (point-min) (1- beg)))
        (end (min (point-max) (1+ end))))
    (let ((affected-instructions (eel--instructions-in-region beg end)))
      (dolist (instruction affected-instructions)
        (eel--update-instruction-newline-prefix instruction)))))

(defun eel--create-instruction-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'eel-instruction t)
      (overlay-put overlay 'evaporate t)
      (puthash overlay t eel--instructions)
      (unless (member 'eel--update-instruction-newline-prefixes-in-region
                      after-change-functions)
        (add-hook 'after-change-functions
                  'eel--update-instruction-newline-prefixes-in-region nil t))
      overlay)))

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
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution."
  (let ((ov (eel--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'eel-instruction-type 'directive)
    (overlay-put ov 'eel-directive "")
    (eel--update-instruction-overlay ov t)
    (eel--directive-prompt ov) ; Switches to another buffer
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

(defun eel--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

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
                (setq label "DIRECTIVE"))
              (let ((directive (overlay-get instruction 'eel-directive)))
                (if (zerop (length directive))
                    (setq label (concat "EMPTY " label))
                  (let ((buffer-fill-column (with-current-buffer (overlay-buffer instruction)
                                              fill-column)))
                    (with-temp-buffer
                      (insert (concat label ": " directive))
                      (let ((fill-column buffer-fill-column))
                        (fill-region-as-paragraph (point-min) (point-max)))
                      (setq label (buffer-string)))))))
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
                                                                 :inherit 'default
                                                                 :foreground label-color
                                                                 :background bg-color)))
             (eel--update-instruction-newline-prefix instruction)
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

(defun eel--instructions-in-region (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'eel-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'eel-instruction-type)
                                        type))
                               t)))
                    (overlays-in start end)))

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

(defun eel--directive-prompt (directive-instruction)
  "Split current frame vertically by 1/3 and open a directive prompt buffer buffer.

DIRECTIVE-INSTRUCTION is the overlay to associate with the buffer."
  (let ((prompt-buffer-name "*evedel-directive-prompt*"))
    ;; Close an existing prompt buffer, if any.
    (when (get-buffer prompt-buffer-name)
      (switch-to-buffer prompt-buffer-name)
      (eel--directive-abort-function t))
    (let ((new-window (split-window-vertically (- (round (* 0.333 (window-total-height)))))))
      (select-window new-window)
      (switch-to-buffer prompt-buffer-name)))
  (text-mode)
  (setq-local directive-overlay directive-instruction)
  (setq-local buffer-killable nil)
  (setq-local original-directive (overlay-get directive-instruction 'eel-directive))
  (unless (zerop (length original-directive))
    (insert original-directive))
  (setq header-line-format
        (concat " Write the directive to the LLM.  "
                (propertize "C-c C-c" 'face 'help-key-binding) ": apply, "
                (propertize "C-c C-k" 'face 'help-key-binding) ": abort."))
  (setq-local kill-buffer-query-functions
              (list (lambda ()
                      (if (not buffer-killable)
                          (user-error (concat "Do not kill this buffer.  Use "
                                              (propertize "C-c C-k" 'face 'help-key-binding)
                                              " to abort"))
                        t))))
  (add-hook 'after-change-functions
            (lambda (_beg _end _len)
              (overlay-put directive-overlay 'eel-directive (buffer-string))
              (eel--update-instruction-overlay directive-overlay))
            nil t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'eel--directive-apply-function)
    (define-key map (kbd "C-c C-k") 'eel--directive-abort-function)
    (use-local-map map)))

(defun eel--directive-apply-function ()
  "Apply change made to the directive instruction."
  (interactive)
  (make-local-variable 'directive-overlay)
  (let* ((buffer (current-buffer))
         (string (buffer-string)))
    (if (string-empty-p string)
        ;; We don't need to update the overlay because it will already be updated from the
        ;; modification hook.
        (eel--delete-instruction directive-overlay)
      (overlay-put directive-overlay 'eel-directive string))
    (setq-local buffer-killable t)
    (delete-window)
    (kill-buffer buffer)))

(defun eel--directive-abort-function (&optional no-error)
  "Abort change made to the directive instruction.

If NO-ERROR is non-nil, do not throw a user error."
  (interactive)
  (make-local-variable 'original-directive)
  (make-local-variable 'directive-overlay)
  (let ((buffer (current-buffer)))
    (if (string-empty-p original-directive)
        (eel--delete-instruction directive-overlay)
      (overlay-put directive-overlay 'eel-directive original-directive)
      (eel--update-instruction-overlay directive-overlay nil))
    (setq-local buffer-killable t)
    (delete-window)
    (kill-buffer buffer)
    (unless no-error
      (user-error "Aborted"))))

(provide 'evedel)

;; Local Variables:
;; read-symbol-shorthands: (("eel-" . "evedel-"));
;; End:

;;; evedel.el ends here.
