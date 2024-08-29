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

;; Evedel is a GPTel extension Emacs package specialized for ergonomic programming with an LLM.  It
;; aims to eliminate manual aspects of software development by relegating them to an LLM.  The
;; programmer acts as an overseer over their LLM by creating various instruction overlays across
;; their project.

;;; Code:

(require 'cl-lib)

(defcustom e-reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'evedel)

(defcustom e-directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'evedel)

(defcustom e-directive-processing-color "cyan"
  "Color to be used as a tint for directives being processed by the model."
  :type 'string
  :group 'evedel)

(defcustom e-directive-success-color "green"
  "Color to be used as a tint for directives successfully processed by the model."
  :type 'string
  :group 'evedel)

(defcustom e-directive-error-color "red"
  "Color to be used as a tint for directives the model could not process."
  :type 'string
  :group 'evedel)

(defcustom e-instruction-bg-tint-intensity 0.1
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'evedel)

(defcustom e-instruction-label-tint-intensity 0.25
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'evedel)

(defcustom e-descriptive-mode-roles
  '((emacs-lisp-mode . "an Emacs Lisp programmer")
    (js-mode         . "a JavaScript programmer")
    (c-mode          . "a C programmer")
    (c++-mode        . "a C++ programmer")
    (lisp-mode       . "a Common Lisp programmer")
    (web-mode        . "a web developer"))
  "Assciation list between major modes and model roles."
  :type 'list
  :group 'evedel)

(defvar e--instructions ()
  "Association list mapping buffers to lists of instruction overlays.")
(defvar e--default-instruction-priority -99)

(defmacro e--foreach-instruction (instruction-binding &rest body)
  "Iterate over `e--instructions' with INSTRUCTION-BINDING as the binding.

Executes BODY inside a `cl-loop' form.

The purpose of this macro is to be able to iterate over instructions while also
making sure that the iterated instructions are valid, i.e. have an associated
buffer to the overlay.

This macro is the preferred way to iterate over instructions, as it handles all
the internal bookkeeping and cleanup."
  (declare (indent 1))
  (cl-with-gensyms (cons)
    `(progn
       ;; Remove invalid buffers from the instruction alist.
       (setq e--instructions
             (cl-loop for ,cons in e--instructions
                      do (setf (cdr ,cons)
                               (cl-delete-if-not (lambda (inst)
                                                   (bufferp (overlay-buffer inst)))
                                                 (cdr ,cons)))
                      when (cdr ,cons)
                      collect ,cons))
       (prog1 (cl-loop for ,instruction-binding
                       in (flatten-tree (mapcar #'cdr e--instructions))
                       ,@body)))))

;;;###autoload
(defun e-save-instructions (path)
  "Save instructions overlays to a file PATH specified by the user."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (let ((saved-instructions (e--foreach-instruction inst
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
(defun e-load-instructions (path)
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (when (and (e--instructions)
             (called-interactively-p 'interactive))
    (unless (y-or-n-p "Discard existing Evedel instructions? ")
      (user-error "Aborted")))
  (let* ((loaded-instructions (with-temp-buffer
                                (insert-file-contents path)
                                (read (current-buffer)))))
    (unless (listp loaded-instructions)
      (user-error "Malformed Evedel instruction list"))
    (e-delete-all-instructions)
    (let ((total (length loaded-instructions))
          (restored 0))
      (dolist (instr loaded-instructions)
        (cl-destructuring-bind (&key file buffer overlay-start overlay-end properties) instr
          (cond
           ((file-exists-p file)
            (with-current-buffer (find-file-noselect file)
              (e--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored))
           ((get-buffer buffer)
            (with-current-buffer (get-buffer buffer)
              (e--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored)))))
      (message "Restored %d out of %d Evedel instructions" restored total))))

(defun e--create-or-delete-instruction (type)
  "Create or delete an instruction of the given TYPE within the selected region.

If no region is selected, deletes the instruction at the current point, if any.

If a region is selected but partially covers an existing instruction, then the
function will resize it.  See either `evedel-create-or-delete-reference' or
`evedel-create-or-delete-directive' for details on how the resizing works."
  (if (use-region-p)
      (let ((intersecting-instructions (e--partially-contained-instructions (current-buffer)
                                                                            (region-beginning)
                                                                            (region-end))))
        (if-let ((instructions
                  (cl-remove-if-not (lambda (inst)
                                      (eq (e--instruction-type inst) type))
                                    intersecting-instructions)))
            (progn
              (dolist (instruction instructions)
                (if (< (overlay-start instruction) (point) (overlay-end instruction))
                    (if (< (mark) (point))
                        (setf (overlay-start instruction) (point))
                      (setf (overlay-end instruction) (point)))
                  (if (> (mark) (point))
                      (setf (overlay-start instruction) (point))
                    (setf (overlay-end instruction) (point))))
                (e--update-instruction-overlay instruction))
              (when instructions
                (deactivate-mark)))
          ;; Else: there are no partially contained instructions of the same type within the
          ;; region...
          (when intersecting-instructions
            ;; ...but there are intersecting instructions of another type, which is bad.
            (user-error "Cannot add intersecting instructions of different types"))
          (let* ((buffer (current-buffer))
                 (instruction (if (eq type 'reference)
                                  (e--create-reference-in-region buffer
                                                                 (region-beginning)
                                                                 (region-end))
                                (e--create-directive-in-region buffer
                                                               (region-beginning)
                                                               (region-end)))))
            (with-current-buffer buffer (deactivate-mark))
            instruction)))
    ;; Else: no region is currently selected.
    (if-let ((instruction (e--highest-priority-instruction
                           (e--instructions-at (point) type))))
        (e--delete-instruction instruction)
      (when (eq type 'directive)
        (prog1 (e--create-directive-in-region (current-buffer) (point) (point) t)
          (deactivate-mark))))))

;;;###autoload
(defun e-create-or-delete-reference ()
  "Create a reference instruction within the selected region.

If no region is selected, deletes the reference at the current point, if any.

If a region is selected but partially covers an existing reference, then the
command will instead resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is located
    OUTSIDE the reference) then the reference will be expanded to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is located
    INSIDE the reference) then the reference will be shrunk to the point."
  (interactive)
  (e--create-or-delete-instruction 'reference))

;;;###autoload
(defun e-create-or-delete-directive ()
  "Create a directive instruction within the selected region.

If no region is selected, deletes the directive at the current point, if any.

If a region is selected but partially covers an existing directive, then the
command will instead resize the directive in the following manner:

  - If the mark is located INSIDE the directive (i.e., the point is located
    OUTSIDE the directive) then the directive will be expanded to the point.
  - If the mark is located OUTSIDE the directive (i.e., the point is located
    INSIDE the directive) then the directive will be shrunk to the point."
  (interactive)
  (e--create-or-delete-instruction 'directive))

(defun e-modify-directive-at-point ()
  "Modify the directive under the point."
  (interactive)
  (when-let ((directive (e--highest-priority-instruction
                         (e--instructions-at (point) 'directive))))
    (when (overlay-get directive 'e-being-executed)
      (user-error "Cannot modify a directive that is being executed"))
    (e--directive-prompt directive)))

(defun e-execute-directives ()
  "Send directives to model via GPTel.

If a region is selected, send all directives within the region.
If a region is not selected and there is a directive under the point, send it."
  (interactive)
  (if (region-active-p)
      (when-let ((toplevel-directives
                  (cl-remove-duplicates
                   (mapcar (lambda (inst)
                             (e--toplevel-instruction inst 'directive))
                           (e--instructions-in-region (region-beginning)
                                                      (region-end)
                                                      'directive)))))
        (dolist (directive toplevel-directives)
          (e--execute-directive directive)))
    (when-let ((directive (e--toplevel-instruction (e--highest-priority-instruction
                                                    (e--instructions-at (point) 'directive))
                                                   'directive)))
    (e--execute-directive directive))))
  
(defun e-delete-instruction-at-point ()
  "Delete the instruction instruction at point."
  (interactive)
  (let* ((instructions (seq-filter (lambda (ov) (overlay-get ov 'e-instruction))
                                   (overlays-at (point))))
         (target (e--highest-priority-instruction instructions)))
    (when target
      (e--delete-instruction target))))

(defun e-delete-all-instructions ()
  "Delete all Evedel instructions."
  (interactive)
  (let ((instructions (e--instructions))
        buffers)
    (when (or (not (called-interactively-p 'interactive))
              (and (called-interactively-p 'interactive)
                   instructions
                   (y-or-n-p "Are you sure you want to delete all instructions?")))
      (when (called-interactively-p 'interactive)
        (setq buffers (cl-remove-duplicates (mapcar #'overlay-buffer instructions))))
      (mapc #'e--delete-instruction instructions)
      (setq e--instructions nil)
      (when (and instructions buffers)
        (let ((instruction-count (length instructions))
              (buffer-count (length buffers)))
          (message "Deleted %d Evedel instruction%s in %d buffer%s"
                   instruction-count
                   (if (= 1 instruction-count) "" "s")
                   buffer-count
                   (if (= 1 buffer-count) "" "s")))))))

(defun e--tint (source-color-name tint-color-name &optional intensity)
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

(defun e--highest-priority-instruction (instructions)
  "Return the instruction with the highest priority from the INSTRUCTIONS list."
  (cl-reduce (lambda (acc instruction)
               (if (and (overlay-get instruction 'e-instruction)
                        (or (not acc)
                            (> (or (overlay-get instruction 'priority)
                                   e--default-instruction-priority))
                            (or (overlay-get acc 'priority)
                                e--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun e--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay."
  (if-let ((type (overlay-get instruction 'e-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun e--create-instruction-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (make-local-variable 'e--after-change-functions-hooked)
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'e-instruction t)
      (push overlay (alist-get buffer e--instructions))
      (unless (bound-and-true-p e--after-change-functions-hooked)
        (setq-local e--after-change-functions-hooked t)
        (add-hook 'after-change-functions
                  (lambda (beg end _len)
                    (let ((beg (max (point-min) (1- beg)))
                          (end (min (point-max) (1+ end))))
                      (let ((affected-instructions (e--instructions-in-region beg end)))
                        (dolist (instruction affected-instructions)
                          (e--update-instruction-overlay instruction)))))
                  nil t))
      overlay)))

(defun e--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'e-instruction))

(defun e--parent-instruction (instruction)
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
                       (overlay-get ov 'e-instruction)
                       (<= min-start ov-start start)
                       (>= max-end ov-end start))
              (setq smallest-overlay ov
                    min-start ov-start
                    max-end ov-end))))))
    smallest-overlay))

(cl-defun e--child-instructions (instruction)
  "Return the child direcct instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (overlay-get instruction 'e-bodyless)
    (cl-return-from e--child-instructions nil))
  (let ((children (remove instruction
                          (e--wholly-contained-instructions (overlay-buffer instruction)
                                                              (overlay-start instruction)
                                                              (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (e--child-instructions child))))
    children))

(defun e--create-reference-in-region (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (e--create-instruction-overlay-in-region buffer start end)))
    (overlay-put ov 'e-instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (e--update-instruction-overlay ov t)
    ov))

(defun e--create-directive-in-region (buffer start end &optional bodyless)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.
BODYLESS controls special formatting if non-nil."
  (let ((ov (e--create-instruction-overlay-in-region buffer start end)))
    (if bodyless
        (overlay-put ov 'e-bodyless t)
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'e-instruction-type 'directive)
    (overlay-put ov 'e-directive "")
    (e--update-instruction-overlay ov (not bodyless))
    (e--directive-prompt ov) ; Switches to another buffer
    ov))

(defun e--delete-instruction (instruction)
  "Delete the INSTRUCTION overlay."
  (let ((children (e--child-instructions instruction)))
    (delq instruction (alist-get (overlay-buffer instruction) e--instructions))
    (delete-overlay instruction)
    (dolist (child children)
      (if (eq (overlay-get child 'e-instruction-type)
              (overlay-get instruction 'e-instruction-type))
          (progn
            (overlay-put child 'e-bg-color (overlay-get instruction 'e-bg-color))
            (overlay-put child 'e-label-color (overlay-get instruction 'e-label-color)))
        (overlay-put child 'e-bg-color 'default)
        (overlay-put child 'e-label-color 'default))
      (e--update-instruction-overlay child t))))

(defun e--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

(defun e--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text, and the
background of the instruction overlay.  This function should be called every
time there is a hierarchy or status change in the instruction overlay that we
wish to reflect.

Also updates the child instructions of the INSTRUCTION, if UPDATE-CHILDREN is
non-nil."
  (unless (e--instruction-p instruction)
    (error "%s is not an instruction overlay" instruction))
  (cl-labels
      ((aux (instruction &optional update-children priority (parent nil))
         (let ((instruction-type (e--instruction-type instruction))
               (label "")
               color)
           ;; Instruction-specific updates
           (pcase instruction-type
             ('reference ; REFERENCE
              (setq color e-reference-color)
              (if (and parent
                       (eq (e--instruction-type parent) 'reference))
                  (setq label "SUBREFERENCE")
                (setq label "REFERENCE")))
             ('directive ; DIRECTIVE
              (let ((being-executed (overlay-get instruction 'e-being-executed)))
                (if being-executed
                    (setq color e-directive-processing-color)
                  (setq color e-directive-color))
                (if (and parent
                         (eq (e--instruction-type parent) 'directive))
                    (setq label "DIRECTIVE HINT")
                  (when being-executed
                    (setq label "PROCESSING\n\n"))
                  (setq label (concat label "DIRECTIVE"))))
              (let ((directive (overlay-get instruction 'e-directive)))
                (if (string-empty-p directive)
                    (setq label (concat "EMPTY " label))
                  (let ((buffer-fill-column (with-current-buffer (overlay-buffer instruction)
                                              fill-column)))
                    (with-temp-buffer
                      (insert (concat label ": " directive))
                      (let ((fill-column buffer-fill-column))
                        (fill-region-as-paragraph (point-min) (point-max)))
                      (setq label (buffer-string))))))))
           (let* ((parent-label-color
                   (if parent
                       (overlay-get parent 'e-label-color)
                     (face-foreground 'default)))
                  (parent-bg-color
                   (if parent
                       (overlay-get parent 'e-bg-color)
                     (face-background 'default)))
                  (label-color (e--tint parent-label-color
                                          color
                                          e-instruction-label-tint-intensity))
                  (bg-color (e--tint parent-bg-color color e-instruction-bg-tint-intensity)))
             (overlay-put instruction 'e-bg-color bg-color)
             (overlay-put instruction 'e-label-color label-color)
             (overlay-put instruction 'priority priority)
             (let (instruction-is-at-eol
                   instruction-is-at-bol)
               (with-current-buffer (overlay-buffer instruction)
                 (save-excursion
                   (goto-char (overlay-end instruction))
                   (setq instruction-is-at-eol (= (point) (pos-eol)))
                   (goto-char (overlay-start instruction))
                   (setq instruction-is-at-bol (= (point) (pos-bol)))))
               (overlay-put instruction
                            'before-string
                            (concat (unless instruction-is-at-bol "\n")
                                    (propertize (concat label
                                                        ;; Instructions without a body (such as
                                                        ;; bodyless directives) look nicer without a
                                                        ;; newline character after the label.
                                                        (if (overlay-get instruction
                                                                         'e-bodyless)
                                                            (unless instruction-is-at-eol
                                                              "\n")
                                                          "\n"))
                                                'face (list :extend t
                                                            :inherit 'default
                                                            :foreground label-color
                                                            :background bg-color)))))
             (overlay-put instruction
                          'face
                          `(:extend t :background ,bg-color))))
         (when update-children
           (dolist (child (e--child-instructions instruction))
             (aux child update-children (1+ priority) instruction)))))
    (let ((parent (e--parent-instruction instruction)))
      (let ((priority (if parent
                          (1+ (overlay-get parent 'priority))
                        e--default-instruction-priority)))
        (aux instruction update-children priority parent)))))

(defun e--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    (push new-ov (alist-get buffer e--instructions))))

(defun e--wholly-contained-instructions (buffer start end)
  "Return Evedel overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'e-instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun e--instructions-at (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE.
Also returns bodyless overlays located right before the point."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'e-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'e-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in point
                                 (min (point-max) (1+ point)))))

(defun e--instructions-in-region (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl-remove-if-not (lambda (ov)
                      (and (overlay-get ov 'e-instruction)
                           (or (and type
                                    (eq (overlay-get ov 'e-instruction-type)
                                        type))
                               (null type))))
                    (overlays-in start end)))

(defun e--partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety the region."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'e-instruction)
                             (or (< (overlay-start ov) start)
                                 (> (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun e--instructions ()
  "Return a list of all Evedel instructions."
  (e--foreach-instruction inst collect inst))

(defun e--toplevel-instruction (instruction &optional type)
  "Return the top-level instruction containing the INSTRUCTION, if any.

If TYPE is non-nil, filter by specified instruction type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((toplevel-instruction
           (cl-reduce (lambda (acc inst)
                        (if (< (overlay-get acc 'priority)
                               (overlay-get inst 'priority))
                            acc
                          inst))
                      (e--instructions-at (overlay-start instruction))
                      :initial-value instruction)))
      (if (null type)
          toplevel-instruction
        (if (eq (overlay-get toplevel-instruction 'e-instruction-type) type)
            toplevel-instruction
          nil)))))

(defun e--recreate-instructions ()
  "Recreate all instructions.  Used for debugging purposes."
  (interactive)
  (let ((instructions (e--foreach-instruction inst
                        collect (list :start (overlay-start inst)
                                      :end (overlay-end inst)
                                      :buffer (overlay-buffer inst)
                                      :type (evedel--instruction-type inst))))
        (recreated 0))
    (e-delete-all-instructions)
    (dolist (instruction instructions)
      (cl-destructuring-bind (&key start end buffer type) instruction
        (pcase type
          ('reference
           (e--create-reference-in-region buffer start end)
           (cl-incf recreated))
          ('directive
           (e--create-directive-in-region buffer start end)
           (cl-incf recreated)))))
    (when (called-interactively-p 'interactive)
      (message "Recreated %d out of %d Evedel instructions"
               recreated
               (length instructions)))))

(defun e--directive-prompt (directive-instruction)
  "Split current frame vertically by 1/3 and open a directive prompt buffer buffer.

DIRECTIVE-INSTRUCTION is the overlay to associate with the buffer."
  (let ((prompt-buffer-name "*evedel-directive-prompt*"))
    ;; Close an existing prompt buffer, if any.
    (when (get-buffer prompt-buffer-name)
      (switch-to-buffer prompt-buffer-name)
      (e--directive-abort-function t))
    (let ((new-window (split-window-vertically (- (round (* 0.333 (window-total-height)))))))
      (select-window new-window)
      (switch-to-buffer prompt-buffer-name)))
  (text-mode)
  (setq-local e--directive-overlay directive-instruction)
  (setq-local e--buffer-killable nil)
  (setq-local e--original-directive (overlay-get directive-instruction 'e-directive))
  (unless (zerop (length e--original-directive))
    (insert e--original-directive))
  (setq header-line-format
        (concat " Write the directive to the LLM.  "
                (propertize "C-c C-c" 'face 'help-key-binding) ": apply, "
                (propertize "C-c C-k" 'face 'help-key-binding) ": abort."))
  (setq-local kill-buffer-query-functions
              (list (lambda ()
                      (if (not e--buffer-killable)
                          (user-error (concat "Do not kill this buffer.  Use "
                                              (propertize "C-c C-k" 'face 'help-key-binding)
                                              " to abort"))
                        t))))
  (add-hook 'after-change-functions
            (lambda (_beg _end _len)
              (overlay-put e--directive-overlay 'e-directive (buffer-string))
              (e--update-instruction-overlay e--directive-overlay))
            nil t)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'e--directive-apply-function)
    (define-key map (kbd "C-c C-k") 'e--directive-abort-function)
    (use-local-map map)))

(defun e--directive-apply-function ()
  "Apply change made to the directive instruction."
  (interactive)
  (make-local-variable 'e--directive-overlay)
  (let* ((buffer (current-buffer))
         (string (buffer-string)))
    (if (string-empty-p string)
        ;; We don't need to update the overlay because it will already be updated from the
        ;; modification hook.
        (e--delete-instruction e--directive-overlay)
      (overlay-put e--directive-overlay 'e-directive string))
    (setq-local e--buffer-killable t)
    (delete-window)
    (kill-buffer buffer)))

(defun e--directive-abort-function (&optional no-error)
  "Abort change made to the directive instruction.

If NO-ERROR is non-nil, do not throw a user error."
  (interactive)
  (make-local-variable 'e--original-directive)
  (make-local-variable 'e--directive-overlay)
  (let ((buffer (current-buffer)))
    (if (string-empty-p e--original-directive)
        (e--delete-instruction e--directive-overlay)
      (overlay-put e--directive-overlay 'e-directive e--original-directive)
      (e--update-instruction-overlay e--directive-overlay nil))
    (setq-local e--buffer-killable t)
    (delete-window)
    (kill-buffer buffer)
    (unless no-error
      (user-error "Aborted"))))

(defun e--descriptive-llm-mode-role (mode)
  "Derive the descriptive major mode role name from the major MODE.

Defaults to \"a helpful assistant\" if no appropriate role has been found in
the `evedel-descriptive-mode-roles' variable.

The role will default to \"a careful programmer\" if the major mode is not
listed in `evedel-descriptive-mode-roles' but is derivative from `prog-mode'."
  (if-let ((role (alist-get mode e-descriptive-mode-roles)))
      role
    (if (provided-mode-derived-p mode 'prog-mode)
        "a careful programmer"
      "a helpful assistant")))

(defun e--directive-llm-system-message (directive)
  "Craft the system message for the LLM model associated with the DIRECTIVE.

Returns the message as a string."
  (with-current-buffer (overlay-buffer directive)
    (concat "You are " (e--descriptive-llm-mode-role major-mode) ". Follow user directive.")))

(defun e--delimiting-markdown-backticks (string)
  "Return a string containing the appropriate code block backticks for STRING."
  (let ((backticks "```"))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks "`")))
    backticks))

(defun e--overlay-region-info (overlay)
  "Return region span information of OVERLAY in its buffer.

Returns two values, first being the region line & column span string in the
buffer, and the second being the content of the span itself."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay))
        start-line
        end-line
        start-column
        end-column
        start-prop
        end-prop)
    (with-current-buffer (overlay-buffer overlay)
      (without-restriction
        (save-excursion
          (goto-char start)
          (setq start-column (current-column))
          (if (bolp)
              (setq start-prop :bol)
            (if (eolp)
                (setq start-prop :eol)))
          (goto-char end)
          (setq end-column (current-column))
          (if (bolp)
              (setq end-prop :bol)
            (if (eolp)
                (setq end-prop :eol))))
        (when (and (eq start-prop :eol) (not (eq start-prop :bol)))
          (cl-incf start)
          (setq start-prop :bol))
        (when (and (eq end-prop :bol) (not (eq end-prop :eol)))
          (cl-decf end)
          (setq end-prop :eol))
        (setq start-line (line-number-at-pos start t)
              end-line (line-number-at-pos end t))
        (cl-values (format "lines %d%s-%d%s"
                      start-line
                      (if (eq start-prop :bol)
                          ""
                        (format ":%d" start-column))
                      end-line
                      (if (eq end-prop :eol)
                          ""
                        (format ":%d" end-column)))
                   (buffer-substring-no-properties start end))))))

(defun e--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))

(defun e--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE.

Returns the prompt as a string."
  (let* ((is-programmer (derived-mode-p 'prog-mode))
         (toplevel-references (e--foreach-instruction
                                  inst when (eq (e--toplevel-instruction inst 'reference) inst)
                                  collect inst))
         ;; The references in the reference alist should be sorted by their order of appearance
         ;; in the buffer.
         (reference-alist (cl-loop for reference in toplevel-references with alist = ()
                                   do (push reference (alist-get (overlay-buffer reference) alist))
                                   finally (progn
                                             (cl-loop for (_ . references) in alist
                                                      do (sort references
                                                               (lambda (x y)
                                                                 (< (overlay-start x)
                                                                    (overlay-start y)))))
                                             (cl-return alist))))
         (reference-count (length toplevel-references))
         (directive-toplevel-reference (e--toplevel-instruction directive 'reference))
         (directive-buffer (overlay-buffer directive))
         ;; Should the directive buffer have a valid file path, we should use a relative path for
         ;; the other references, assuming that they too have a valid file path.
         (directive-filename (buffer-file-name directive-buffer)))
    (cl-destructuring-bind (directive-region-info-string directive-region-string)
        (e--overlay-region-info directive)
      ;; This marking function is used to mark the prompt text so that it may later be formatted by
      ;; sections, should the need to do so will arise.
      (cl-labels ((capitalize-first-letter (s)
                    (if (> (length s) 0)
                        (concat (upcase (substring s 0 1)) (downcase (substring s 1)))
                      nil))
                  (instruction-path-namestring (buffer)
                    (if directive-filename
                        (if-let ((buffer-filename (buffer-file-name buffer)))
                            (format "file `%s`"
                                    (file-relative-name
                                     buffer-filename
                                     (file-name-parent-directory directive-filename)))
                          (format "buffer `%s`" (buffer-name buffer)))
                      (format "buffer `%s`" (buffer-name buffer))))
                  (expanded-directive-string (directive)
                    (let ((directive-hints
                           (cl-remove-if-not (lambda (inst)
                                               (and (eq (e--instruction-type inst) 'directive)
                                                    (not (eq inst directive))))
                                             (e--wholly-contained-instructions
                                              (overlay-buffer directive)
                                              (overlay-start directive)
                                              (overlay-end directive)))))
                      (concat
                       (format "%s" directive-region-info-string)
                       (if (string-empty-p directive-region-string)
                           ":"
                         (let ((markdown-delimiter
                                (e--delimiting-markdown-backticks directive-region-string)))
                           (concat
                            ", corresponding to"
                            "\n\n"
                            (format "%s\n%s\n%s"
                                    markdown-delimiter
                                    directive-region-string
                                    markdown-delimiter)
                            "\n\n")))
                       (format "with the directive being:\n\n%s"
                               (e--markdown-enquote (overlay-get directive 'e-directive)))
                       (cl-loop for hint in directive-hints
                                concat (concat
                                        "\n\n"
                                        (cl-destructuring-bind (hint-region-info _)
                                            (e--overlay-region-info hint)
                                          (format "Hint for %s:\n\n%s"
                                                  hint-region-info
                                                  (e--markdown-enquote
                                                   (overlay-get hint 'e-directive))))))))))
        (with-temp-buffer
          (insert
           (concat "Listed below" (pcase reference-count
                                    (0 " is a")
                                    (1 " is a single reference and a")
                                    (_ " are references and a"))
                   (when is-programmer " programming")
                   " directive."
                   (when directive-toplevel-reference
                     (format " Note that the directive is embedded within %s reference."
                             (if (> reference-count 1) "the" "a")))))
          (insert
           (concat
            "\n\n"
            (format "## Reference%s%s"
                    (if (> reference-count 1) "s" "")
                    (if directive-toplevel-reference " & Directive" ""))))
          (cl-loop for (buffer . references) in reference-alist
                   do (progn
                        (insert
                         (concat
                          "\n\n"
                          (format "### %s" (capitalize-first-letter
                                            (instruction-path-namestring buffer)))))
                        (dolist (ref references)
                          (cl-destructuring-bind (ref-info-string ref-string)
                              (e--overlay-region-info ref)
                            (let ((markdown-delimiter
                                   (e--delimiting-markdown-backticks ref-string)))
                              (insert
                               (concat
                                "\n\n"
                                (format "Reference in %s%s"
                                        ref-info-string
                                        (if (eq ref directive-toplevel-reference)
                                            (format " with embedded directive in %s"
                                                    directive-region-info-string)
                                          ":"))
                                "\n\n"
                                (format "%s\n%s\n%s"
                                        markdown-delimiter
                                        ref-string
                                        markdown-delimiter)
                                (when directive-toplevel-reference
                                  (concat
                                   (format "\n\nwith directive embedded in %s"
                                           (expanded-directive-string directive)))))))))))
          (unless directive-toplevel-reference
            (insert
             (concat "\n\n"
                     "## Directive"
                     "\n\n"
                     (format "For %s, %s"
                             (instruction-path-namestring directive-buffer)
                             (expanded-directive-string directive)))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(cl-defun e--execute-directive (directive)
  "Send DIRECTIVE to GPTel for evaluation."
  (let ((being-executed (overlay-get directive 'e-being-executed)))
    (when being-executed
      (cl-return-from e--execute-directive)))
  (overlay-put directive 'e-being-executed t)
  (e--update-instruction-overlay directive t))

(provide 'evedel)
;;; evedel.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("e-" . "evedel-"));
;; End:
