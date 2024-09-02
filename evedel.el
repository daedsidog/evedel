
;;; evedel.el --- Instructed LLM programmer/assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
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

;; Evedel is a GPTel extension aimed at managing and processing in-code instructions.  It provides
;; functionality to define, manipulate, and process directives and references within a buffer.  Key
;; features include creating, deleting, and updating instructional overlays, as well as processing
;; responses from LLMs for directives.

;;; Code:

(require 'cl-lib)
(require 'gptel)

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

(defcustom e-directive-fail-color "red"
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
  "Assciation list between major modes and model roles.

Answers the question \"who is the model?\""
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
                                                                      (plist-get inst :buffer))
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
           ((and file (file-exists-p file))
            (with-current-buffer (find-file-noselect file)
              (e--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored))
           ((get-buffer buffer)
            (with-current-buffer (get-buffer buffer)
              (e--restore-overlay (current-buffer) overlay-start overlay-end properties))
            (cl-incf restored)))))
      (message "Restored %d out of %d Evedel instructions" restored total))))

;;;###autoload
(defun e-create-reference ()
  "Create a reference instruction within the selected region.

If a region is selected but partially covers an existing reference, then the
command will resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is located
    OUTSIDE the reference) then the reference will be expanded to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is located
    INSIDE the reference) then the reference will be shrunk to the point."
  (interactive)
  (e--create-instruction :reference))

;;;###autoload
(defun e-create-directive ()
  "Create a directive instruction within the selected region.

If a region is selected but partially covers an existing directive, then the
command will resize the directive in the following manner:

  - If the mark is located INSIDE the directive (i.e., the point is located
    OUTSIDE the directive) then the directive will be expanded to the point.
  - If the mark is located OUTSIDE the directive (i.e., the point is located
    INSIDE the directive) then the directive will be shrunk to the point."
  (interactive)
  (e--create-instruction :directive))

(defun e-modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let ((directive (e--highest-priority-instruction
                         (e--instructions-at (point) :directive))))
    (when (eq (overlay-get directive 'e-directive-status) :processing)
      (user-error "Cannot modify a directive that is being processed"))
    (e--directive-prompt directive)))

(defun e-process-directives ()
  "Send directives to model via gptel.

If a region is selected, send all directives within the region.
If a region is not selected and there is a directive under the point, send it."
  (interactive)
  (cl-labels ((execute (directive)
                (when (e--being-processed-p directive)
                  (cl-return-from execute))
                (if (e--directive-empty-p directive)
                    ;; There is no point in sending an empty directive to gptel.
                    (e--process-directive-llm-response "The directive is empty!"
                                                       (list :context directive))
                  (gptel-request (e--directive-llm-prompt directive)
                    :system (e--directive-llm-system-message directive)
                    :dry-run nil
                    :stream nil
                    :in-place nil
                    :callback #'e--process-directive-llm-response
                    :context directive)
                  (overlay-put directive 'e-directive-status :processing)
                  (e--update-instruction-overlay directive t))))
    (if (region-active-p)
        (when-let ((toplevel-directives
                    (cl-remove-duplicates
                     (mapcar (lambda (inst)
                               (e--topmost-instruction inst :directive))
                             (e--instructions-in-region (region-beginning)
                                                        (region-end)
                                                        :directive)))))
          (dolist (directive toplevel-directives)
            (execute directive))
          (let ((directive-count (length toplevel-directives)))
            (message "Sent %d directive%s to gptel for processing"
                     directive-count
                     (if (> directive-count 1) "s" ""))))
      (if-let ((directive (e--topmost-instruction (e--highest-priority-instruction
                                                    (e--instructions-at (point) :directive))
                                                   :directive)))
          (progn
            (execute directive)
            (message "Sent directive to gptel for processing"))
        (when-let ((toplevel-directives (cl-remove-duplicates
                                         (mapcar (lambda (inst)
                                                   (e--topmost-instruction inst :directive))
                                                 (e--instructions-in-region (point-min)
                                                                            (point-max)
                                                                            :directive)))))
          (dolist (dir toplevel-directives)
            (execute dir))
          (message "Sent all directives in current buffer to gptel for processing"))))))

(defun e-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were deleted.
Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (overlays-in start end))
            (when (overlay-get overlay 'e-instruction)
              (e--delete-instruction overlay)
              (setq deleted-count (1+ deleted-count))))
          (when (> deleted-count 0)
            (deactivate-mark))
          (unless (> deleted-count 0)
            (user-error "No instructions to delete within the selected region")))
      (let ((overlay (e--delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (user-error "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun e-delete-all-instructions ()
  "Delete all Evedel instructions across all buffers."
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

(defun e-convert-instructions ()
  "Convert instructions between reference and directive within the selected
region or at point.

If a region is selected, convert all instructions within the region. If no
region is selected, convert only the highest priority instruction at point.

Bodyless directives cannot be converted to references. Attempting to do so
will throw a user error."
  (interactive)
  (let* ((instructions (if (use-region-p)
                           (e--instructions-in (region-beginning)
                                               (region-end))
                         (cl-remove-if #'null
                                       (list (e--highest-priority-instruction
                                              (e--instructions-at (point)))))))
         (num-instructions (length instructions))
         (converted-directives-to-references 0)
         (converted-references-to-directives 0))
    (if (= num-instructions 0)
        (user-error "No instructions to convert")
      (dolist (instr instructions)
        (let ((start (overlay-start instr))
              (end (overlay-end instr))
              (buffer (overlay-buffer instr))
              (directive-text (overlay-get instr 'e-directive)))
          (cond
           ((e--directivep instr)
            (unless (e--bodyless-instruction-p instr)
              (e--delete-instruction instr)
              (let ((overlay (e--create-reference-in-region buffer start end)))
                (overlay-put overlay 'e-directive directive-text))
              (setq converted-directives-to-references (1+ converted-directives-to-references))))
           ((e--referencep instr)
            (e--delete-instruction instr)
            (e--create-directive-in-region buffer start end nil (or directive-text ""))
            (setq converted-references-to-directives (1+ converted-references-to-directives)))
           (t
            (user-error "Unknown instruction type")))))
      (let ((msg "Converted %d instruction%s")
            (conversion-msgs
             (delq nil
                   (list (when (> converted-directives-to-references 0)
                           (format "%d directive%s to reference%s"
                                   converted-directives-to-references
                                   (if (= converted-directives-to-references 1) "" "s")
                                   (if (= converted-directives-to-references 1) "" "s")))
                         (when (> converted-references-to-directives 0)
                           (format "%d reference%s to directive%s"
                                   converted-references-to-directives
                                   (if (= converted-references-to-directives 1) "" "s")
                                   (if (= converted-references-to-directives 1) "" "s")))))))
        (message (concat
                  msg (if conversion-msgs
                          (concat ": " (mapconcat 'identity conversion-msgs " and "))
                        ""))
                 num-instructions
                 (if (> num-instructions 1) "s" ""))
        (when (region-active-p)
          (deactivate-mark))))))

(defun e--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (e--instructions-at point))
         (target (e--highest-priority-instruction instructions)))
    (when target
      (e--delete-instruction target))))

(defun e--being-processed-p (instruction)
  "Return non-nil if the directive INSTRUCTION is being processed."
  (eq (overlay-get instruction 'e-directive-status) :processing))

(defun e--directive-empty-p (directive)
  "Check if DIRECTIVE is empty."
  (let ((directive-text (overlay-get directive 'e-directive)))
    (and directive-text (string-empty-p directive-text))))

(defun e--create-instruction (type)
  "Create or scale an instruction of the given TYPE within the selected region.

If a region is selected but partially covers an existing instruction, then the
function will resize it. See either `evedel-create-reference' or
`evedel-create-directive' for details on how the resizing works."
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
                 (instruction (if (eq type :reference)
                                  (e--create-reference-in-region buffer
                                                                 (region-beginning)
                                                                 (region-end))
                                (e--create-directive-in-region buffer
                                                               (region-beginning)
                                                               (region-end)))))
            (with-current-buffer buffer (deactivate-mark))
            instruction)))
    (when (eq type :directive)
      (prog1 (e--create-directive-in-region (current-buffer) (point) (point) t)
        (deactivate-mark)))))

(cl-defun e--process-directive-llm-response (response info)
  "Process RESPONSE string.  See `gptel-request' regarding INFO.

Removes any superfluous markup formatting and indents the response according to
the current buffer."
  (let ((directive (plist-get info :context)))
    (unless (overlay-buffer directive)
      ;; Directive is gone...
      (cl-return-from e--process-directive-llm-response))
    (cl-flet ((mark-failed (reason)
                (overlay-put directive 'e-directive-status :failed)
                (overlay-put directive 'e-directive-fail-reason reason)))
      (if (null response)
          (mark-failed (plist-get info :status))
        ;; Parse response that's delimited by Markdown code blocks.
        (if (not (string-match "```+.*\n\\(\\(?:.+\\|\n\\)+\\)\n```+" response))
            (mark-failed response)
          (let ((parsed-response (match-string 1 response)))
            (overlay-put directive 'e-directive-status :succeeded)
            (with-current-buffer (overlay-buffer directive)
              (let ((beg (overlay-start directive))
                    (end (overlay-end directive)))
                ;; Delete any child directives of the top-level directive.
                (let ((child-directives (cl-remove-if-not #'e--directivep
                                                          (e--child-instructions directive))))
                  (dolist (child-directive child-directives)
                    (e--delete-instruction child-directive)))
                (save-excursion
                  (goto-char beg)
                  ;; Insert a dummy character so that the overlay won't be deleted when we erase the
                  ;; entire region spanned by the overlay.
                  (insert " ")
                  (delete-region (1+ beg) (1+ end))
                  (insert parsed-response)
                  (let ((end (point)))
                    (setf (overlay-end directive) end)
                    (goto-char (1+ beg))
                    (backward-delete-char 1)
                    (unless (eq indent-line-function #'indent-relative)
                      (indent-region beg end))))))))))
    (e--update-instruction-overlay directive t)))

(defun e--referencep (instruction)
  (eq (e--instruction-type instruction) :reference))

(defun e--directivep (instruction)
  (eq (e--instruction-type instruction) :directive))

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

(defun e--bodyless-instruction-p (instr)
  "Returns non-nil if the instruction has a body."
  (= (overlay-start instr) (overlay-end instr)))

(cl-defun e--child-instructions (instruction)
  "Return the child direcct instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (e--bodyless-instruction-p instruction)
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
    (overlay-put ov 'e-instruction-type :reference)
    (overlay-put ov 'evaporate t)
    (e--update-instruction-overlay ov t)
    ov))

(defun e--create-directive-in-region (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.
BODYLESS controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive.  Having DIRECTIVE-TEXT be
non-nil prevents the opening of a prompt buffer."
  (let ((ov (e--create-instruction-overlay-in-region buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'e-instruction-type :directive)
    (overlay-put ov 'e-directive (or directive-text ""))
    (e--update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (e--directive-prompt ov)) ; This switches to another buffer.
    ov))

(defun e--delete-instruction (instruction)
  "Delete the INSTRUCTION overlay.

Returns the deleted instruction overlay."
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
      (e--update-instruction-overlay child t)))
  instruction)

(defun e--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

(defun e--fill-label-string (string &optional prefix-string buffer)
  "Fill STRING into its label.

If PREFIX-STRING is not nil, whitespace padding is added at the start of
every newline in STRING so that it aligns visually under PREFIX-STRING.

If BUFFER is provided, STRING will be wrapped to not overflow the fill column
of BUFFER.  Wrapping will attempt to respect word boundaries and only hyphenate
words as a last resort if a word is too long to fit on a line by itself."
  (let* ((padding (if prefix-string
                      (make-string (length prefix-string) ? )
                    ""))
         (padding-fill-column (if buffer
                                  (- (with-current-buffer buffer
                                       fill-column)
                                     (length padding))
                                nil)))
    (when (< padding-fill-column (length prefix-string))
      (setq padding-fill-column nil))
    (with-temp-buffer
      (when fill-column
        (let ((fill-column padding-fill-column))
          (insert string " ") ; The whitespace is so that large words at the EOB will be wrapped.
          (goto-char (point-min))
          (catch 'search-end
            (while t
              (beginning-of-line)
              (let ((beg (point)))
                (let (best-col-pos
                      (lineno (line-number-at-pos beg)))
                  (while (and (= (line-number-at-pos (point)) lineno)
                              (< (current-column) fill-column))
                    (setq best-col-pos (point))
                    (condition-case nil
                        (re-search-forward "\\s-+")
                      (error
                       (throw 'search-end nil))))
                  (goto-char best-col-pos)
                  (let ((eol-col (save-excursion (end-of-line) (current-column))))
                    (if (>= eol-col fill-column)
                        (progn
                          (when (bolp)
                            (forward-char (1- fill-column))
                            (insert "-"))
                          (save-excursion
                            (end-of-line)
                            (unless (>= (current-column) fill-column)
                              (delete-char 1)
                              (insert " ")))
                          (insert "\n"))
                      (forward-line)))))))))
      (goto-char (point-min))
      (forward-line)
      (while (not (eobp))
        (insert padding)
        (beginning-of-line)
        (forward-line))
      (goto-char (point-min))
      (insert prefix-string)
      (string-trim (buffer-string)))))

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
  (let ((topmost-directive nil))
    (cl-labels
        ((directive-color (directive)
           (pcase (overlay-get (if topmost-directive
                                   topmost-directive
                                 directive)
                               'e-directive-status)
             (:processing e-directive-processing-color)
             (:succeeded  e-directive-success-color)
             (:failed     e-directive-fail-color)
             (_           e-directive-color)))
         (aux (instruction &optional update-children priority (parent nil))
           (let ((instruction-type (e--instruction-type instruction))
                 (label "")
                 color)
             (pcase instruction-type
               (:reference ; REFERENCE
                (setq color e-reference-color)
                (if (and parent
                         (eq (e--instruction-type parent) :reference))
                    (setq label "SUBREFERENCE")
                  (setq label "REFERENCE")))
               (:directive ; DIRECTIVE
                (when (and (null topmost-directive) (overlay-get instruction
                                                                 'e-directive-status))
                  (setq topmost-directive instruction))
                (pcase (overlay-get instruction 'e-directive-status)
                  (:processing (setq label "PROCESSING\n"))
                  (:succeeded (setq label "SUCCEEDED\n"))
                  (:failed
                   (setq label (concat
                                (e--fill-label-string (overlay-get instruction
                                                                   'e-directive-fail-reason)
                                                      "FAILED: "
                                                      (overlay-buffer instruction))
                                "\n"))))
                (setq color (directive-color instruction))
                (let (sublabel)
                  (if (and parent
                           (e--directivep parent))
                      (setq sublabel "DIRECTIVE HINT")
                    (setq sublabel (concat sublabel "DIRECTIVE")))
                  (let ((directive (string-trim (overlay-get instruction 'e-directive))))
                    (if (string-empty-p directive)
                        (setq sublabel (concat "EMPTY " sublabel))
                      (setq sublabel (concat sublabel ": ")))
                    (setq label (concat
                                 label
                                 (e--fill-label-string directive
                                                       sublabel
                                                       (overlay-buffer instruction))))))))
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
                                      (propertize (concat
                                                   label
                                                   ;; Instructions without a body (such as bodyless
                                                   ;; directives) look nicer without a newline
                                                   ;; character after the label.
                                                   (if (e--bodyless-instruction-p instruction)
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
          (aux instruction update-children priority parent))))))

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

(defun e--instructions-in (start end &optional type)
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

(cl-defun e--topmost-instruction (instruction &optional of-type)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE.
If OF-TYPE is nil, the instruction returned is the top-level one."
  (unless instruction
    (cl-return-from e--topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction (if of-type
                                (when (eq (e--instruction-type instruction) of-type)
                                  instruction)
                              instruction)))
      (cl-labels ((parent-instr (instr)
                    (if-let ((parent (e--parent-instruction instr)))
                        (progn
                          (when (and of-type (eq of-type (e--instruction-type parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (parent-instr instruction)))))

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
          (:reference
           (e--create-reference-in-region buffer start end)
           (cl-incf recreated))
          (:directive
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
              ;; Reset the status of the directive to be normal, if it is not one already.
              (let ((status (overlay-get e--directive-overlay 'e-directive-status)))
                (when status
                  (overlay-put e--directive-overlay 'e-directive-status nil)))
              (e--update-instruction-overlay e--directive-overlay t))
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

(defun e--preview-directive-llm-prompt-at-point ()
  "Preview directive prompt at the current point.  Useful for debugging."
  (interactive)
  (let ((directive (e--topmost-instruction (car (e--instructions-at (point) :directive))
                                           :directive)))
    (let ((request-string (e--directive-llm-prompt directive)))
      (let ((bufname "*evedel-directive-preview*"))
        (with-output-to-temp-buffer bufname
          (princ (format "<!-- SYSTEM: %s -->"
                         (replace-regexp-in-string "\n"
                                                   "\n# "
                                                   (e--directive-llm-system-message directive))))
          (princ "\n\n")
          (princ request-string)
          (with-current-buffer bufname
            (markdown-mode)
            (read-only-mode 1)
            (visual-line-mode 1)
            (display-line-numbers-mode 1)
            (local-set-key (kbd "q") 'quit-window)))))))
    
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
        (if (zerop end)
            ;; If END is zero, it means that the buffer is entirely empty.
            (cl-values "start of buffer" "")
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
                     (buffer-substring-no-properties start end)))))))

(defun e--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))

(defun e--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE.

Returns the prompt as a string."
  (let* ((is-programmer (derived-mode-p 'prog-mode))
         (toplevel-references (e--foreach-instruction
                                  inst when (and (e--referencep inst)
                                                 (eq (e--topmost-instruction inst :reference) inst))
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
         (directive-toplevel-reference (e--topmost-instruction directive :reference))
         (directive-buffer (overlay-buffer directive))
         ;; Should the directive buffer have a valid file path, we should use a relative path for
         ;; the other references, assuming that they too have a valid file path.
         (directive-filename (buffer-file-name directive-buffer)))
    (cl-destructuring-bind (directive-region-info-string directive-region-string)
        (e--overlay-region-info directive)
      ;; This marking function is used to mark the prompt text so that it may later be formatted by
      ;; sections, should the need to do so will arise.
      (cl-labels ((response-directive-guide-text ()
                    (if (e--bodyless-instruction-p directive)
                      "Note that your response will be injected in the position the directive is \
embedded in, so be mindful not to return anything superfluous that surrounds the embedded \
directive."
                      "Note that your response will replace the region spanned by the embedded \
directive, so be mindful not to return anything superflous that surrounds it."))
                  (capitalize-first-letter (s)
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
                  (expanded-directive-text (directive)
                    (let ((directive-hints
                           (cl-remove-if-not (lambda (inst)
                                               (and (eq (e--instruction-type inst) :directive)
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
                            ", which correspond to:"
                            "\n\n"
                            (format "%s\n%s\n%s"
                                    markdown-delimiter
                                    directive-region-string
                                    markdown-delimiter)
                            "\n\n")))
                       (format "The directive is:\n\n%s"
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
           (concat
            "Listed below" (pcase reference-count
                             (0 " is a")
                             (1 " is a single reference and a")
                             (_ " are references and a"))
            (when is-programmer " programming")
            " directive."
            (when directive-toplevel-reference
              (format " Note that the directive is embedded within %s reference."
                      (if (> reference-count 1) "the" "a")))
           " Follow the directive and return what it asks of you.

What you return must be enclosed within a Markdown block. The content of your Markdown block will \
be parsed, and the result will then be formatted and injected into the region the directive spans \
in the buffer, replacing it."
           (when (string-empty-p directive-region-string)
             " In this case, since the directive doesn't span a region, your response will be \
injected directly instead of it, without replacing anything.")
            "\n\n"
            "If you cannot complete your directive or something is unclear to you, be it due to \
missing information or due to the directive asking something outside your abilities, do not guess \
or proceed. Instead, reply with a question or clarification that does not contain Markdown code \
blocks. A response without Markdown code blocks is invalidated, and its contents will be displayed \
to the user as a failure reason. Be very strict, and announce failure even at the slightest \
discrepancy."
            (unless (zerop reference-count)
              (format "\n\n## Reference%s%s"
                      (if (> reference-count 1) "s" "")
                      (if directive-toplevel-reference " & Directive" "")))))
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
                                            (format " with embedded directive in %s:"
                                                    directive-region-info-string)
                                          ":"))
                                "\n\n"
                                (format "%s\n%s\n%s"
                                        markdown-delimiter
                                        ref-string
                                        markdown-delimiter)
                                (when directive-toplevel-reference
                                  (concat
                                   (format "\n\nThe directive is embedded in %s"
                                           (expanded-directive-text directive))
                                   "\n\n"
                                   (response-directive-guide-text))))))))))
          (unless directive-toplevel-reference
            (insert
             (concat "\n\n"
                     "## Directive"
                     "\n\n"
                     (format "For %s, %s"
                             (instruction-path-namestring directive-buffer)
                             (expanded-directive-text directive)
                             (response-directive-guide-text)))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(provide 'evedel)
;;; evedel.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("e-" . "evedel-"));
;; End:
