;;; evedel.el --- Instructed LLM programmer/assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.4.9
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0"))
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

;; Evedel is a gptel extension aimed at managing and processing in-code instructions.  It provides
;; functionality to define, manipulate, and process directives and references within a buffer.  Key
;; features include creating, deleting, and updating instructional overlays, as well as processing
;; responses from LLMs for directives.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'gptel)

(defgroup evedel nil
  "Customization group for Evedel."
  :group 'tools)

(defcustom e:reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string)

(defcustom e:directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string)

(defcustom e:directive-processing-color "cyan"
  "Color to be used as a tint for directives being processed by the model."
  :type 'string)

(defcustom e:directive-success-color "green"
  "Color to be used as a tint for directives successfully processed by the model."
  :type 'string)

(defcustom e:directive-fail-color "red"
  "Color to be used as a tint for directives the model could not process."
  :type 'string)

(defcustom e:highlighted-instruction-color "cyan"
  "Color for currently highlighted instructions."
  :type 'string)

(defcustom e:instruction-bg-tint-intensity 0.075
  "Default intensity for background tinting of instructions."
  :type 'float)

(defcustom e:instruction-label-tint-intensity 0.2
  "Default intensity for label tinting of instructions."
  :type 'float)

(defcustom e:highlighted-instruction-tint-intensity 0.2
  "Default intensity for tinting of highlighted instructions."
  :type 'float)

(defcustom e:subinstruction-tint-coefficient 0.4
  "Coeffecient multiplied by by tint intensities.

Only applicable to the subinstructions. Makes it possible to have more a
more finely-tuned control over how tinting looks.

Does not affect the label colors, just the backgrounds."
  :type 'float)

(defcustom e:empty-tag-query-matches-all t
  "Determines behavior of directives without a tag search query.

If set to t, directives without a specific tag search query will use all
available references.  Alternatively, if this is set to nil, directives without 
a search query will not use any references."
  :type 'boolean)

(defcustom e:always-match-untagged-references t
  "Controls inclusion of untagged references in directive prompts.

When set to t, untagged references are always incorporated into directive
references, ensuring comprehensive coverage.  Conversely, when set to nil,
untagged references are ignored, unless `evedel-empty-tag-query-matches-all'
is set to t."
  :type 'boolean)

(defcustom e:patch-outdated-instructions t
  "Automatically patch instructions when the save file is outdated if non-nil."
  :type 'boolean
  :group 'evedel)

(defcustom e:descriptive-mode-roles
  '((emacs-lisp-mode . "an Emacs Lisp programmer")
    (js-mode         . "a JavaScript programmer")
    (c-mode          . "a C programmer")
    (c++-mode        . "a C++ programmer")
    (lisp-mode       . "a Common Lisp programmer")
    (web-mode        . "a web developer"))
  "Assciation list between major modes and model roles.

Answers the question \"who is the model?\""
  :type 'list)

(defvar e::instructions ()
  "Association list mapping buffers or files to lists of instruction overlays.")
(defvar e::default-instruction-priority -99)
(defvar e::highlighted-instruction nil)
(defvar e::inhibit-file-restoration nil
  "If t, `evedel--restore-file-instructions' becomes inert.
This is sometimes necessary to prevent various hooks from interfering with the
instruction restoration process.")
(defvar e::version "v0.4.9")

(defmacro e::foreach-instruction (binding &rest body)
  "Iterate over `evedel--instructions' with BINDING as the binding.

Executes BODY inside an existing `cl-loop' form, which means that the macro is
expecting for BODY to be written in the `cl-loop' DSL.

BINDING can either be a symbol to bind the instruction to, or a
list where the `car' is the symbol binding and the `cadr' is a buffer.

If the buffer inside BINDING is non-nil, only iterate over the instructions
that are located inside that buffer.

The purpose of this macro is to be able to iterate over instructions
while also making sure that the iterated instructions are valid, i.e.
have an associated buffer to the overlay.

This macro is the preferred way to iterate over instructions, as it 
handles all the internal bookkeeping and cleanup."
  (declare (indent 1))
  ;; "bof" stands for "buffer or file".
  (cl:with-gensyms (cons bof specific-buffer)
    (let ((instr (if (listp binding) (car binding) binding)))
      `(cl:labels ((clean-alist-entry (cons)
                     (let ((instrs (cl:remove-if-not #'overlay-buffer (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (if (not ,specific-buffer)
               (cl:loop for ,cons in e::instructions
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (e::restore-file-instructions ,bof)
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when-let ((cons (assoc ,specific-buffer e::instructions)))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setq e::instructions (cl:remove-if (lambda (cons)
                                                 (null (cdr cons)))
                                               e::instructions))
           ;; The instructions alist should now be cleaned of deleted instructions.
           (cl:loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer e::instructions)
                         (flatten-tree
                          (cl:remove nil
                                     (mapcar (lambda (plist-or-instrs)
                                               (if (plist-get plist-or-instrs :instructions)
                                                   nil ; Plist
                                                 plist-or-instrs))
                                             (mapcar #'cdr e::instructions)))))
                    ,@body))))))

;;;###autoload
(defun e:version (&optional here message)
  "Return the current version of Evedel.

Interactively, or when MESSAGE is non-nil, show it in echo area.  With prefix
argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version e::version))
    (cond
     ((or message (called-interactively-p 'any)) (message "Evedel %s" version))
     (here (insert (format "Evedel %s" version)))
     (t version))))

;;;###autoload
(defun e:save-instructions (path)
  "Save instructions overlays to a file PATH specified by the user.

Instructions are only saved if they are associated with a buffer that has an
associated file on disk.  In other words, instructions in ethereal buffers are
not saved."
  (interactive (list (read-file-name "Save instruction list to file: ")))
  (let ((file-alist ())
        (saved-instruction-count 0))
    (cl:loop for cons in e::instructions
             if (bufferp (car cons))
             do (let ((buffer (car cons)))
                  (when-let ((buffer-file-name (buffer-file-name buffer)))
                    (let ((file (file-relative-name buffer-file-name
                                                    (file-name-directory path))))
                      (when-let ((instrs (e::stashed-buffer-instructions buffer)))
                        (let ((original-content
                               (with-current-buffer buffer
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                          (push (cons file
                                      (list :original-content original-content
                                            :instructions instrs))
                                file-alist))
                        (cl:incf saved-instruction-count (length instrs))))))
             else do
             (push cons file-alist)
             (cl:incf saved-instruction-count (length (plist-get (cdr cons) :instructions))))
    (if (not (zerop saved-instruction-count))
        (with-temp-file path
          (let ((save-file ()))
            (setf save-file (plist-put save-file :version (e:version)))
            (setf save-file (plist-put save-file :ids (list :id-counter e::id-counter
                                                            :id-usage-map e::id-usage-map
                                                            :retired-ids e::retired-ids)))
            (setf save-file (plist-put save-file :files file-alist))
            (prin1 save-file (current-buffer)))
          (let ((file-count (length file-alist)))
            (message "Wrote %d Evedel instruction%s from %d file%s to %s"
                     saved-instruction-count
                     (if (= 1 saved-instruction-count) "" "s")
                     file-count
                     (if (= 1 file-count) "" "s")
                     path)))
      (when (called-interactively-p 'any)
        (message "No Evedel instructions to save")))))

;;;###autoload
(defun e:load-instructions (path)
  "Load instruction overlays from a file specified by PATH."
  (interactive (list (read-file-name "Instruction list file: ")))
  (when (and (e::instructions)
             (called-interactively-p 'any))
    (unless (y-or-n-p "Discard existing Evedel instructions? ")
      (user-error "Aborted")))
  (let* ((save-file (e::patch-save-file (with-temp-buffer
                                          (insert-file-contents path)
                                          (read (current-buffer)))))
         (file-alist (plist-get save-file :files))
         (id-counter-plist (plist-get save-file :ids)))
    (unless (listp file-alist)
      (user-error "Malformed Evedel instruction list"))
    (e:delete-all-instructions)
    (cl:destructuring-bind (&key id-counter id-usage-map retired-ids) id-counter-plist
      (setq e::id-counter id-counter
            e::id-usage-map id-usage-map
            e::retired-ids retired-ids))
    (setq e::instructions file-alist)
    (cl:loop for cons in e::instructions
             do (when (stringp (car cons))
                  (setf (car cons)
                        ;; We want to turn the relative paths of the save file to be absolute paths
                        ;; that we will be able to handle.
                        (expand-file-name (car cons) (file-name-parent-directory path)))))
    (let ((total-restored 0)
          (total-kia 0)
          (total (cl:reduce #'+
                         (mapcar #'length
                                 (mapcar (lambda (plist)
                                           (plist-get plist :instructions))
                                         (mapcar #'cdr e::instructions))))))
      (cl:loop for (file . _) in e::instructions
               do (progn
                    (cl:multiple-value-bind (restored kia) (e::restore-file-instructions file t)
                      (cl:incf total-restored restored)
                      (cl:incf total-kia kia))))
      (when (called-interactively-p 'any)
        (message "Restored %d out of %d instructions from %s%s"
                 total-restored
                 total
                 (expand-file-name path)
                 (if (not (zerop total-kia))
                     (format ", with %d lost to patching" total-kia)
                   ""))))))

;;;###autoload
(defun e:instruction-count ()
  "Return the number of instructions currently loaded instructions.

If called interactively, it messages the number of instructions and buffers."
  (interactive)
  (let ((count 0)
        (buffer-hash (make-hash-table :test 'eq)))
    (e::foreach-instruction instr count instr into instr-count
                            do (puthash (overlay-buffer instr) t buffer-hash)
                            finally (setf count instr-count))
    (let ((buffers (hash-table-count buffer-hash)))
      (when (called-interactively-p 'interactive)
          (if (= count 0)
              (message "No Evedel instructions currently loaded")
            (message "Evedel is showing %d instruction%s from %d buffer%s"
                     count (if (/= count 1) "s" "")
                     buffers (if (/= buffers 1) "s" ""))))
      count)))

;;;###autoload
(defun e:create-reference ()
  "Create a reference instruction within the selected region.

If a region is selected but partially covers an existing reference, then the
command will resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is located
    OUTSIDE the reference) then the reference will be expanded to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is located
    INSIDE the reference) then the reference will be shrunk to the point."
  (interactive)
  (e::create-instruction 'reference))

;;;###autoload
(defun e:create-directive ()
  "Create a directive instruction within the selected region.

If a region is selected but partially covers an existing directive, then the
command will resize the directive in the following manner:

  - If the mark is located INSIDE the directive (i.e., the point is located
    OUTSIDE the directive) then the directive will be expanded to the point.
  - If the mark is located OUTSIDE the directive (i.e., the point is located
    INSIDE the directive) then the directive will be shrunk to the point."
  (interactive)
  (e::create-instruction 'directive))

(defun e:cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when instructions
overlap to the point where no other reasonable option is available."
  (interactive "d")
  (let ((instructions-at-point (e::instructions-at point))
        (original-highlighted-instruction e::highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq e::highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (message "No instructions at point")))
     ((or (null e::highlighted-instruction)
          (not (memq e::highlighted-instruction instructions-at-point)))
      (setq e::highlighted-instruction nil)
      (setq e::highlighted-instruction (e::highest-priority-instruction instructions-at-point)))
     (t
      (if-let ((parent (e::parent-instruction e::highlighted-instruction)))
          (setq e::highlighted-instruction parent)
        (setq e::highlighted-instruction nil))))
    (when e::highlighted-instruction
      (e::update-instruction-overlay e::highlighted-instruction))
    (when original-highlighted-instruction
      (e::update-instruction-overlay original-highlighted-instruction))
    e::highlighted-instruction))

(defun e:modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let ((directive (e::highest-priority-instruction (e::instructions-at (point) 'directive)
                                                         t)))
    (when (eq (overlay-get directive 'e:directive-status) 'processing)
      (overlay-put directive 'e:directive-status nil))
    (e::read-directive directive)))

(defun e:modify-reference-commentary ()
  "Modify the reference commentary under the point."
  (interactive)
  (when-let ((reference (e::highest-priority-instruction (e::instructions-at (point) 'reference)
                                                         t)))
    (e::read-commentary reference)))

(defun e:process-directives ()
  "Send directives to model via gptel.

If a region is selected, send all directives within the region.
If a region is not selected and there is a directive under the point, send it."
  (interactive)
  (cl:labels ((execute (directive)
                (when (e::being-processed-p directive)
                  (cl:return-from execute))
                (if (e::directive-empty-p directive)
                    ;; There is no point in sending an empty directive to gptel.
                    (e::process-directive-llm-response "The directive is empty!"
                                                       (list :context directive))
                  (gptel-request (e::directive-llm-prompt directive)
                    :system (e::directive-llm-system-message directive)
                    :dry-run nil
                    :stream nil
                    :in-place nil
                    :callback #'e::process-directive-llm-response
                    :context directive)
                  (overlay-put directive 'e:directive-status 'processing)
                  (e::update-instruction-overlay directive t))))
    (if (region-active-p)
        (when-let ((toplevel-directives
                    (cl:remove-duplicates
                     (mapcar (lambda (inst)
                               (e::topmost-instruction inst 'directive))
                             (e::instructions-in (region-beginning)
                                                 (region-end)
                                                 'directive)))))
          (dolist (directive toplevel-directives)
            (execute directive))
          (let ((directive-count (length toplevel-directives)))
            (message "Sent %d directive%s to gptel for processing"
                     directive-count
                     (if (> directive-count 1) "s" ""))))
      (if-let ((directive (e::topmost-instruction (e::highest-priority-instruction
                                                   (e::instructions-at (point) 'directive)
                                                   t)
                                                  'directive)))
          (progn
            (execute directive)
            (message "Sent directive to gptel for processing"))
        (when-let ((toplevel-directives (cl:remove-duplicates
                                         (mapcar (lambda (inst)
                                                   (e::topmost-instruction inst 'directive))
                                                 (e::instructions-in (point-min)
                                                                            (point-max)
                                                                            'directive)))))
          (dolist (dir toplevel-directives)
            (execute dir))
          (message "Sent all directives in current buffer to gptel for processing"))))))

(defun e:delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were deleted.
Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (e::wholly-contained-instructions (current-buffer) start end))
            (when (overlay-get overlay 'e:instruction)
              (e::delete-instruction overlay)
              (setq deleted-count (1+ deleted-count))))
          (when (> deleted-count 0)
            (deactivate-mark))
          (unless (> deleted-count 0)
            (user-error "No instructions to delete within the selected region")))
      (let ((overlay (e::delete-instruction-at (point))))
        (when overlay
          (setq deleted-count 1))
        (unless overlay
          (user-error "No instruction to delete at point"))))
    (when (> deleted-count 0)
      (message "Deleted %d instruction%s" deleted-count (if (> deleted-count 1) "s" "")))))

(defun e:delete-all-instructions ()
  "Delete all Evedel instructions across all buffers."
  (interactive)
  (let ((instr-count (length (e::instructions))))
    (when (and (called-interactively-p 'any)
               (zerop instr-count))
      (user-error "No instructions to delete"))
    (when (and (called-interactively-p 'any)
               instr-count
               (not (y-or-n-p "Are you sure you want to delete all instructions?")))
      (user-error "Aborted")))
  (let ((buffer-count 0)
        (deleted-instr-count 0))
    (e::foreach-instruction instr
      with buffer-hash = (make-hash-table)
      unless (gethash (overlay-buffer instr) buffer-hash)
      do (progn
           (puthash (overlay-buffer instr) t buffer-hash)
           (cl:incf buffer-count))
      do (progn
           (e::delete-instruction instr)
           (cl:incf deleted-instr-count)))
    (setq e::instructions nil)
    (when (not (zerop deleted-instr-count))
      (message "Deleted %d Evedel instruction%s in %d buffer%s"
               deleted-instr-count
               (if (= 1 deleted-instr-count) "" "s")
               buffer-count
               (if (= 1 buffer-count) "" "s"))))
  (e::reset-id-counter))

(defun e:convert-instructions ()
  "Convert instructions between reference and directive within the selected
region or at point.

If a region is selected, convert all instructions within the region. If no
region is selected, convert only the highest priority instruction at point.

Bodyless directives cannot be converted to references. Attempting to do so
will throw a user error."
  (interactive)
  (let* ((instructions (if (use-region-p)
                           (e::instructions-in (region-beginning)
                                               (region-end))
                         (cl:remove-if #'null
                                       (list (e::highest-priority-instruction
                                              (e::instructions-at (point))
                                              t)))))
         (num-instructions (length instructions))
         (converted-directives-to-references 0)
         (converted-references-to-directives 0))
    (if (= num-instructions 0)
        (user-error "No instructions to convert")
      (dolist (instr instructions)
        (cond
         ((e::directivep instr)
          (unless (e::bodyless-instruction-p instr)
            (overlay-put instr 'e:instruction-type 'reference)
            (setq converted-directives-to-references (1+ converted-directives-to-references))))
         ((e::referencep instr)
          (overlay-put instr 'e:instruction-type 'directive)
          (setq converted-references-to-directives (1+ converted-references-to-directives)))
         (t
          (user-error "Unknown instruction type")))
        (e::update-instruction-overlay instr t))
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

(defun e:next-instruction ()
  "Cycle through instructions in the forward direction."
  (interactive)
  (unless (e::cycle-instruction nil 'next)
    (e::print-instruction-not-found 'next nil)))

(defun e:previous-instruction ()
  "Cycle through instructions in the backward direction."
  (interactive)
  (unless (e::cycle-instruction nil 'previous)
    (e::print-instruction-not-found 'previous nil)))

(defun e:next-reference ()
  "Cycle through references in the forward direction."
  (interactive)
  (unless (e::cycle-instruction 'reference 'next)
    (e::print-instruction-not-found 'next 'reference)))

(defun e:previous-reference ()
  "Cycle through references in the backward direction."
  (interactive)
  (unless (e::cycle-instruction 'reference 'previous)
    (e::print-instruction-not-found 'previous 'reference)))

(defun e:next-directive ()
  "Cycle through directives in the forward direction."
  (interactive)
  (unless (e::cycle-instruction 'directive 'next)
    (e::print-instruction-not-found 'next 'directive)))

(defun e:previous-directive ()
  "Cycle through directives in the backward direction."
  (interactive)
  (unless (e::cycle-instruction 'directive 'previous)
    (e::print-instruction-not-found 'previous 'directive)))

(defun e:preview-directive-prompt ()
  "Preview directive prompt at the current point.

This command is useful to see what is actually being sent to the model."
  (interactive)
  (let ((directive (e::topmost-instruction (car (e::instructions-at (point) 'directive))
                                           'directive)))
    (let ((request-string (e::directive-llm-prompt directive)))
      (let ((bufname "*evedel-directive-preview*"))
        (with-output-to-temp-buffer bufname
          (princ (format "<!-- SYSTEM: %s -->"
                         (replace-regexp-in-string "\n"
                                                   "\n# "
                                                   (e::directive-llm-system-message directive))))
          (princ "\n\n")
          (princ request-string)
          (with-current-buffer bufname
            (markdown-mode)
            (read-only-mode 1)
            (visual-line-mode 1)
            (display-line-numbers-mode 1)
            (let ((local-map (make-sparse-keymap)))
              (set-keymap-parent local-map (current-local-map))
              (define-key local-map (kbd "q") 'quit-window)
              (use-local-map local-map))))))))

(defun e:modify-directive-tag-query ()
  "Prompt minibuffer to enter a tag search query for a directive.

The directive in question is either the directive under the curent point.

A tag query is an _infix_ expression, containing symbol atoms and the operator
symbols: `and', `or', `not'.  If no operator is present between two expressions,
then an implicit `and' operator is assumed.

Examples:
  (signature and function and doc)
  (not dog or not cat)
  (cat or dog or (sheep and black))
  ((cat and dog) or (dog and goose))"
  (interactive)
  (if-let ((directive (e::topmost-instruction
                         (e::highest-priority-instruction (e::instructions-at (point)) t)
                         'directive)))
      (e::read-directive-tag-query directive)
    (user-error "No directive at point")))

(defun e:add-tags (&optional reference)
  "Add tags to the reference under the point.

Adds specificly to REFERENCE if it is non-nil."
  (interactive)
  (let* ((instructions (e::instructions-at (point) 'reference))
         (instr (or reference (e::highest-priority-instruction instructions t))))
    (if instr
        (let* ((existing-tags (e::available-tags))
               (input (completing-read-multiple "Add tags (or leave empty): "
                                                existing-tags nil nil))
               (new-tags (mapcar 'intern input)))
            (let ((added (e::add-tags instr new-tags)))
              (message "%d tag%s added" added (if (= added 1) "" "s"))))
      (user-error "No reference at point"))))

(defun e:remove-tags ()
  "Remove tags from the reference under the point."
  (interactive)
  (let* ((instructions (e::instructions-at (point) 'reference))
         (instr (e::highest-priority-instruction instructions t)))
    (if instr
        (let ((tags-list (e::reference-tags instr)))
          (if (null tags-list)
              (user-error "Reference has no tags of its own to remove")
            ;; Prompt the user to remove tags.
            (let* ((input (completing-read-multiple "Remove tags: " tags-list nil t))
                   (tags-to-remove (mapcar 'intern input)))
              (let ((removed (e::remove-tags instr tags-to-remove)))
                (message "%d tag%s removed" removed (if (= removed 1) "" "s"))))))
      (user-error "No reference at point"))))

(cl:defun e::patch-save-file (save-file)
  "Return a patched SAVE-FILE that matches the current version."
  (let ((save-file-version (plist-get save-file :version))
        (new-save-file ()))
    (when (string= save-file-version (e:version))
      (cl:return-from e::patch-save-file save-file))
    (cond
     ;; Save file is version v0.4.7, the first version with backward compatibility.
     (t
      (condition-case err
          (progn
            (e::reset-id-counter)
            (let ((files-alist save-file))
              (cl:loop for (_ . file-plist) in files-alist
                       do (let ((instr-plists (plist-get file-plist :instructions)))
                            (cl:loop for instr-plist in instr-plists
                                     do (let ((ov-props (plist-get instr-plist :properties)))
                                          (with-temp-buffer
                                            (let ((ov (make-overlay 1 1)))
                                              (mapc (lambda (prop)
                                                      (overlay-put ov
                                                                   prop
                                                                   (plist-get ov-props prop)))
                                                    ov-props)
                                              (overlay-put ov 'e:id (e::create-id))
                                              (plist-put instr-plist
                                                         :properties
                                                         (overlay-properties ov))))))))
              (e::reset-id-counter)
              (setf new-save-file (plist-put new-save-file :version (e:version)))
              (setf new-save-file (plist-put new-save-file :ids (list :id-counter e::id-counter
                                                                      :id-usage-map e::id-usage-map
                                                                      :retired-ids e::retired-ids)))
              (setf new-save-file (plist-put new-save-file :files files-alist))))
        (error
         (error "Error patching a versionless save file.

Save file backward compatibility was added in v0.4.7.  If the save file is older than that, then \
unfortunately it is no longer supported.  If the save file is from v0.4.7 or newer, then this is a \
bug that you should report.

The error: %s" err)))))
    (if new-save-file
        (progn
          (message "Patched loaded save file to version %s" (e:version))
          new-save-file)
      save-file)))

(defun e::instruction-id (instruction)
  (overlay-get instruction 'e:id))

(defun e::stashed-buffer-instructions (buffer)
  (e::foreach-instruction (instr buffer)
    collect (list :overlay-start (overlay-start instr)
                  :overlay-end (overlay-end instr)
                  :properties (overlay-properties instr))))

(defun e::stash-buffer (buffer &optional file-contents)
  (let ((instrs (e::stashed-buffer-instructions buffer)))
    (when instrs
      (with-current-buffer buffer
        (let ((original-content (or file-contents (buffer-substring-no-properties (point-min)
                                                                                  (point-max)))))
          (setf (alist-get buffer e::instructions)
                (list :original-content original-content
                      :instructions instrs)
                (car (assoc buffer e::instructions))
                (buffer-file-name buffer))
          (mapc #'delete-overlay (e::instructions-in (point-min) (point-max))))))))

(defun e::read-directive-tag-query (directive)
  "Prompt user to enter a directive tag query text via minibuffer for DIRECTIVE."
  (let ((original-tag-query (overlay-get directive 'e:directive-infix-tag-query-string))
        (timer nil)
        (minibuffer-message))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (when timer
                        (cancel-timer timer)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (when timer
                        (cancel-timer timer))
                      (setq timer
                            (run-with-timer
                             0.5
                             nil
                             (lambda ()
                               (condition-case err
                                   (let ((query (read (concat "("
                                                              (minibuffer-contents)
                                                              ")"))))
                                     (let ((refs (e::filter-references
                                                  (e::tag-query-prefix-from-infix query)))
                                           (total-count 0)
                                           (buffer-count 0))
                                       (cl:loop with bufhash = (make-hash-table)
                                                for ref in refs
                                                do (progn
                                                     (cl:incf total-count)
                                                     (puthash (overlay-buffer ref) t bufhash))
                                                finally (setq buffer-count
                                                              (hash-table-count bufhash)))
                                       (setq minibuffer-message
                                             (format "%d hit%s in %d buffer%s"
                                                     total-count
                                                     (if (= total-count 1) "" "s")
                                                     buffer-count
                                                     (if (= buffer-count 1) "" "s")))))
                                 (error
                                  (setq minibuffer-message
                                        (error-message-string err))))
                               (when minibuffer-message
                                 (set-minibuffer-message minibuffer-message))))))
                    nil t))
      (condition-case err
          (let ((tag-query (read-from-minibuffer "Directive tag query: "
                                                 (substring-no-properties
                                                  (or original-tag-query "")))))
            (let ((parsed-prefix-tag-query
                   (e::tag-query-prefix-from-infix (read (concat "(" tag-query ")")))))
              (overlay-put directive 'e:directive-prefix-tag-query parsed-prefix-tag-query)
              (if (string-empty-p tag-query)
                  (overlay-put directive 'e:directive-infix-tag-query-string nil)
                (overlay-put directive
                             'e:directive-infix-tag-query-string
                             ;; Since Emacs doesn't have negative-lookaheads, we have to make due
                             ;; by first applying the face we want the symbols to be, and then
                             ;; applying the default face on everything we don't want to match.
                             (e::apply-face-to-match "\\b\\(?:(*not\\|or\\|and\\)\\b\\|(\\|)"
                                                     (e::apply-face-to-match
                                                      "\\(:?.+\\)"
                                                      tag-query
                                                      'font-lock-constant-face)
                                                     nil))))
            (e::update-instruction-overlay directive t))
        (error 
         (message (error-message-string err)))))))
            
(defun e::print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('directive "directive")
                       ('reference "reference")
                       (_ "instruction"))))
    (message "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))

(cl:defun e::reference-matches-query-p (reference query)
  "Return t only if REFERENCE matches the tag QUERY."
  (unless reference
    (cl:return-from e::reference-matches-query-p nil))
  (let ((atoms (cl:remove-duplicates (cl:remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) e:empty-tag-query-matches-all)
        t
      (let ((tags (e::reference-tags reference t)))
        (if (and (null tags) e:always-match-untagged-references)
            t
          (let ((atom-bindings (mapcar (lambda (atom)
                                         (pcase atom
                                           ('is:bufferlevel
                                            (e::instruction-bufferlevel-p reference))
                                           ('is:subreference
                                            (e::parent-instruction reference 'reference))
                                           ('is:tagless
                                            (null tags))
                                           ('is:directly-tagless
                                            (null (e::reference-tags reference nil)))
                                           ('is:with-commentary
                                            (not (string-empty-p (e::commentary-text reference))))
                                           (_ (member atom tags))))
                                       atoms)))
            (cl:progv atoms atom-bindings
              (eval query))))))))

(defun e::filter-references (query)
  "Return a list of all references filtered by the tag QUERY.

See `evedel--tag-query-prefix-from-infix' for QUERY format."
  (let ((atoms (cl:remove-duplicates (cl:remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) e:empty-tag-query-matches-all)
        (e::foreach-instruction instr when (e::referencep instr) collect instr)
      (e::foreach-instruction instr
        when (and (e::referencep instr)
                  (e::reference-matches-query-p instr query))
        collect instr))))

(defun e::tag-query-prefix-from-infix (query)
  "Transform the tag QUERY to prefix notation for Lisp.
  
Signals an error when the query is malformed."
  (cl:labels ((operatorp (elm)
                (member elm '(and or not)))
              (unary-op-p (elm)
                (eq elm 'not))
              (binary-op-p (elm)
                (member elm '(and or)))
              (expressionp (elm)
                (or (atomp elm) (listp elm)))
              (atomp (elm)
                (and (not (listp elm)) (not (operatorp elm))))
              (expand-implicit-and-ops (expr)
                (let ((result '()))
                  (dolist (elm expr)
                    (let ((prev (car result)))
                      (cond
                       ((binary-op-p elm)
                        (cond
                         ((binary-op-p prev)
                          (error "Consecutive binary operators: %s, %s" prev elm))
                         ((not (expressionp prev))
                          (error "Binary operator follows operator: %s, %s" prev elm))))
                       ((unary-op-p elm)
                        (cond
                         ((unary-op-p prev)
                          (error "Consecutive unary operator: %s" prev)))))
                      (when (and (not (binary-op-p elm)) prev (not (operatorp prev)))
                        (push 'and result))
                      (push elm result)))
                  (cond
                   ((operatorp (car result))
                    (error "Operator not followed by any expression: %s" (car result)))
                   ((binary-op-p (car (last result)))
                    (error "Binary operator not following any expression: %s" (car (last result)))))
                  (nreverse result)))
              (aux (elm)
                (pcase elm
                  ((pred atomp) elm)
                  ((pred expressionp)
                   (let ((expanded-expr (expand-implicit-and-ops elm))
                         (toplevel-op nil)
                         (operator nil)
                         (multiplicative-exprs ())
                         (operatorless-arg nil)
                         (args ())
                         (negate-next-expr nil))
                     (dolist (elm expanded-expr)
                       (pcase elm
                         ((pred expressionp)
                          (if (null operator)
                              (if (not negate-next-expr)
                                  (setq operatorless-arg (aux elm))
                                (setq operatorless-arg `(not ,(aux elm)))
                                (setq negate-next-expr nil))
                            (cl:symbol-macrolet ((dst (if (eq operator 'and)
                                                          multiplicative-exprs
                                                        args)))
                              (when operatorless-arg
                                (push operatorless-arg dst)
                                (setq operatorless-arg nil))
                              (if (not negate-next-expr)
                                  (push (aux elm) dst)
                                (push `(not ,(aux elm)) dst)
                                (setq negate-next-expr nil)))))
                         ((pred operatorp)
                          (if (unary-op-p elm)
                              (setq negate-next-expr t)
                            (unless (eq toplevel-op 'or)
                              (setq toplevel-op elm))
                            (setq operator elm)
                            (unless (eq operator 'and)
                              (when multiplicative-exprs
                                (push `(and ,@(nreverse multiplicative-exprs)) args)
                                (setq multiplicative-exprs ())))))))
                     (if operatorless-arg
                         operatorless-arg
                       (if args
                           (progn
                             (when multiplicative-exprs
                               (push `(and ,@multiplicative-exprs) args))
                             `(,toplevel-op ,@(nreverse args)))
                         (when multiplicative-exprs
                           `(and ,@(nreverse multiplicative-exprs))))))))))
    (aux query)))
                 
(defun e::available-tags ()
  "Return a list of all the tags in the loaded references."
  (let ((tags-hash (make-hash-table)))
    (e::foreach-instruction (ref)
      do (when (e::referencep ref)
           (cl:loop for tag in (e::reference-tags ref)
                    do (puthash tag t tags-hash))))
    (hash-table-keys tags-hash)))

(defun e::cycle-list-around (element list)
  "Cycle list LIST around ELEMENT.

If ELEMENT is found in LIST, returns a list with ELEMENT as the head and the rest
of the list rotated around it.  Otherwise, returns the LIST."
  (if-let ((element-tail (member element list)))
      (append element-tail 
              (cl:loop for elt in list
                       while (not (eq elt element))
                       collect elt))
    list))

(defun e::cycle-instruction (type direction)
  "Get the next or previous instruction overlay of TYPE.
DIRECTION should be `next' or `previous' from the current point.

If no instruction found in the buffer, checks the next buffers in
the `evedel--instructions' alist.

Returns the found instruction, if any."
  ;; We want the buffers to be a cyclic list, based on the current buffer.
  (let* ((buffers (let ((bufs (mapcar #'car e::instructions)))
                    (if (eq direction 'next)
                        (e::cycle-list-around (current-buffer) bufs)
                      (e::cycle-list-around (current-buffer) (nreverse bufs)))))
         (original-buffer (current-buffer))
         (found-instr))
    (while (and buffers (null found-instr))
      (let* ((buffer (car buffers))
             (instrs (e::foreach-instruction (instr buffer) collect instr)))
        (setq buffers (delq buffer buffers))
        (when type
          (setq instrs (cl:remove-if-not (lambda (instr)
                                           (eq (e::instruction-type instr) type))
                                         instrs)))
        (let ((sorting-pred (pcase direction
                              ('next #'<)
                              ('previous #'>))))
          (when (eq buffer original-buffer)
            (setq instrs (cl:remove-if-not (lambda (instr)
                                             (funcall sorting-pred
                                                      (point)
                                                      (overlay-start instr)))
                                           instrs)))
          (setq instrs (sort instrs (lambda (instr1 instr2)
                                      (funcall sorting-pred
                                               (overlay-start instr1)
                                               (overlay-start instr2)))))
          (when-let ((instruction (car instrs)))
            (let ((buffer (overlay-buffer instruction)))
              (unless (eq buffer original-buffer)
                (switch-to-buffer buffer)))
            (goto-char (overlay-start instruction))
            (setq found-instr instruction)))))
    found-instr))

(defun e::add-tags (reference tags)
  "Add TAGS to REFERENCE.

TAGS should be a list of symbols.
Returns the number of new tags added."
  (let* ((tag-type 'e:reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl:remove-if (lambda (tag) (member tag existing-tags)) tags)))
    (overlay-put reference tag-type (cl:union existing-tags new-tags :test 'eq))
    (let ((added (length new-tags)))
      (when (> added 0)
        (e::update-instruction-overlay reference t))
      added)))

(defun e::remove-tags (reference tags)
  "Remove TAGS from REFERENCE.

TAGS should be a list of symbols.
Returns the number of tags removed."
  (let* ((tag-type 'e:reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl:set-difference existing-tags tags :test 'eq)))
    (overlay-put reference tag-type new-tags)
    (let ((removed (- (length existing-tags) (length new-tags))))
      (when (> removed 0)
        (e::update-instruction-overlay reference t))
      removed)))

(defun e::inherited-tags (reference)
  "Return the list of all tags that REFERENCE inherits from its parents."
  (when-let ((parent (e::parent-instruction reference 'reference)))
    (e::reference-tags parent t)))

(defun e::reference-tags (reference &optional include-parent-tags)
  "Return the list of tags for the given REFERENCE.

If INCLUDE-PARENT-TAG is non-nil, gets te parent's tags as well."
  (if (not include-parent-tags)
      (overlay-get reference 'e:reference-tags)
    (append (overlay-get reference 'e:reference-tags)
            (when-let ((parent (e::parent-instruction reference 'reference)))
              (e::reference-tags parent t)))))

(defun e::delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (e::instructions-at point))
         (target (e::highest-priority-instruction instructions t)))
    (when target
      (e::delete-instruction target))))

(defun e::being-processed-p (instruction)
  "Return non-nil if the directive INSTRUCTION is being processed."
  (eq (overlay-get instruction 'e:directive-status) 'processing))

(defun e::directive-empty-p (directive)
  "Check if DIRECTIVE is empty.

A directive is empty if it does not have a body or hints."
  (let ((subdirectives
         (cl:remove-if-not #'e::directivep
                           (e::wholly-contained-instructions (overlay-buffer directive)
                                                             (overlay-start directive)
                                                             (overlay-end directive)))))
    (not (cl:some (lambda (subdir)
                    (not (string-empty-p (e::directive-text subdir))))
                  subdirectives))))

(defun e::create-instruction (type)
  "Create or scale an instruction of the given TYPE within the selected region.

If a region is selected but partially covers an existing instruction, then the
function will resize it. See either `evedel-create-reference' or
`evedel-create-directive' for details on how the resizing works."
  (if (use-region-p)
      (let ((intersecting-instructions
             (cl:remove-if (lambda (instr)
                             (xor (= (overlay-start instr) (region-beginning))
                                  (= (overlay-end instr) (region-end))))
                           (e::partially-contained-instructions (current-buffer)
                                                                (region-beginning)
                                                                (region-end)))))
        (if-let ((instructions
                  (cl:remove-if-not (lambda (instr)
                                      (eq (e::instruction-type instr) type))
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
                (e::update-instruction-overlay instruction))
              (when instructions
                (deactivate-mark)))
          ;; Else: there are no partially contained instructions of the same type within the
          ;; region...
          (when (or intersecting-instructions
                    (or (cl:some (lambda (instr)
                                   (and (= (overlay-start instr) (region-beginning))
                                        (= (overlay-end instr) (region-end))))
                                 (e::instructions-in (region-beginning) (region-end)))))
            ;; ...but there are intersecting instructions of another type, or another instruction
            ;; existing precisely at the start of another.
            (user-error "Instruction intersects with existing instruction"))
          (let* ((buffer (current-buffer))
                 (instruction (if (eq type 'reference)
                                  (e::create-reference-in buffer
                                                          (region-beginning)
                                                          (region-end))
                                (save-window-excursion
                                  (let ((pos (region-beginning)))
                                    (unless (<= (window-start) pos (window-end))
                                      (set-window-start (selected-window)
                                                        (max (point-min)
                                                             (- (region-beginning)
                                                                (- (window-end) (window-start))))))
                                    (e::create-directive-in buffer
                                                            (region-beginning)
                                                            (region-end)))))))
            (with-current-buffer buffer
              (deactivate-mark)
              (when (eq type 'reference)
                (e:add-tags instruction)))
            instruction)))
    (when (eq type 'directive)
      (prog1 (e::create-directive-in (current-buffer) (point) (point) t)
        (deactivate-mark)))))

(cl:defun e::process-directive-llm-response (response info)
  "Process RESPONSE string.  See `gptel-request' regarding INFO.

Removes any superfluous markup formatting and indents the response according to
the current buffer."
  (let ((directive (plist-get info :context)))
    (unless (overlay-buffer directive)
      ;; Directive is gone...
      (cl:return-from e::process-directive-llm-response))
    (unless (eq (overlay-get directive 'e:directive-status) 'processing)
      ;; The directive has been modified.  Do not continue.
      (cl:return-from e::process-directive-llm-response))
    (cl:flet ((mark-failed (reason)
                (overlay-put directive 'e:directive-status 'failed)
                (overlay-put directive 'e:directive-fail-reason reason)))
      (if (null response)
          (mark-failed (plist-get info :status))
        ;; Parse response that's delimited by Markdown code blocks.
        (if (not (string-match "```+.*\n\\(\\(?:.+\\|\n\\)+\\)\n```+" response))
            (mark-failed response)
          (let ((parsed-response (match-string 1 response)))
            (overlay-put directive 'e:directive-status 'succeeded)
            (with-current-buffer (overlay-buffer directive)
              (let ((beg (overlay-start directive))
                    (end (overlay-end directive)))
                ;; Delete any child directives of the top-level directive.
                (let ((child-directives (cl:remove-if-not #'e::directivep
                                                          (e::child-instructions directive))))
                  (dolist (child-directive child-directives)
                    (e::delete-instruction child-directive)))
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
                      (indent-region beg end)))
                  (overlay-put directive 'evaporate t))))))))
    (e::update-instruction-overlay directive t)))

(defun e::referencep (instruction)
  (eq (e::instruction-type instruction) 'reference))

(defun e::directivep (instruction)
  (eq (e::instruction-type instruction) 'directive))

(defun e::tint (source-color-name tint-color-name &optional intensity)
  "Return hex string color of SOURCE-COLOR-NAME tinted with TINT-COLOR-NAME.

INTENSITY controls the tinting intensity, where 0 means no tinting and 1 means
that the resulting color is the same as the TINT-COLOR-NAME color."
  (let* ((tint (color-name-to-rgb tint-color-name))
         (color (color-name-to-rgb source-color-name))
         (result (cl:mapcar (lambda (color tint)
                              (+ (* (- 1.0 intensity) color)
                                 (* intensity tint)))
                            color
                            tint)))
    (apply 'color-rgb-to-hex `(,@result 2))))

(cl:defun e::highest-priority-instruction (instructions &optional return-highlighted)
  "Return the instruction with the highest priority from the INSTRUCTIONS list.

Priority here refers to the priority property used by overlays.

If RETURN-HIGHLIGHTED is non-nil and `evedel--highlighted-instruction' is
non-nil, the function will return `evedel--highlighted-instruction' if it is
also in the INSTRUCTIONS list."
  (when (and return-highlighted
             e::highlighted-instruction
             (member e::highlighted-instruction instructions))
    (cl:return-from e::highest-priority-instruction e::highlighted-instruction))
  (cl:reduce (lambda (acc instruction)
               (if (or (not acc)
                       (> (or (overlay-get instruction 'priority)
                              e::default-instruction-priority)
                          (or (overlay-get acc 'priority)
                              e::default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun e::instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay.

Instruction type can either be `reference' or `directive'."
  (if-let ((type (overlay-get instruction 'e:instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun e::create-instruction-overlay-in (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (make-local-variable 'e::after-change-functions-hooked)
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'e:instruction t)
      (overlay-put overlay 'e:id (e::create-id))
      (push overlay (alist-get buffer e::instructions))
      (unless (bound-and-true-p e::after-change-functions-hooked)
        (setq-local e::after-change-functions-hooked t)
        (add-hook 'after-change-functions
                  (lambda (beg end _len)
                    (let ((beg (max (point-min) (1- beg)))
                          (end (min (point-max) (1+ end))))
                      (let ((affected-instructions (e::instructions-in beg end)))
                        (dolist (instruction affected-instructions)
                          (e::update-instruction-overlay instruction)))))
                  nil t))
      (e::setup-buffer-hooks buffer)
      overlay)))

(defun e::instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'e:instruction))

(defun e::parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((beg (overlay-start instruction))
          (end (overlay-end instruction)))
      (e::highest-priority-instruction (cl:remove-if-not (lambda (instr)
                                                           (and (not (eq instr instruction))
                                                                (or (null of-type)
                                                                    (eq (e::instruction-type instr)
                                                                        of-type))
                                                                (<= (overlay-start instr) beg
                                                                    end (overlay-end instr))))
                                                         (e::instructions-in beg end))))))

(defun e::bodyless-instruction-p (instr)
  "Returns non-nil if the instruction has a body."
  (= (overlay-start instr) (overlay-end instr)))

(defun e::subinstruction-of-p (sub parent)
  "Return t is instruction SUB is contained entirely within instruction PARENT.

In this case, an instruction is _not_ considered a subinstruction of itself."
  (and (eq (overlay-buffer sub)
           (overlay-buffer parent))
       (<= (overlay-start parent) (overlay-start sub) (overlay-end sub) (overlay-end parent))
       (and (/= (overlay-start parent) (overlay-start sub))
            (/= (overlay-end parent) (overlay-end sub)))))

(cl:defun e::child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (e::bodyless-instruction-p instruction)
    (cl:return-from e::child-instructions nil))
  (let ((children (cl:remove-if (lambda (instr)
                                  (or (eq instr instruction)
                                      (and (= (overlay-start instr) (overlay-start instruction))
                                           (= (overlay-end instr) (overlay-end instruction)))))
                                (e::wholly-contained-instructions (overlay-buffer instruction)
                                                                  (overlay-start instruction)
                                                                  (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl:set-difference children
                                        (e::child-instructions child))))
    children))

(defun e::create-reference-in (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (e::create-instruction-overlay-in buffer start end)))
    (overlay-put ov 'e:instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (e::update-instruction-overlay ov t)
    ov))

(defun e::create-directive-in (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.
BODYLESS controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive.  Having DIRECTIVE-TEXT be
non-nil prevents the opening of a prompt buffer."
  ;; Reset existing directive overlay from non-default state.
  (when-let ((topmost-directive (e::topmost-instruction (e::highest-priority-instruction
                                                         (e::instructions-at start 'directive))
                                                        'directive)))
    (let ((directive-status (overlay-get topmost-directive 'e:directive-status)))
      (unless (null directive-status)
        (overlay-put topmost-directive 'e:directive-status nil)
        (e::update-instruction-overlay topmost-directive t))))
  (let ((ov (e::create-instruction-overlay-in buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'e:instruction-type 'directive)
    (overlay-put ov 'e:directive (or directive-text ""))
    (e::update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (deactivate-mark)
      (e::read-directive ov))
    ov))

(defun e::delete-instruction (instruction)
  "Delete the INSTRUCTION overlay.

Returns the deleted instruction overlay."
  (let ((children (e::child-instructions instruction)))
    (delq instruction (alist-get (overlay-buffer instruction) e::instructions))
    (e::retire-id (e::instruction-id instruction))
    (delete-overlay instruction)
    (dolist (child children)
      (e::update-instruction-overlay child t)))
  instruction)

(defun e::pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

(defun e::fill-label-string (string &optional prefix-string padding buffer)
  "Fill STRING into its label.

If PREFIX-STRING is not nil, whitespace padding is added at the start of
every newline in STRING so that it aligns visually under PREFIX-STRING.

If PADDING is non-nil, then pad the entire string from the left with it.

If BUFFER is provided, STRING will be wrapped to not overflow the fill column
of BUFFER.  Wrapping will attempt to respect word boundaries and only hyphenate
words as a last resort if a word is too long to fit on a line by itself."
  (let* ((paragraph-padding (if prefix-string
                      (make-string (length prefix-string) ? )
                    ""))
         (padding-fill-column (if buffer
                                  (- (with-current-buffer buffer
                                       fill-column)
                                     (if (null padding) 0 (length padding))
                                     (length paragraph-padding))
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
      (insert prefix-string)
      (forward-line)
      (beginning-of-line)
      (while (not (eobp))
        (when padding
          (insert padding))
        (insert paragraph-padding)
        (beginning-of-line)
        (forward-line))
      (string-trim (buffer-string)))))

(defun e::instructions-congruent-p (a b)
  "Returns t only if instruction overlays A and B are congruent."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (= (overlay-start a) (overlay-start b))
       (= (overlay-end a) (overlay-end b))))

(defun e::apply-face-to-match (regex string face)
  "Apply FACE as a text property to the REGEX match in STRING.

If FACE is nil, removes the face property from the REGEX match in STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (if face
          (add-text-properties (match-beginning 0) (match-end 0) `(face ,face))
        (remove-text-properties (match-beginning 0) (match-end 0) '(face nil))))
    (buffer-string)))

(defun e::instruction-bufferlevel-p (instruction)
  "Return t if INSTRUCTION contains the entirety of its buffer."
  (let ((buffer (overlay-buffer instruction)))
    (when buffer
      (with-current-buffer buffer
        (and (= (overlay-start instruction) (point-min))
             (= (overlay-end instruction) (point-max)))))))

(defun e::update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text, and the
background of the instruction overlay.  This function should be called every
time there is a hierarchy or status change in the instruction overlay that we
wish to reflect.

Also updates the child instructions of the INSTRUCTION, if UPDATE-CHILDREN is
non-nil."
  (let ((topmost-directive nil))
    (cl:labels
        ((directive-color (directive)
           (pcase (overlay-get (if topmost-directive
                                   topmost-directive
                                 directive)
                               'e:directive-status)
             ('processing e:directive-processing-color)
             ('succeeded  e:directive-success-color)
             ('failed     e:directive-fail-color)
             (_           e:directive-color)))
         (aux (instruction &optional update-children priority (parent nil))
           (let* ((instruction-type (e::instruction-type instruction))
                  (padding (with-current-buffer (overlay-buffer instruction)
                             (save-excursion
                               (goto-char (overlay-start instruction))
                               (make-string (current-column) ? ))))
                  (is-bufferlevel (e::instruction-bufferlevel-p instruction))
                  (parent-bufferlevel (and parent (e::instruction-bufferlevel-p parent)))
                  ;; This is to prevent the buffer-level instruction from having a background color.
                  (priority (if is-bufferlevel (1- priority) priority))
                  (label "")
                  color)
             (cl:labels
                 ((append-to-label (content &optional prefix)
                    (setq label
                          (concat label
                                  (if (string-empty-p label) "" (concat "\n" padding))
                                  (e::fill-label-string content
                                                        (or prefix "")
                                                        padding
                                                        (overlay-buffer instruction))))))
               (pcase instruction-type
                 ('reference ; REFERENCE
                  (setq color e:reference-color)
                  (if (and parent
                           (and (eq (e::instruction-type parent) 'reference)
                                (not parent-bufferlevel)))
                      (append-to-label "SUBREFERENCE")
                    (if is-bufferlevel
                        (append-to-label "BUFFER REFERENCE")
                      (append-to-label "REFERENCE")))
                  (let* ((direct-tags (e::reference-tags instruction))
                         (inherited-tags (e::inherited-tags instruction))
                         (common-tags (cl:intersection inherited-tags direct-tags))
                         (unique-tags (cl:set-difference direct-tags common-tags)))
                    (cl:labels
                        ((propertized-string-from-tags (tags)
                           (string-join
                            (mapcar (lambda (tag)
                                      (propertize (symbol-name tag)
                                                  'face
                                                  (if (memq tag common-tags)
                                                      'font-lock-warning-face
                                                    'font-lock-constant-face)))
                                    tags)
                            " ")))
                      (when inherited-tags
                        (append-to-label (propertized-string-from-tags
                                          (sort (append inherited-tags) #'string-lessp))
                                         (if common-tags
                                             "INHERITED & COMMON TAGS: "
                                           "INHERITED TAGS: ")))
                      (when unique-tags
                        (append-to-label (propertized-string-from-tags
                                          (sort unique-tags #'string-lessp))
                                         (if inherited-tags
                                             (if common-tags
                                                 "UNIQUE TAGS: "
                                               "DIRECT TAGS: ")
                                           "TAGS: "))))
                    (let ((commentary (string-trim (or (e::commentary-text instruction)
                                                       ""))))
                      (unless (string-empty-p commentary)
                        (append-to-label commentary "COMMENTARY: ")))))
               ('directive ; DIRECTIVE
                (when (and (null topmost-directive) (overlay-get instruction
                                                                 'e:directive-status))
                  (setq topmost-directive instruction))
                (pcase (overlay-get instruction 'e:directive-status)
                  ('processing (append-to-label "PROCESSING"))
                  ('succeeded (append-to-label "SUCCEEDED"))
                  ('failed (append-to-label (overlay-get instruction
                                                         'e:directive-fail-reason)
                                            "FAILED: ")))
                (setq color (directive-color instruction))
                (let (sublabel)
                  (if (and parent
                           (e::directivep parent))
                      (setq sublabel "DIRECTIVE HINT")
                    (setq sublabel (concat sublabel "DIRECTIVE")))
                  (let ((directive (string-trim (or (overlay-get instruction 'e:directive)
                                                    ""))))
                    (if (string-empty-p directive)
                        (setq sublabel (concat "EMPTY " sublabel))
                      (setq sublabel (concat sublabel ": ")))
                    (setq label (concat
                                 label
                                 (unless (string-empty-p label)
                                   (concat "\n" padding))
                                 (e::fill-label-string directive
                                                       sublabel
                                                       padding
                                                       (overlay-buffer instruction))))
                    (unless (e::parent-instruction instruction 'directive)
                      (if-let ((query-string (overlay-get instruction
                                                          'e:directive-infix-tag-query-string)))
                          (append-to-label query-string "TAG QUERY: ")
                        (let (matchinfo)
                          (if e:empty-tag-query-matches-all
                              (setq matchinfo "REFERENCES ALL")
                            (if e:always-match-untagged-references
                                (setq matchinfo "REFERENCES UNTAGGED ONLY")
                              (setq matchinfo "REFERENCES NOTHING")))
                          (setq label (concat label "\n" padding matchinfo)))))))))
             (let* ((default-fg (face-foreground 'default))
                    (default-bg (face-background 'default))
                    (bg-tint-intensity
                     (if (and parent (not parent-bufferlevel))
                         (* e:subinstruction-tint-coefficient e:instruction-bg-tint-intensity)
                       e:instruction-bg-tint-intensity))
                    (label-color (if is-bufferlevel
                                     (e::tint default-fg color e:instruction-label-tint-intensity)
                                   (let ((tint (e::tint default-fg
                                                        color
                                                        e:instruction-label-tint-intensity)))
                                     (dotimes (_  (- priority
                                                     e::default-instruction-priority))
                                       (setq tint (e::tint tint
                                                           color
                                                           e:instruction-label-tint-intensity)))
                                     tint)))
                    ;; We want to make sure that the buffer-level instructions don't superfluously
                    ;; tint the background.
                    (bg-color (if is-bufferlevel
                                  default-bg
                                (let ((tint (e::tint default-bg
                                                     color
                                                     e:instruction-bg-tint-intensity)))
                                  (dotimes (_ (- priority
                                                 e::default-instruction-priority))
                                    (setq tint (e::tint tint color bg-tint-intensity)))
                                  tint))))
               (overlay-put instruction 'e:bg-color bg-color)
               (overlay-put instruction 'e:label-color label-color)
               (overlay-put instruction 'priority priority)
               (when (eq instruction
                         e::highlighted-instruction)
                 (setq bg-color
                       (e::tint default-bg
                                e:highlighted-instruction-color
                                e:highlighted-instruction-tint-intensity)))
               (let ((instruction-is-at-eol (with-current-buffer (overlay-buffer instruction)
                                              (save-excursion
                                                (goto-char (overlay-end instruction))
                                                (= (point) (pos-eol))))))
                 ;; Propertize specific parts of the before-string of the label, to give
                 ;; the illusion that its a "sticker" in the buffer.
                 (cl:labels
                     ((colorize-region (beg end &optional fg bg)
                        (unless (= beg end)
                          (let ((fg (or fg label-color))
                                (bg (or bg bg-color)))
                            (add-face-text-property beg end
                                                    (list :inherit 'default
                                                          :extend t
                                                          :foreground fg
                                                          :background bg)
                                                    t))))
                      (colorize-region-as-parent (beg end)
                        (when-let ((parent (e::parent-instruction instruction)))
                          (colorize-region beg end
                                           (overlay-get parent 'e:label-color)
                                           (overlay-get parent 'e:bg-color)))))
                   (let ((before-string
                          (with-temp-buffer
                            (insert label)
                            (if (e::bodyless-instruction-p instruction)
                                (unless instruction-is-at-eol
                                  (insert "\n"))
                              (insert "\n"))
                            (goto-char (point-min))
                            (end-of-line)
                            (unless (eobp)
                              (forward-char))
                            (colorize-region (point-min) (point))
                            (goto-char (point-min))
                            (forward-line)
                            (while (not (eobp))
                              (beginning-of-line)
                              (let ((mark (point)))
                                (forward-char (length padding))
                                (colorize-region-as-parent mark (point)))
                              (let ((went-to-next-line))
                                (let ((mark (point)))
                                  (end-of-line)
                                  (unless (eobp)
                                    (setq went-to-next-line t)
                                    (forward-char))
                                  (colorize-region mark (point)))
                                (unless went-to-next-line
                                  (forward-line))))
                            (unless (e::bodyless-instruction-p instruction)
                              (let ((mark (point)))
                                (insert padding)
                                (colorize-region-as-parent mark (point))))
                            (buffer-string))))
                     (overlay-put instruction 'before-string before-string))))
               (overlay-put instruction 'face `(:extend t :background ,bg-color)))
           (when update-children
             (dolist (child (e::child-instructions instruction))
               (aux child update-children (1+ priority) instruction)))))))
      (let ((instructions-conflicting (cl:some (lambda (instr)
                                                 (and (not (eq instr instruction))
                                                      (e::instructions-congruent-p instruction
                                                                                   instr)))
                                               (e::instructions-at (overlay-start instruction)))))
        (if instructions-conflicting
            ;; This instruction is causing conflicts, and therefore must be deleted.
            (e::delete-instruction instruction)
          (let ((parent (e::parent-instruction instruction)))
            (let ((priority (if parent
                                (1+ (overlay-get parent 'priority))
                              e::default-instruction-priority)))
              (aux instruction update-children priority parent))))))))

(defun e::restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
    new-ov))

(defun e::file-outdated-p (file)
  "Determine whether or not FILE needs patching.

A file being outdated refers to the file in the instructions alist not being
up-to-date, not the actual file on the disk being outdated."
  (when (file-exists-p file)
    (when-let ((file-plist (cdr (assoc file e::instructions))))
      (let ((e::inhibit-file-restoration t))
        (let ((original-content (plist-get file-plist :original-content))
              (buffer (find-file-noselect file)))
          (with-current-buffer buffer
            (not (string= original-content
                          (buffer-substring-no-properties (point-min) (point-max))))))))))

(defun e::setup-buffer-hooks (buffer)
  (with-current-buffer buffer
    (unless (bound-and-true-p e::buffer-hooks-setup)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (e::buffer-has-instructions-p (current-buffer))
                    (when-let ((file (buffer-file-name buffer)))
                      (if (file-exists-p file)
                        (let ((file-contents
                               (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                          (e::stash-buffer buffer file-contents))
                        (setq e::instructions (assq-delete-all buffer e::instructions))))))
                  nil t))
      (add-hook 'before-revert-hook
                (lambda ()
                  (when (e::buffer-has-instructions-p buffer)
                    (e::stash-buffer buffer)
                    (setq-local e::buffer-instructions-reverted t)))
                nil t)
      (add-hook 'after-revert-hook
                (lambda ()
                  (when (bound-and-true-p e::buffer-instructions-reverted)
                    (e::restore-file-instructions (buffer-file-name buffer) t)
                    (setq-local e::buffer-instructions-reverted nil)))
                nil t)
      (setq-local e::buffer-hooks-setup t)))

(defun e::buffer-has-instructions-p (buffer)
  (assoc buffer e::instructions))

(cl:defun e::restore-file-instructions (file &optional message)
  "Restore FILE and its INSTRUCTIONS.

Returns two values: the amount of instructions restored and the amount of
instructions lost to the patching process, if any.

If MESSAGE is non-nil, message the intent of patching outdated files."
  (let ((e::inhibit-file-restoration t))
    (unless (and (file-exists-p file)
                 (assoc file e::instructions))
      (cl:return-from e::restore-file-instructions (cl:values 0 0)))
    (cl:destructuring-bind (&key original-content instructions) (alist-get file
                                                                           e::instructions
                                                                           nil
                                                                           nil
                                                                           #'equal)
      (when (or (null original-content)
                (null instructions))
        (error "Malformed file given for restoration"))
      (let ((buffer (find-file-noselect file))
            (restored 0)
            (kia 0))
        (with-current-buffer buffer
          (e::setup-buffer-hooks buffer)
          (cl:labels ((restore-overlays (dstbuf instr-maybe-plists)
                        (let ((ovs ()))
                          (dolist (instr instr-maybe-plists)
                            (if (plist-get instr :overlay-start)
                              (cl:destructuring-bind (&key overlay-start overlay-end properties)
                                  instr
                                (push (e::restore-overlay dstbuf
                                                          overlay-start
                                                          overlay-end
                                                          properties)
                                      ovs))
                              (push (e::restore-overlay dstbuf
                                                        (overlay-start instr)
                                                        (overlay-end instr)
                                                        (overlay-properties instr))
                                    ovs)))
                          ovs)))
            (if (and e:patch-outdated-instructions
                     (e::file-outdated-p file))
                (if (not (executable-find "diff"))
                    (progn
                      (warn "Patching outdated instructions requires 'diff' to be installed.")
                      (setq e:patch-outdated-instructions nil)
                      (restore-overlays buffer instructions))
                  (when message
                    (message "Patching outdated instructions in buffer '%s'..."
                             (buffer-name buffer)))
                  (with-temp-buffer
                    (let ((new-buffer (current-buffer)))
                      (insert-buffer-substring-no-properties buffer)
                      (with-temp-buffer
                        (insert original-content)
                        (restore-overlays (current-buffer) instructions)
                        (e::wordwise-diff-patch-buffers (current-buffer) new-buffer)
                        (restore-overlays buffer (e::instructions-in (point-min) (point-max)))))))
              (restore-overlays buffer instructions)))
          (let ((restored-instrs (e::instructions-in (point-min) (point-max))))
            (setq restored (length restored-instrs)
                  kia (- (length instructions) restored))
            (setf (alist-get file e::instructions nil nil #'equal) restored-instrs)))
        (setf (car (assoc file e::instructions)) buffer)
        (cl:values restored kia)))))

(defun e::wholly-contained-instructions (buffer start end)
  "Return Evedel overlays in BUFFER that are entirely within START and END."
  (with-current-buffer buffer
    (cl:remove-if-not (lambda (ov)
                        (and (overlay-get ov 'e:instruction)
                             (>= (overlay-start ov) start)
                             (<= (overlay-end ov) end)))
                      (overlays-in start end))))

(defun e::instructions-at (point &optional type)
  "Return a list of instructions at current POINT.

Optionally return only instructions of specific TYPE.
Also returns bodyless overlays located right before the point."
  (cl:remove-if-not (lambda (ov)
                      (and (overlay-get ov 'e:instruction)
                           (or (and type
                                    (eq (overlay-get ov 'e:instruction-type)
                                        type))
                               (null type))))
                    (overlays-in point
                                 (min (point-max) (1+ point)))))

(defun e::instructions-in (start end &optional type)
  "Return a list of instructions in region delimited by START and END.

Optionally return only instructions of specific TYPE."
  (cl:remove-if-not (lambda (ov)
                      (and (overlay-get ov 'e:instruction)
                           (or (and type
                                    (eq (overlay-get ov 'e:instruction-type)
                                        type))
                               (null type))))
                    (overlays-in start end)))

(defun e::partially-contained-instructions (buffer start end)
  "Return instructions in BUFFER that overlap with START and END.

Does not return instructions that contain the region in its entirety."
  (with-current-buffer buffer
    (cl:remove-if-not (lambda (ov)
                        (and (overlay-get ov 'e:instruction)
                             (or (<= (overlay-start ov) start)
                                 (>= (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun e::instructions ()
  "Return a list of all currently loaded instructions."
  (e::foreach-instruction inst collect inst))

(cl:defun e::topmost-instruction (instruction &optional of-type pred)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE.
If OF-TYPE is nil, the instruction returned is the top-level one.

If PRED is non-nil, then the best instruction must also satisfy it.
The PRED must be a function which accepts an instruction."
  (unless instruction
    (cl:return-from e::topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction instruction))
      (cl:labels ((parent-instr (instr)
                    (if-let ((parent (e::parent-instruction instr)))
                        (progn
                          (when (and (or (null of-type) (eq of-type (e::instruction-type parent)))
                                     (or (null pred) (funcall pred parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (setq best-instruction (parent-instr instruction)))
      (if (and (or (null of-type) (eq of-type (e::instruction-type best-instruction)))
               (or (null pred) (funcall pred best-instruction)))
          best-instruction
        nil))))

(defun e::directive-text (directive)
  "Return the directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (overlay-get directive 'e:directive) ""))

(defun e::commentary-text (reference)
  "Return the commentary text of the REFERENCE overlay.

Returns an empty string if there is no commentary."
  (or (overlay-get reference 'e:commentary) ""))

(defun e::read-directive (directive)
  "Prompt user to enter a directive text via minibuffer for DIRECTIVE."
  (let ((original-directive-text (e::directive-text directive)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((directive-text (minibuffer-contents)))
                        (overlay-put directive 'e:directive directive-text)
                        (e::update-instruction-overlay directive)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put directive 'e:directive (minibuffer-contents))
                      (e::update-instruction-overlay directive))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Directive: " original-directive-text)
        (quit
         (if (string-empty-p original-directive-text)
             (e::delete-instruction directive)
           (overlay-put directive 'e:directive original-directive-text)
           (e::update-instruction-overlay directive nil))
         (signal 'quit nil))))))

(defun e::read-commentary (reference)
  "Prompt user to enter a commentary text via minibuffer for REFERENCE."
  (let ((original-commentary-text (e::commentary-text reference)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((commentary-text (minibuffer-contents)))
                        (overlay-put reference 'e:commentary commentary-text)
                        (e::update-instruction-overlay reference)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put reference 'e:commentary (minibuffer-contents))
                      (e::update-instruction-overlay reference))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Commentary: " original-commentary-text)
        (quit
         (overlay-put reference 'e:commentary original-commentary-text)
         (e::update-instruction-overlay reference nil))
        (signal 'quit nil)))))

(defun e::descriptive-llm-mode-role (mode)
  "Derive the descriptive major mode role name from the major MODE.

Defaults to \"a helpful assistant\" if no appropriate role has been found in
the `evedel-descriptive-mode-roles' variable.

The role will default to \"a careful programmer\" if the major mode is not
listed in `evedel-descriptive-mode-roles' but is derivative from `prog-mode'."
  (if-let ((role (alist-get mode e:descriptive-mode-roles)))
      role
    (if (provided-mode-derived-p mode 'prog-mode)
        "a careful programmer"
      "a helpful assistant")))

(defun e::directive-llm-system-message (directive)
  "Craft the system message for the LLM model associated with the DIRECTIVE.

Returns the message as a string."
  (with-current-buffer (overlay-buffer directive)
    (concat "You are " (e::descriptive-llm-mode-role major-mode) ". Follow user directive.")))

(defun e::delimiting-markdown-backticks (string)
  "Return a string containing the appropriate code block backticks for STRING."
  (let ((backticks "```"))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks "`")))
    backticks))

(defun e::overlay-region-info (overlay)
  "Return region span information of OVERLAY in its buffer.

Returns three values, first being the region line & column span string in the
buffer, and the second being the content of the span itself."
  (let ((beg (overlay-start overlay))
        (end (overlay-end overlay)))
    (cl:labels ((pos-bol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (bolp)))
                (pos-eol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (eolp)))
                (pos-lineno (pos)
                  (line-number-at-pos pos))
                (pos-colno (pos)
                  (save-excursion
                    (goto-char pos)
                    (current-column))))
      (with-current-buffer (overlay-buffer overlay)
        (without-restriction
          (unless (= beg end)
            (when (pos-eol-p beg)
              (cl:incf beg))
            (when (pos-bol-p end)
              (cl:decf end)))
          (if (= beg end (point-min))
              (cl:values "beginning of the buffer" "")
            (let ((beg-lineno (pos-lineno beg))
                  (end-lineno (pos-lineno end))
                  (beg-colno (pos-colno beg))
                  (end-colno (pos-colno end)))
              (cl:values (format "line%s %s"
                                 (if (/= beg-lineno end-lineno) "s" "")
                                 (if (/= beg-lineno end-lineno)
                                     (format "%d%s-%d%s"
                                             beg-lineno
                                             (if (pos-bol-p beg)
                                                 ""
                                               (format ":%d" beg-colno))
                                             end-lineno
                                             (if (pos-eol-p end)
                                                 ""
                                               (format ":%d" end-colno)))
                                   (format "%s%s"
                                           beg-lineno
                                           (if (and (pos-bol-p beg) (pos-eol-p end))
                                               ""
                                             (if (= beg-colno end-colno)
                                                 (format ", column %d" beg-colno)
                                               (format ", columns %d-%s"
                                                       beg-colno
                                                       (if (pos-eol-p end)
                                                           "eol"
                                                         (format "%d" end-colno))))))))
                         (buffer-substring-no-properties beg end)))))))))

(defun e::toplevel-references ()
  "Fetch all toplevel reference instructions.

A toplevel reference instruction is one that has no parents."
  (seq-filter (lambda (instr)
                (and (null (e::parent-instruction instr))
                     (e::referencep instr)))
              (e::instructions)))

(defun e::multiline-string-p (str)
  "Check if STR contains multiple lines."
  (string-match-p "\n" str))

(defun e::markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))

(defun e::directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE.

Returns the prompt as a string."
  (when (e::directive-empty-p directive)
    (error "Directive %s is empty" directive))
  (let* ((is-programmer (derived-mode-p 'prog-mode))
         (query (overlay-get directive 'e:directive-prefix-tag-query))
         ;; This hash map is for saving up references whose commentary has been used up.  The reason
         ;; we want to do this is for the case when we already queried the parent reference
         ;; commentary, we wouldn't want for that commentary to appear again.
         (used-commentary-refs (make-hash-table))
         (pred (lambda (instr)
                 (e::reference-matches-query-p instr query)))
         (toplevel-references (e::foreach-instruction instr
                                when (and (e::referencep instr)
                                          (eq (e::topmost-instruction instr 'reference pred)
                                              instr)
                                          ;; We do not wish to collect references that are
                                          ;; contained within directives, it's redundant.
                                          (not (e::subinstruction-of-p instr directive)))
                                collect instr))
         ;; The references in the reference alist should be sorted by their order of appearance
         ;; in the buffer.
         (reference-alist (cl:loop for reference in toplevel-references with alist = ()
                                   do (push reference (alist-get (overlay-buffer reference) alist))
                                   finally (progn
                                             (cl:loop for (_ . references) in alist
                                                      do (sort references
                                                               (lambda (x y)
                                                                 (< (overlay-start x)
                                                                    (overlay-start y)))))
                                             (cl:return alist))))
         (reference-count (length toplevel-references))
         (toplevel-directive-is-empty (string-empty-p (e::directive-text directive)))
         (directive-toplevel-reference (e::topmost-instruction directive 'reference pred))
         (directive-buffer (overlay-buffer directive))
         ;; Should the directive buffer have a valid file path, we should use a relative path for
         ;; the other references, assuming that they too have a valid file path.
         (directive-filename (buffer-file-name directive-buffer))
         (reference-commentators ()))
    (cl:destructuring-bind (directive-region-info-string directive-region-string)
        (e::overlay-region-info directive)
      ;; This marking function is used to mark the prompt text so that it may later be formatted by
      ;; sections, should the need to do so will arise.
      (cl:labels ((unreferenced-ancestral-commentators (instr)
                    (cl:remove-if (lambda (ref)
                                    (gethash ref used-commentary-refs))
                                  (e::ancestral-commentators instr)))
                  (aggregated-commentary (commentators)
                    (cl:loop for ref in commentators
                       count ref into refnum
                       concat (cl:destructuring-bind (ref-info-string _)
                                  (e::overlay-region-info ref)
                                (format "\n\nCommentary #%d for %s:\n\n%s"
                                        refnum
                                        ref-info-string
                                        (e::markdown-enquote (e::commentary-text ref))))
                       into commentary
                       finally (cl:return commentary)))
                  (response-directive-guide-text ()
                    (if (e::bodyless-instruction-p directive)
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
                           (cl:remove-if-not (lambda (inst)
                                               (and (eq (e::instruction-type inst) 'directive)
                                                    (not (eq inst directive))))
                                             (e::wholly-contained-instructions
                                              (overlay-buffer directive)
                                              (overlay-start directive)
                                              (overlay-end directive)))))
                      (concat
                       (format "%s" directive-region-info-string)
                       (if (string-empty-p directive-region-string)
                           "."
                         (let ((markdown-delimiter
                                (e::delimiting-markdown-backticks directive-region-string)))
                           (concat
                            (format ", which correspond%s to:"
                                    (if (e::multiline-string-p directive-region-string) "" "s"))
                            "\n\n"
                            (format "%s\n%s\n%s"
                                    markdown-delimiter
                                    directive-region-string
                                    markdown-delimiter))))
                       "\n\n"
                       (if (not toplevel-directive-is-empty)
                           (format "The directive is:\n\n%s"
                                   (e::markdown-enquote (overlay-get directive 'e:directive)))
                         "The directive is composed entirely of hints, so you should treat them as \
subdirectives.")
                       (cl:loop for hint in directive-hints
                                when (not (string-empty-p (e::directive-text hint)))
                                concat (concat
                                        "\n\n"
                                        (cl:destructuring-bind (hint-region-info hint-region)
                                            (e::overlay-region-info hint)
                                          (concat
                                           (format "For %s"
                                                   hint-region-info)
                                           (let ((hint-text (e::markdown-enquote
                                                             (overlay-get hint 'e:directive))))
                                             (if (e::bodyless-instruction-p hint)
                                                 (format ", you have a hint:\n\n%s"
                                                         hint-text)
                                               (let ((markdown-delimiter
                                                      (e::delimiting-markdown-backticks
                                                       hint-region)))
                                                 (concat
                                                  (format ", which correspond%s to:\n\n%s"
                                                          (if (e::multiline-string-p hint-region)
                                                              "" "s")
                                                          (format "%s\n%s\n%s"
                                                                  markdown-delimiter
                                                                  hint-region
                                                                  markdown-delimiter))
                                                  (format "\n\nYou have the hint:\n\n%s"
                                                          hint-text)))))))))))))
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
          (cl:loop for (buffer . references) in reference-alist
                   do (insert
                       (concat
                        "\n\n"
                        (format "### %s" (capitalize-first-letter
                                          (instruction-path-namestring buffer)))))
                   (dolist (ref references)
                     (setq reference-commentators
                           (append reference-commentators
                                   (unreferenced-ancestral-commentators ref)))
                     (cl:destructuring-bind (ref-info-string ref-string)
                         (e::overlay-region-info ref)
                       (let ((markdown-delimiter
                              (e::delimiting-markdown-backticks ref-string)))
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
                              (response-directive-guide-text)))
                           (let ((commentary (e::commentary-text ref)))
                             (unless (string-empty-p commentary)
                               (puthash ref t used-commentary-refs)
                               (format "\n\nReference commentary:\n\n%s"
                                       (e::markdown-enquote commentary))))))))))
          (let ((directive-commentators (unreferenced-ancestral-commentators directive)))
            (when (or directive-commentators reference-commentators)
              (insert (concat "\n\n"
                              "## Additional Commentary"
                              "\n\n"
                              "Listed below is commentary from references which were not used in \
the directive, but are nonetheless either containing the directive or belong to relevant parent \
references, and thus could prove important:"))
              (insert (aggregated-commentary (append directive-commentators
                                                     reference-commentators)))))
          (unless directive-toplevel-reference
            (insert
             (concat "\n\n"
                     "## Directive"
                     "\n\n"
                     (format "For %s, %s"
                             (instruction-path-namestring directive-buffer)
                             (expanded-directive-text directive))
                     "\n\n"
                     (response-directive-guide-text))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun e::ancestral-commentators (instruction)
  "Return list of references which contain INSTRUCTION that have commentary.

The list is sorted with the topmost references first."
  (with-current-buffer (overlay-buffer instruction)
    (let* ((start (overlay-start instruction))
           (end (overlay-end instruction))
           (instructions (e::instructions-in start end 'reference))
           (filtered (cl:remove-if-not (lambda (instr)
                                         (and (not (string-empty-p (e::commentary-text instr)))
                                              (e::subinstruction-of-p instruction instr)))
                                       instructions))
           (sorted (sort filtered (lambda (a b) (e::subinstruction-of-p b a)))))
      sorted)))

(defun e::wordwise-diff-patch-buffers (old new)
  "Wordwise patch buffer OLD to be equivalent to buffer NEW via `ediff-buffers'.

This is mostly a brittle hack meant to make Ediff be used noninteractively."
  (cl:labels ((apply-all-diffs ()
                (ediff-next-difference)
                (while (ediff-valid-difference-p)
                  (ediff-copy-B-to-A nil)
                  (ediff-next-difference))))
    (let ((orig-window-config (current-window-configuration)))
      (unwind-protect
          (progn
            (let ((old-region (with-current-buffer old
                                (cons (point-min) (point-max))))
                  (new-region (with-current-buffer new
                                (cons (point-min) (point-max)))))
              ;; The following two bindings prevent Ediff from creating a new window.
              (let ((ediff-window-setup-function 'ediff-setup-windows-plain)
                    (ediff-split-window-function 'split-window-horizontally))
                (let ((inhibit-message t))
                  ;; Prevent Ediff from polluting the messages buffer.
                  (cl:letf (((symbol-function 'message) (lambda (&rest _)) t))
                    ;; Run wordwise diff first to replace with higher granularity.
                    (ediff-regions-internal old
                                            (car old-region)
                                            (cdr old-region)
                                            new
                                            (car new-region)
                                            (cdr new-region)
                                            nil
                                            (gensym "evedel-ediff-")
                                            t
                                            nil)
                    (cl:letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                      (let ((ediff-control-buffer-name "*Ediff Control Panel*"))
                        ;; This is very brittle.
                        (with-current-buffer (get-buffer ediff-control-buffer-name)
                          (apply-all-diffs)
                          (ediff-quit t))
                        ;; Run regular diff to also replace empty newlines.
                        (ediff-buffers old new)
                        (with-current-buffer (get-buffer ediff-control-buffer-name)
                          (apply-all-diffs)
                          (ediff-quit t)))))))))
        (set-window-configuration orig-window-config)))))

(defvar e::id-counter 0)
(defvar e::id-usage-map (make-hash-table))
(defvar e::retired-ids ())

(defun e::create-id ()
  (let ((id
         (if e::retired-ids
             (prog1
                 (car e::retired-ids)
               (setq e::retired-ids (cdr e::retired-ids)))
           (cl:incf e::id-counter))))
    (puthash id t e::id-usage-map )
    id))

(defun e::retire-id (id)
  (when (gethash id e::id-usage-map)
    (remhash id e::id-usage-map)
    (push id e::retired-ids)))

(defun e::reset-id-counter ()
  "Reset all custom variables to their default values."
  (setq e::id-counter 0)
  (setq e::id-usage-map (make-hash-table))
  (setq e::retired-ids ()))

(add-hook 'find-file-hook
          (lambda ()
            (unless e::inhibit-file-restoration
              (e::restore-file-instructions (buffer-file-name (current-buffer))))))

(provide 'evedel)

;; Local Variables:
;; read-symbol-shorthands: (("e::" . "evedel--")
;;                          ("e:"  . "evedel-")
;;                          ("cl:" . "cl-")) 
;; End:

;;; evedel.el ends here.
