;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'evedel-utilities)

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

(defcustom e-highlighted-instruction-color "cyan"
  "Color for currently highlighted instructions."
  :type 'string
  :group 'evedel)

(defcustom e-instruction-bg-tint-intensity 0.1
  "Default intensity for background tinting of instructions."
  :type 'float
  :group 'evedel)

(defcustom e-instruction-label-tint-intensity 0.2
  "Default intensity for label tinting of instructions."
  :type 'float
  :group 'evedel)

(defcustom e-highlighted-instruction-tint-intensity 0.2
  "Default intensity for tinting of highlighted instructions."
  :type 'float
  :group 'evedel)

(defcustom e-subinstruction-tint-coefficient 0.4
  "Coeffecient multiplied by by tint intensities.

Only applicable to the subinstructions.  Makes it possible to have more a
more finely-tuned control over how tinting looks.

Does not affect the label colors, just the backgrounds."
  :type 'float
  :group 'evedel)

(defcustom e-empty-tag-query-matches-all t
  "Determines behavior of directives without a tag search query.

If set to t, directives without a specific tag search query will use all
available references.  Alternatively, if this is set to nil, directives without
a search query will not use any references."
  :type 'boolean
  :group 'evedel)

(defcustom e-always-match-untagged-references t
  "Controls inclusion of untagged references in directive prompts.

When set to t, untagged references are always incorporated into directive
references, ensuring comprehensive coverage.  Conversely, when set to nil,
untagged references are ignored, unless `evedel-empty-tag-query-matches-all'
is set to t.

A reference is considered untagged when it has no direct tags.  References can
inherit tags from ancestor references and still be considered untagged."
  :type 'boolean
  :group 'evedel)

(defvar e--instructions ()
  "Association list mapping buffers or files to lists of instruction overlays.")
(defvar e--default-instruction-priority -99)
(defvar e--highlighted-instruction nil)
(defvar e--id-counter 0)
(defvar e--id-usage-map (make-hash-table))
(defvar e--retired-ids ())

(defmacro e--foreach-instruction (binding &rest body)
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
  (cl-with-gensyms (cons bof specific-buffer)
    (let ((instr (if (listp binding) (car binding) binding)))
      `(cl-labels ((trashp (instr)
                     (and (null (overlay-buffer instr))
                          (not (overlay-get instr 'e-marked-for-deletion))))
                   (clean-alist-entry (cons)
                     (mapc (lambda (instr) (e--delete-instruction instr (car cons)))
                           (cl-remove-if-not #'trashp (cdr cons)))
                     (let ((instrs (cl-remove-if #'trashp (cdr cons))))
                       (setf (cdr cons) instrs))))
         (let ((,specific-buffer ,(if (listp binding) (cadr binding) nil)))
           (if (not ,specific-buffer)
               (cl-loop for ,cons in e--instructions
                        do (let ((,bof (car ,cons)))
                             (if (stringp ,bof) ; bof is a file, restore it.
                                 (e--restore-file-instructions ,bof)
                               (clean-alist-entry ,cons)))) ; bof is a buffer, clean it.
             (when-let ((cons (assoc ,specific-buffer e--instructions)))
               (clean-alist-entry cons)))
           ;; Remove empty cons cells from the alist.
           (setq e--instructions (cl-remove-if (lambda (cons)
                                                 (null (cdr cons)))
                                               e--instructions))
           ;; The instructions alist should now be cleaned of deleted instructions.
           (cl-loop for ,instr
                    in (if ,specific-buffer
                           (alist-get ,specific-buffer e--instructions)
                         (flatten-tree
                          (cl-remove nil
                                     (mapcar (lambda (plist-or-instrs)
                                               (if (plist-get plist-or-instrs :instructions)
                                                   nil ; Plist
                                                 plist-or-instrs))
                                             (mapcar #'cdr e--instructions)))))
                    ,@body))))))

;;;###autoload
(defun e-instruction-count ()
  "Return the number of instructions currently loaded instructions.

If called interactively, it messages the number of instructions and buffers."
  (interactive)
  (let ((count 0)
        (buffer-hash (make-hash-table :test 'eq)))
    (e--foreach-instruction instr count instr into instr-count
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
(defun e-create-reference ()
  "Create a reference instruction within the selected region.

If a region is selected but partially covers an existing reference, then the
command will resize the reference in the following manner:

  - If the mark is located INSIDE the reference (i.e., the point is located
    OUTSIDE the reference) then the reference will be expanded to the point.
  - If the mark is located OUTSIDE the reference (i.e., the point is located
    INSIDE the reference) then the reference will be shrunk to the point."
  (interactive)
  (e--create-instruction 'reference))

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
  (e--create-instruction 'directive))

(defun e-link-instructions (from-list to-list)
  "Link instructions with ids in FROM-LIST to those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction ids."
  (interactive
   (let ((completion-table (mapcar #'number-to-string (hash-table-keys e--id-usage-map))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to link to: "
                                             completion-table nil t)))))
  (cl-labels
      ((update-links (instr-id num-key update-id)
         (let* ((instr (e--instruction-with-id instr-id))
                (links (overlay-get instr 'e-links))
                (ids (plist-get links num-key)))
           (unless (member update-id ids)
             (setq ids (cons update-id ids))
             (overlay-put instr 'e-links (plist-put links num-key ids))
             t))))
    (let ((new-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let ((from-instr (e--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when (/= from-id to-id)
              (when-let ((to-instr (e--instruction-with-id to-id)))
                (when (and (update-links from-id :to to-id)
                           (update-links to-id :from from-id))
                  (puthash from-instr t involved-instrs)
                  (puthash to-instr t involved-instrs)
                  (cl-incf new-link-count)))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (e--update-instruction-overlay instr))
      (when (called-interactively-p 'interactive)
        (message "Created %d instruction link%s"
                 new-link-count
                 (if (= new-link-count 1) "" "s"))))))

(defun e-unlink-instructions (from-list to-list)
  "Unlink instructions with ids in FROM-LIST from those in TO-LIST.

When invoked interactively, prompts user for two lists of instruction ids."
  (interactive
   (let ((completion-table (mapcar #'number-to-string (hash-table-keys e--id-usage-map))))
     (list (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink: "
                                             completion-table nil t))
           (mapcar #'string-to-number
                   (completing-read-multiple "Select instruction ids to unlink from: "
                                             completion-table nil t)))))
  (cl-labels
      ((remove-links (instr-id num-key remove-id)
         (let* ((instr (e--instruction-with-id instr-id))
                (links (overlay-get instr 'e-links))
                (ids (plist-get links num-key)))
           (when (member remove-id ids)
             (setq ids (remove remove-id ids))
             (overlay-put instr 'e-links (plist-put links num-key ids))
             t))))
    (let ((removed-link-count 0)
          (involved-instrs (make-hash-table)))
      (dolist (from-id from-list)
        (when-let ((from-instr (e--instruction-with-id from-id)))
          (dolist (to-id to-list)
            (when-let ((to-instr (e--instruction-with-id to-id)))
              (when (and (remove-links from-id :to to-id)
                         (remove-links to-id :from from-id))
                (puthash from-instr t involved-instrs)
                (puthash to-instr t involved-instrs)
                (cl-incf removed-link-count))))))
      (cl-loop for instr being the hash-keys of involved-instrs
               do (when (buffer-live-p (overlay-buffer instr))
                    (e--update-instruction-overlay instr)))
      (when (called-interactively-p 'interactive)
        (message "Removed %d instruction link%s"
                 removed-link-count
                 (if (= removed-link-count 1) "" "s"))))))

(defun e-cycle-instructions-at-point (point)
  "Cycle through instructions at POINT, highlighting them.

This command allows for cycling through overlapping instructions at a
point in the buffer and allows one to have better accuracy when instructions
overlap to the point where no other reasonable option is available."
  (interactive "d")
  (let ((instructions-at-point (e--instructions-at point))
        (original-highlighted-instruction e--highlighted-instruction))
    (cond
     ((null instructions-at-point)
      (setq e--highlighted-instruction nil)
      (when (called-interactively-p 'any)
        (message "No instructions at point")))
     ((or (null e--highlighted-instruction)
          (not (memq e--highlighted-instruction instructions-at-point)))
      (setq e--highlighted-instruction nil)
      (setq e--highlighted-instruction (e--highest-priority-instruction instructions-at-point)))
     (t
      (if-let ((parent (e--parent-instruction e--highlighted-instruction)))
          (setq e--highlighted-instruction parent)
        (setq e--highlighted-instruction nil))))
    (when e--highlighted-instruction
      (e--update-instruction-overlay e--highlighted-instruction))
    (when original-highlighted-instruction
      (e--update-instruction-overlay original-highlighted-instruction))
    e--highlighted-instruction))

(defun e-modify-directive ()
  "Modify the directive under the point."
  (interactive)
  (when-let ((directive (e--highest-priority-instruction (e--instructions-at (point) 'directive)
                                                         t)))
    (when (eq (overlay-get directive 'e-directive-status) 'processing)
      (overlay-put directive 'e-directive-status nil))
    (let ((topmost-directive (e--topmost-instruction directive 'directive)))
      (when (eq (overlay-get topmost-directive 'e-directive-status) 'failed)
        (setf (overlay-get topmost-directive 'e-directive-status) nil)
        (e--update-instruction-overlay topmost-directive t)))
    (e--read-directive directive)))

(defun e-modify-reference-commentary ()
  "Modify the reference commentary under the point."
  (interactive)
  (when-let ((reference (e--highest-priority-instruction (e--instructions-at (point) 'reference)
                                                         t)))
    (e--read-commentary reference)))

(cl-defgeneric e--process-directive (directive callback))

(cl-defgeneric e--llm-client-name ())

(defun e-process-directives ()
  "Send directives to model .

If a region is selected, send all directives within the region.
If a region is not selected and there is a directive under the point, send it."
  (interactive)
  (let ((count 0))
    (cl-labels ((execute (directive)
                  (unless (e--being-processed-p directive)
                    (if (e--directive-empty-p directive)
                        ;; There is no point in sending empty directives.
                        (e--process-directive-llm-response "The directive is empty!"
                                                           directive
                                                           'empty-directive)
                      (e--process-directive directive #'e--process-directive-llm-response)
                      (overlay-put directive 'e-directive-status 'processing)
                      (e--update-instruction-overlay directive t)
                      (setq count (1+ count))))))
      (if (region-active-p)
          (when-let ((toplevel-directives
                      (cl-remove-duplicates
                       (mapcar (lambda (instr)
                                 (e--topmost-instruction instr 'directive))
                               (e--instructions-in (region-beginning)
                                                   (region-end)
                                                   'directive)))))
            (dolist (directive toplevel-directives)
              (execute directive)))
        (if-let ((directive (e--topmost-instruction (e--highest-priority-instruction
                                                     (e--instructions-at (point) 'directive)
                                                     t)
                                                    'directive)))
            (execute directive)
          (when-let ((toplevel-directives (cl-remove-duplicates
                                           (mapcar (lambda (instr)
                                                     (e--topmost-instruction instr 'directive))
                                                   (e--instructions-in (point-min)
                                                                       (point-max)
                                                                       'directive)))))
            (dolist (dir toplevel-directives)
              (execute dir)))))
      (if (> count 0)
          (message "Sent %d directive%s to %s for processing"
                   count
                   (if (> count 1) "s" "")
                   (e--llm-client-name))
        (message "No directives sent to %s" (e--llm-client-name))))))

(defun e-delete-instructions ()
  "Delete instruction(s) either at point or within the selected region.

Display a message to the user showing how many instructions were deleted.
Throw a user error if no instructions to delete were found."
  (interactive)
  (let ((deleted-count 0))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (dolist (overlay (e--wholly-contained-instructions (current-buffer) start end))
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
  (let ((instr-count (length (e--instructions))))
    (when (and (called-interactively-p 'any)
               (zerop instr-count))
      (user-error "No instructions to delete"))
    (when (and (called-interactively-p 'any)
               instr-count
               (not (y-or-n-p "Are you sure you want to delete all instructions?")))
      (user-error "Aborted")))
  (let ((buffer-count 0)
        (deleted-instr-count 0))
    (e--foreach-instruction instr
                            with buffer-hash = (make-hash-table)
                            unless (gethash (overlay-buffer instr) buffer-hash)
                            do (progn
                                 (puthash (overlay-buffer instr) t buffer-hash)
                                 (cl-incf buffer-count))
                            do (progn
                                 (e--delete-instruction instr)
                                 (cl-incf deleted-instr-count)))
    (when (not (zerop deleted-instr-count))
      (message "Deleted %d Evedel instruction%s in %d buffer%s"
               deleted-instr-count
               (if (= 1 deleted-instr-count) "" "s")
               buffer-count
               (if (= 1 buffer-count) "" "s"))))
  (setq e--instructions nil)
  (e--reset-id-counter))

(defun e-convert-instructions ()
  "Convert instructions between reference and directive type.

If a region is selected, convert all instructions within the region.  If no
region is selected, convert only the highest priority instruction at point.

Bodyless directives cannot be converted to references.  Attempting to do so
will throw a user error."
  (interactive)
  (let* ((instructions (if (use-region-p)
                           (e--instructions-in (region-beginning)
                                               (region-end))
                         (cl-remove-if #'null
                                       (list (e--highest-priority-instruction
                                              (e--instructions-at (point))
                                              t)))))
         (num-instructions (length instructions))
         (converted-directives-to-references 0)
         (converted-references-to-directives 0))
    (if (= num-instructions 0)
        (user-error "No instructions to convert")
      (dolist (instr instructions)
        (cond
         ((e--directivep instr)
          (unless (e--bodyless-instruction-p instr)
            (overlay-put instr 'e-instruction-type 'reference)
            (setq converted-directives-to-references (1+ converted-directives-to-references))))
         ((e--referencep instr)
          (overlay-put instr 'e-instruction-type 'directive)
          (setq converted-references-to-directives (1+ converted-references-to-directives)))
         (t
          (user-error "Unknown instruction type")))
        (e--update-instruction-overlay instr t))
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
                          (concat ": " (mapconcat #'identity conversion-msgs " and "))
                        ""))
                 num-instructions
                 (if (> num-instructions 1) "s" ""))
        (when (region-active-p)
          (deactivate-mark))))))

(defun e-next-instruction ()
  "Cycle through instructions in the forward direction."
  (interactive)
  (unless (e--cycle-instruction nil 'next)
    (e--print-instruction-not-found 'next nil)))

(defun e-previous-instruction ()
  "Cycle through instructions in the backward direction."
  (interactive)
  (unless (e--cycle-instruction nil 'previous)
    (e--print-instruction-not-found 'previous nil)))

(defun e-next-reference ()
  "Cycle through references in the forward direction."
  (interactive)
  (unless (e--cycle-instruction 'reference 'next)
    (e--print-instruction-not-found 'next 'reference)))

(defun e-previous-reference ()
  "Cycle through references in the backward direction."
  (interactive)
  (unless (e--cycle-instruction 'reference 'previous)
    (e--print-instruction-not-found 'previous 'reference)))

(defun e-next-directive ()
  "Cycle through directives in the forward direction."
  (interactive)
  (unless (e--cycle-instruction 'directive 'next)
    (e--print-instruction-not-found 'next 'directive)))

(defun e-previous-directive ()
  "Cycle through directives in the backward direction."
  (interactive)
  (unless (e--cycle-instruction 'directive 'previous)
    (e--print-instruction-not-found 'previous 'directive)))

(defun e-preview-directive-prompt ()
  "Preview directive prompt at the current point.

This command is useful to see what is actually being sent to the model."
  (interactive)
  (let ((directive (e--topmost-instruction (car (e--instructions-at (point) 'directive))
                                           'directive)))
    (let ((request-string (e--directive-llm-prompt directive)))
      (let ((bufname "*evedel-directive-preview*"))
        (with-temp-buffer-window bufname
            '((display-buffer-reuse-window
               display-buffer-same-window))
            nil
          (princ (format "<!-- SYSTEM: %s -->"
                         (replace-regexp-in-string "\n"
                                                   "\n# "
                                                   (e--directive-llm-system-message directive))))
          (princ "\n\n")
          (princ request-string)
          (with-current-buffer bufname
            (when (fboundp 'markdown-mode)
              (markdown-mode))
            (read-only-mode 1)
            (visual-line-mode 1)
            (display-line-numbers-mode 1)
            (let ((local-map (make-sparse-keymap)))
              (set-keymap-parent local-map (current-local-map))
              (define-key local-map (kbd "q") 'quit-window)
              (use-local-map local-map))))))))

(defun e-modify-directive-tag-query ()
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
  (if-let ((directive (e--topmost-instruction
                       (e--highest-priority-instruction (e--instructions-at (point)) t)
                       'directive)))
      (let ((query (e--read-tag-query (substring-no-properties
                                       (or
                                        (overlay-get directive
                                                     'e-directive-infix-tag-query-string)
                                        "")))))
        (e--set-directive-tag-query directive query))
    (user-error "No directive at point")))

(defun e-directive-undo (&optional arg)
  "Undo the last change of the directive history at point.

If ARG is nonzero, traverse the directive history backwards; otherwise, forwards."
  (interactive "P")
  (let ((directive (e--highest-priority-instruction
                    (e--instructions-at (point) 'directive))))
    (if directive
        (e--directive-next-history directive (not (null arg)))
      (user-error "No directive found at point"))))

(defun e-add-tags (&optional reference)
  "Add tags to the reference under the point.

Adds specificly to REFERENCE if it is non-nil."
  (interactive)
  (let* ((instructions (e--instructions-at (point) 'reference))
         (instr (or reference (e--highest-priority-instruction instructions t))))
    (if instr
        (let* ((existing-tags (e--available-tags))
               (input (completing-read-multiple "Add tags (or leave empty): "
                                                existing-tags nil nil))
               (new-tags (mapcar #'intern input)))
          (let ((added (e--add-tags instr new-tags)))
            (message "%d tag%s added" added (if (= added 1) "" "s"))))
      (user-error "No reference at point"))))

(defun e-remove-tags ()
  "Remove tags from the reference under the point."
  (interactive)
  (let* ((instructions (e--instructions-at (point) 'reference))
         (instr (e--highest-priority-instruction instructions t)))
    (if instr
        (let ((tags-list (e--reference-tags instr)))
          (if (null tags-list)
              (user-error "Reference has no tags of its own to remove")
            ;; Prompt the user to remove tags.
            (let* ((input (completing-read-multiple "Remove tags: " tags-list nil t))
                   (tags-to-remove (mapcar #'intern input)))
              (let ((removed (e--remove-tags instr tags-to-remove)))
                (message "%d tag%s removed" removed (if (= removed 1) "" "s"))))))
      (user-error "No reference at point"))))

(declare-function e--instruction-with-id "evedel.el")
(let ((map (make-hash-table)))
  (cl-defun e--instruction-with-id (target-id)
    "Return the instruction with the given integer TARGET-ID.

Returns nil if no instruction with the spcific id was found."
    (when-let ((instr (gethash target-id map)))
      (when (buffer-live-p instr)
        (cl-return-from e--instruction-with-id instr)))
    (setq map (make-hash-table))
    (e--foreach-instruction instr
                            do (puthash (e--instruction-id instr) instr map))
    (gethash target-id map)))

(defun e--instruction-id (instruction)
  (overlay-get instruction 'e-id))

(defun e--stashed-buffer-instructions (buffer)
  (e--foreach-instruction (instr buffer)
                          collect (list :overlay-start (overlay-start instr)
                                        :overlay-end (overlay-end instr)
                                        :properties (overlay-properties instr))))

(defun e--stash-buffer (buffer &optional file-contents)
  (let ((instrs (e--stashed-buffer-instructions buffer)))
    (when instrs
      (with-current-buffer buffer
        (let ((original-content (or file-contents (buffer-substring-no-properties (point-min)
                                                                                  (point-max)))))
          (setf (alist-get buffer e--instructions)
                (list :original-content original-content
                      :instructions instrs)
                (car (assoc buffer e--instructions))
                (buffer-file-name buffer))
          (mapc #'delete-overlay (e--instructions-in (point-min) (point-max))))))))

(defun e--reference-list-info (refs)
  "Return a plist with information regarding REFS list.

:buffer-count - Amount of buffers with references from REFS
:line-count   - Amount of total lines spanned by top-level references"
  (let ((bufhash (make-hash-table))
        (buffer-count 0)
        (line-count 0))
    (cl-loop
     for ref in refs
     do (let ((buffer (overlay-buffer ref))
              (start (overlay-start ref))
              (end (overlay-end ref)))
          (if-let ((line-ranges (gethash buffer bufhash)))
              (cl-loop for range in line-ranges
                       do (cl-destructuring-bind (range-start . range-end) range
                            (when (<= start range-start range-end end)
                              (setf (car range) start
                                    (cdr range) end)
                              (cl-return)))
                       finally (puthash buffer (push (cons start end) line-ranges) bufhash))
            (puthash buffer `((,(overlay-start ref) . ,(overlay-end ref))) bufhash)))
     finally (setq buffer-count
                   (hash-table-count bufhash))
     (maphash (lambda (buffer ranges)
                (with-current-buffer buffer
                  (cl-loop for (beg . end) in ranges
                           do (let ((end-lineno (line-number-at-pos end))
                                    (beg-lineno (line-number-at-pos beg)))
                                (setq line-count
                                      (+ line-count (+ 1 (- end-lineno beg-lineno))))))))
              bufhash)
     (cl-return (list :buffer-count buffer-count :line-count line-count)))))

(defun e--reference-list-info-string (refs)
  (cl-destructuring-bind (&key buffer-count line-count)
      (e--reference-list-info refs)
    (let ((ref-count (length refs)))
      (format "%d hit%s in %d buffer%s, %d line%s"
              ref-count
              (if (= ref-count 1) "" "s")
              buffer-count
              (if (= buffer-count 1) "" "s")
              line-count
              (if (= line-count 1) "" "s")))))

(defun e--read-tag-query (&optional default)
  "Prompt user via minibuffer for a tag query text.

DEFAULT is the default query to display in the minibuffer.
Returns the validated query string."
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((timer nil)
              (minibuffer-message))
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
                                   (let* ((input (minibuffer-contents))
                                          (query (read (concat "(" input ")"))))
                                     (let ((refs (e--filter-references
                                                  (e--tag-query-prefix-from-infix query))))
                                       (setq minibuffer-message
                                             (e--reference-list-info-string refs))))
                                 (error
                                  (let ((errmsg (error-message-string err)))
                                    (setq minibuffer-message errmsg))))
                               (when minibuffer-message
                                 (set-minibuffer-message minibuffer-message))))))
                    nil t)))
    (let ((default (or default "")))
      (let ((input (read-from-minibuffer "Tag query: " default)))
        (let ((query (read (format "(%s)" input))))
          (condition-case err
              (progn
                (e--tag-query-prefix-from-infix query)
                (mapconcat (lambda (q) (format "%s" q)) query " "))
            (error
             (let ((errmsg (error-message-string err)))
               (user-error errmsg)))))))))

(defun e--set-directive-tag-query (directive query)
  "Set the tag query for DIRECTIVE to QUERY string."
  (condition-case err
      (let ((parsed-prefix-tag-query
             (e--tag-query-prefix-from-infix (read (concat "(" query ")")))))
        (overlay-put directive 'e-directive-prefix-tag-query parsed-prefix-tag-query)
        (if (string-empty-p query)
            (overlay-put directive 'e-directive-infix-tag-query-string nil)
          (overlay-put directive
                       'e-directive-infix-tag-query-string
                       (e--apply-face-to-match "\\b\\(?:(*not\\|or\\|and\\)\\b\\|(\\|)"
                                               (e--apply-face-to-match
                                                "\\(:?.+\\)"
                                                query
                                                'font-lock-constant-face)
                                               nil))
          (overlay-put directive 'e-directive-status nil))
        (e--update-instruction-overlay directive t))
    (error
     (message (error-message-string err)))))

(defun e--print-instruction-not-found (direction type)
  "Print a not found message for the given DIRECTION and TYPE."
  (let ((type-string (pcase type
                       ('directive "directive")
                       ('reference "reference")
                       (_ "instruction"))))
    (message "No %s %s found"
             (if (eq direction 'next) "next" "previous")
             type-string)))

(cl-defun e--reference-matches-query-p (reference query)
  "Return t only if REFERENCE matches the tag QUERY."
  (unless reference
    (cl-return-from e--reference-matches-query-p nil))
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) e-empty-tag-query-matches-all)
        t
      (let ((tags (e--reference-tags reference t))
            (direct-tags (e--reference-tags reference nil))
            (instr-id (lambda (tag) (let ((tagname (symbol-name tag)))
                                      (when (string-match "^id:\\([1-9][0-9]*\\)$" tagname)
                                        (string-to-number (match-string 1 tagname)))))))
        (if (and (null direct-tags) e-always-match-untagged-references)
            t
          (let ((atom-bindings (mapcar (lambda (atom)
                                         (pcase atom
                                           ('is:bufferlevel
                                            (e--instruction-bufferlevel-p reference))
                                           ('is:subreference
                                            (e--parent-instruction reference 'reference))
                                           ('is:tagless
                                            (null tags))
                                           ('is:directly-tagless
                                            (null (e--reference-tags reference nil)))
                                           ('is:with-commentary
                                            (not (string-empty-p (e--commentary-text reference))))
                                           (_ (if-let ((id (funcall instr-id atom)))
                                                  (= id (e--instruction-id reference))
                                                (member atom tags)))))
                                       atoms)))
            (cl-progv atoms atom-bindings
              (eval query))))))))

(defun e--filter-references (query)
  "Return a list of all references filtered by the tag QUERY.

See `evedel--tag-query-prefix-from-infix' for QUERY format."
  (let ((atoms (cl-remove-duplicates (cl-remove-if (lambda (elm)
                                                     (member elm '(not or and nil)))
                                                   (flatten-tree query)))))
    (if (and (null atoms) e-empty-tag-query-matches-all)
        (e--foreach-instruction instr when (e--referencep instr) collect instr)
      (e--foreach-instruction instr
                              when (and (e--referencep instr)
                                        (e--reference-matches-query-p instr query))
                              collect instr))))

(defun e--available-tags ()
  "Return a list of all the tags in the loaded references."
  (let ((tags-hash (make-hash-table)))
    (e--foreach-instruction (ref)
                            do (when (e--referencep ref)
                                 (cl-loop for tag in (e--reference-tags ref)
                                          do (puthash tag t tags-hash))))
    (hash-table-keys tags-hash)))

(defun e--cycle-instruction (type direction)
  "Get the next or previous instruction overlay of TYPE.
DIRECTION should be `next' or `previous' from the current point.

If no instruction found in the buffer, checks the next buffers in
the `evedel--instructions' alist.

Returns the found instruction, if any."
  ;; We want the buffers to be a cyclic list, based on the current buffer.
  (let* ((buffers (let ((bufs (mapcar #'car e--instructions)))
                    (if (eq direction 'next)
                        (e--cycle-list-around (current-buffer) bufs)
                      (e--cycle-list-around (current-buffer) (nreverse bufs)))))
         (original-buffer (current-buffer))
         (found-instr))
    (while (and buffers (null found-instr))
      (let* ((buffer (car buffers))
             (instrs (e--foreach-instruction (instr buffer) collect instr)))
        (setq buffers (delq buffer buffers))
        (when type
          (setq instrs (cl-remove-if-not (lambda (instr)
                                           (eq (e--instruction-type instr) type))
                                         instrs)))
        (let ((sorting-pred (pcase direction
                              ('next #'<)
                              ('previous #'>))))
          (when (eq buffer original-buffer)
            (setq instrs (cl-remove-if-not (lambda (instr)
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

(defun e--add-tags (reference tags)
  "Add TAGS to REFERENCE.

TAGS should be a list of symbols.
Returns the number of new tags added."
  (let* ((tag-type 'e-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-remove-if (lambda (tag) (member tag existing-tags)) tags)))
    (overlay-put reference tag-type (cl-union existing-tags new-tags :test 'eq))
    (let ((added (length new-tags)))
      (when (> added 0)
        (e--update-instruction-overlay reference t))
      added)))

(defun e--remove-tags (reference tags)
  "Remove TAGS from REFERENCE.

TAGS should be a list of symbols.
Returns the number of tags removed."
  (let* ((tag-type 'e-reference-tags)
         (existing-tags (overlay-get reference tag-type))
         (new-tags (cl-set-difference existing-tags tags :test 'eq)))
    (overlay-put reference tag-type new-tags)
    (let ((removed (- (length existing-tags) (length new-tags))))
      (when (> removed 0)
        (e--update-instruction-overlay reference t))
      removed)))

(defun e--inherited-tags (reference)
  "Return the list of all tags that REFERENCE inherits from its parents."
  (when-let ((parent (e--parent-instruction reference 'reference)))
    (e--reference-tags parent t)))

(defun e--reference-tags (reference &optional include-parent-tags)
  "Return the list of tags for the given REFERENCE.

If INCLUDE-PARENT-TAGS is non-nil, gets te parent's tags as well."
  (if (not include-parent-tags)
      (overlay-get reference 'e-reference-tags)
    (append (overlay-get reference 'e-reference-tags)
            (when-let ((parent (e--parent-instruction reference 'reference)))
              (e--reference-tags parent t)))))

(defun e--delete-instruction-at (point)
  "Delete the instruction at POINT.

Returns the deleted instruction overlay."
  (let* ((instructions (e--instructions-at point))
         (target (e--highest-priority-instruction instructions t)))
    (when target
      (e--delete-instruction target))))

(defun e--being-processed-p (instruction)
  "Return non-nil if the directive INSTRUCTION is being processed."
  (eq (overlay-get instruction 'e-directive-status) 'processing))

(defun e--directive-empty-p (directive)
  "Check if DIRECTIVE is empty.

A directive is empty if it does not have a body or secondary directives."
  (let ((subdirectives
         (cl-remove-if-not #'e--directivep
                           (e--wholly-contained-instructions (overlay-buffer directive)
                                                             (overlay-start directive)
                                                             (overlay-end directive)))))
    (not (cl-some (lambda (subdir)
                    (not (string-empty-p (e--directive-text subdir))))
                  subdirectives))))

(defun e--create-instruction (type)
  "Create or scale an instruction of the given TYPE within the selected region.

If a region is selected but partially covers an existing instruction, then the
function will resize it.  See either `evedel-create-reference' or
`evedel-create-directive' for details on how the resizing works."
  (if (use-region-p)
      (let ((intersecting-instructions
             (cl-remove-if (lambda (instr)
                             (xor (= (overlay-start instr) (region-beginning))
                                  (= (overlay-end instr) (region-end))))
                           (e--partially-contained-instructions (current-buffer)
                                                                (region-beginning)
                                                                (region-end)))))
        (if-let ((instructions
                  (cl-remove-if-not (lambda (instr)
                                      (eq (e--instruction-type instr) type))
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
          ;; Else- there are no partially contained instructions of the same type within the
          ;; region...
          (when (or intersecting-instructions
                    (or (cl-some (lambda (instr)
                                   (and (= (overlay-start instr) (region-beginning))
                                        (= (overlay-end instr) (region-end))))
                                 (e--instructions-in (region-beginning) (region-end)))))
            ;; ...but there are intersecting instructions of another type, or another instruction
            ;; existing precisely at the start of another.
            (user-error "Instruction intersects with existing instruction"))
          (let* ((buffer (current-buffer))
                 (instruction (if (eq type 'reference)
                                  (e--create-reference-in buffer
                                                          (region-beginning)
                                                          (region-end))
                                (save-window-excursion
                                  (let ((pos (region-beginning)))
                                    (unless (<= (window-start) pos (window-end))
                                      (set-window-start (selected-window)
                                                        (max (point-min)
                                                             (- (region-beginning)
                                                                (- (window-end) (window-start))))))
                                    (e--create-directive-in buffer
                                                            (region-beginning)
                                                            (region-end)))))))
            (with-current-buffer buffer
              (deactivate-mark)
              (when (eq type 'reference)
                (e-add-tags instruction)))
            instruction)))
    (when (eq type 'directive)
      (prog1 (e--create-directive-in (current-buffer) (point) (point) t)
        (deactivate-mark)))))

(cl-defun e--process-directive-llm-response (response directive status)
  "Process RESPONSE string for sent DIRECTIVE.

Removes any superfluous markup formatting and indents the response according to
the current buffer."
  (unless (overlay-buffer directive)
    ;; Directive is gone...
    (cl-return-from e--process-directive-llm-response))
  (unless (eq (overlay-get directive 'e-directive-status) 'processing)
    ;; The directive has been modified.  Do not continue.
    (cl-return-from e--process-directive-llm-response))
  (cl-flet ((mark-failed (reason)
              (overlay-put directive 'e-directive-status 'failed)
              (overlay-put directive 'e-directive-fail-reason reason)))
    (cond
     ((null response)
      (mark-failed status))
     ((eq status 'aborted)
      (mark-failed "The request has been aborted."))
     (t
      (let* ((response-code-blocks (e--markdown-code-blocks response))
             (parsed-response (car response-code-blocks)))
        (if (/= (length response-code-blocks) 1)
            (mark-failed response)
          (overlay-put directive 'e-directive-status 'succeeded)
          (with-current-buffer (overlay-buffer directive)
            (let ((beg (overlay-start directive))
                  (end (overlay-end directive)))
              ;; Add current directive text to history.
              (let ((current-text (buffer-substring-no-properties beg end)))
                (let ((trimmed-text (string-trim current-text)))
                  (unless (string-empty-p trimmed-text)
                    (push current-text (overlay-get directive 'e-directive-history)))))
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
                    (indent-region beg end)))
                (overlay-put directive 'evaporate t))))))))
    (e--update-instruction-overlay directive t)))

(defun e--directive-next-history (directive &optional backwards)
  "Cycle through the directive history.

DIRECTIVE is an instruction directive overlay.
If BACKWARDS is non-nil, traverse the history backward."
  (when-let ((history (overlay-get directive 'e-directive-history)))
    (let ((current-text (buffer-substring-no-properties
                         (overlay-start directive)
                         (overlay-end directive))))
      (if backwards
          ;; Traverse backwards: Get the last element of the history.
          (let ((prev-text (car (last history))))
            ;; Replace current directive text with the previous directive.
            (e--replace-text (overlay-start directive) (overlay-end directive) prev-text)
            ;; Append current text to the end of the history list.
            (setf (overlay-get directive 'e-directive-history)
                  (append (butlast history) (list current-text))))
        ;; Traverse forward: Get the first element of the history.
        (save-excursion
          (let ((next-text (car history)))
            ;; Replace current directive text with the next directive.
            (e--replace-text (overlay-start directive) (overlay-end directive) next-text)))
        ;; Move current text to the end of the history list.
        (setf (overlay-get directive 'e-directive-history)
              (append (cdr history) (list current-text)))))))

(defun e--referencep (instruction)
  (eq (e--instruction-type instruction) 'reference))

(defun e--directivep (instruction)
  (eq (e--instruction-type instruction) 'directive))

(cl-defun e--highest-priority-instruction (instructions &optional return-highlighted)
  "Return the instruction with the highest priority from the INSTRUCTIONS list.

Priority here refers to the priority property used by overlays.

If RETURN-HIGHLIGHTED is non-nil and `evedel--highlighted-instruction' is
non-nil, the function will return `evedel--highlighted-instruction' if it is
also in the INSTRUCTIONS list."
  (when (and return-highlighted
             e--highlighted-instruction
             (member e--highlighted-instruction instructions))
    (cl-return-from e--highest-priority-instruction e--highlighted-instruction))
  (cl-reduce (lambda (acc instruction)
               (if (or (not acc)
                       (> (or (overlay-get instruction 'priority)
                              e--default-instruction-priority)
                          (or (overlay-get acc 'priority)
                              e--default-instruction-priority)))
                   instruction
                 acc))
             instructions
             :initial-value nil))

(defun e--instruction-type (instruction)
  "Return the type of the INSTRUCTION overlay.

Instruction type can either be `reference' or `directive'."
  (if-let ((type (overlay-get instruction 'e-instruction-type)))
      type
    (error "%s is not an instruction overlay" instruction)))

(defun e--create-instruction-overlay-in (buffer start end)
  "Create an overlay in BUFFER from START to END of the lines."
  (make-local-variable 'e--after-change-functions-hooked)
  (with-current-buffer buffer
    (let ((is-bufferlevel
           ;; Check if the overlay spans the start and end of the buffer.  If it does, make it
           ;; sticky so that additions to edges of the buffer will cause it to expand there.  This
           ;; is useful for when we want to append new text to the end of the buffer but don't want
           ;; to "invalidate" the buffer-level status of the instruction.
           (and (= start (point-min)) (= end (point-max)))))
      (let ((overlay (make-overlay start end (current-buffer) nil is-bufferlevel)))
        (overlay-put overlay 'e-instruction t)
        (overlay-put overlay 'e-id (e--create-id))
        (push overlay (alist-get buffer e--instructions))
        (unless (bound-and-true-p e--after-change-functions-hooked)
          (setq-local e--after-change-functions-hooked t)
          (add-hook 'after-change-functions
                    (lambda (beg end _len)
                      (let ((beg (max (point-min) (1- beg)))
                            (end (min (point-max) (1+ end))))
                        (let ((affected-instructions (e--instructions-in beg end)))
                          (dolist (instruction affected-instructions)
                            (e--update-instruction-overlay instruction)))))
                    nil t))
        (e--setup-buffer-hooks buffer)
        overlay))))

(defun e--instruction-p (overlay)
  "Return non-nil if OVERLAY is an instruction overlay."
  (overlay-get overlay 'e-instruction))

(defun e--parent-instruction (instruction &optional of-type)
  "Return the parent of the given INSTRUCTION overlay.

If OF-TYPE is non-nil, returns the parent with the given type."
  (with-current-buffer (overlay-buffer instruction)
    (let ((beg (overlay-start instruction))
          (end (overlay-end instruction)))
      (e--highest-priority-instruction (cl-remove-if-not (lambda (instr)
                                                           (and (not (eq instr instruction))
                                                                (or (null of-type)
                                                                    (eq (e--instruction-type instr)
                                                                        of-type))
                                                                (<= (overlay-start instr) beg
                                                                    end (overlay-end instr))))
                                                         (e--instructions-in beg end))))))

(defun e--bodyless-instruction-p (instr)
  "Return non-nil if the INSTR instruction has a body."
  (= (overlay-start instr) (overlay-end instr)))

(defun e--subinstruction-of-p (sub parent)
  "Return t is instruction SUB is contained entirely within instruction PARENT.

In this case, an instruction is _not_ considered a subinstruction of itself."
  (and (eq (overlay-buffer sub)
           (overlay-buffer parent))
       (<= (overlay-start parent) (overlay-start sub) (overlay-end sub) (overlay-end parent))
       (and (/= (overlay-start parent) (overlay-start sub))
            (/= (overlay-end parent) (overlay-end sub)))))

(cl-defun e--child-instructions (instruction)
  "Return the direct child instructions of the given INSTRUCTION overlay."
  ;; Bodyless instructions cannot have any children.
  (when (e--bodyless-instruction-p instruction)
    (cl-return-from e--child-instructions nil))
  (let ((children (cl-remove-if (lambda (instr)
                                  (or (eq instr instruction)
                                      (and (= (overlay-start instr) (overlay-start instruction))
                                           (= (overlay-end instr) (overlay-end instruction)))))
                                (e--wholly-contained-instructions (overlay-buffer instruction)
                                                                  (overlay-start instruction)
                                                                  (overlay-end instruction)))))
    (dolist (child children)
      (setq children (cl-set-difference children
                                        (e--child-instructions child))))
    children))

(defun e--create-reference-in (buffer start end)
  "Create a region reference from START to END in BUFFER."
  (let ((ov (e--create-instruction-overlay-in buffer start end)))
    (overlay-put ov 'e-instruction-type 'reference)
    (overlay-put ov 'evaporate t)
    (e--update-instruction-overlay ov t)
    ov))

(defun e--create-directive-in (buffer start end &optional bodyless directive-text)
  "Create a region directive from START to END in BUFFER.

This function switches to another buffer midway of execution.
BODYLESS controls special formatting if non-nil.

DIRECTIVE-TEXT is used as the default directive.  Having DIRECTIVE-TEXT be
non-nil prevents the opening of a prompt buffer."
  (let ((ov (e--create-instruction-overlay-in buffer start end)))
    (unless bodyless
      (overlay-put ov 'evaporate t))
    (overlay-put ov 'e-instruction-type 'directive)
    (overlay-put ov 'e-directive (or directive-text ""))
    (e--update-instruction-overlay ov (not bodyless))
    (unless directive-text
      (deactivate-mark)
      (e--read-directive ov))
    ov))

(defun e--delete-instruction (instruction &optional buffer)
  "Delete the INSTRUCTION overlay and return it.

If the overlay is already dead, just perform the cleanup.
BUFFER is required in order to perform cleanup on a dead instruction."
  ;; We want to handle this function in two different ways.  The first way handles regular deletion,
  ;; i.e. when the function was invoked on an existing instruction.  The second way is for when the
  ;; instruction was deleted uncanonically through text manipulation.  In the latter case, the
  ;; function will be called during a cleanup routine and the instruction will not be alive.
  (when (overlay-get instruction 'e-marked-for-deletion)
    (error "Instruction %s already marked for deletion" instruction))
  (overlay-put instruction 'e-marked-for-deletion t)
  (cl-labels ((cleanup (instr buffer)
                (let ((id (e--instruction-id instr)))
                  (e--retire-id id)
                  (e-unlink-instructions `(,id) (e--instruction-outlinks instr))
                  (e-unlink-instructions (e--instruction-inlinks instr) `(,id)))
                (setf (cdr (assoc buffer e--instructions))
                      (delq instr (cdr (assoc buffer e--instructions))))))
    (let ((ov-buffer (overlay-buffer instruction)))
      (when (buffer-live-p ov-buffer)
        (let ((children (e--child-instructions instruction)))
          (delete-overlay instruction)
          (dolist (child children)
            (e--update-instruction-overlay child t))))
      (cleanup instruction (or ov-buffer
                               buffer
                               (error "Cannot perform cleanup without a buffer")))))
  instruction)

(defun e--instructions-congruent-p (a b)
  "Return t only if instruction overlays A and B are congruent."
  (and (eq (overlay-buffer a) (overlay-buffer b))
       (= (overlay-start a) (overlay-start b))
       (= (overlay-end a) (overlay-end b))))

(defun e--instruction-bufferlevel-p (instruction)
  "Return t if INSTRUCTION contains the entirety of its buffer."
  (let ((buffer (overlay-buffer instruction)))
    (when buffer
      (with-current-buffer buffer
        (and (= (overlay-start instruction) (point-min))
             (= (overlay-end instruction) (point-max)))))))

(defun e--update-instruction-overlay (instruction &optional update-children)
  "Update the appearance of the INSTRUCTION overlay.

This function updates the overlay label text, color of the label text, and the
background of the instruction overlay.  This function should be called every
time there is a hierarchy or status change in the instruction overlay that we
wish to reflect.

Also updates the child instructions of the INSTRUCTION, if UPDATE-CHILDREN is
non-nil."
  (cl-labels
      ((directive-color (directive)
         (cl-labels ((dircol ()
                       (pcase (overlay-get directive 'e-directive-status)
                         ('processing e-directive-processing-color)
                         ('succeeded  e-directive-success-color)
                         ('failed     e-directive-fail-color)
                         (_           e-directive-color))))
           (if-let ((parent-directive (e--topmost-instruction directive 'directive)))
               (let ((parent-status (overlay-get parent-directive 'e-directive-status)))
                 (if (eq parent-status 'processing)
                     e-directive-processing-color
                   (if (eq parent-status 'failed)
                       e-directive-fail-color
                     (dircol))))
             (dircol))))
       (aux (instruction &optional update-children priority (parent nil))
         (let* ((instruction-type (e--instruction-type instruction))
                (padding (with-current-buffer (overlay-buffer instruction)
                           (save-excursion
                             (goto-char (overlay-start instruction))
                             (make-string (current-column) ? ))))
                (is-bufferlevel (e--instruction-bufferlevel-p instruction))
                (parent-bufferlevel (and parent (e--instruction-bufferlevel-p parent)))
                ;; This is to prevent the buffer-level instruction from having a background color.
                (priority (if is-bufferlevel (1- priority) priority))
                (label "")
                color)
           (cl-labels
               ((append-to-label (content &optional prefix)
                  (setq label
                        (concat label
                                (if (string-empty-p label) "" (concat "\n" padding))
                                (e--fill-label-string content
                                                      (or prefix "")
                                                      padding
                                                      (overlay-buffer instruction)))))
                (stylized-id-str (id)
                  (propertize (format "#%d" id) 'face 'font-lock-constant-face))
                (append-links-to-label ()
                  (cl-labels ((filter-ids (ids)
                                (cl-loop for id in ids
                                         unless
                                         (let ((instr (e--instruction-with-id id)))
                                           (or (null instr)
                                               (not (eq (e--instruction-type instr)
                                                        instruction-type))))
                                         collect id)))
                    (let ((outlinks (filter-ids (e--instruction-outlinks instruction)))
                          (inlinks (filter-ids (e--instruction-inlinks instruction))))
                      (when (or outlinks inlinks)
                        (let ((prefix (format "%s LINKS: "
                                              (if (eq instruction-type instruction)
                                                  "REFERENCE"
                                                "DIRECTIVE")))
                              (link-list-text
                               (concat
                                (when outlinks
                                  (format "TO: %s"
                                          (string-join (mapcar #'stylized-id-str
                                                               outlinks)
                                                       ", ")))
                                (when inlinks
                                  (format "%sFROM: %s"
                                          (if outlinks "\n" "")
                                          (string-join (mapcar #'stylized-id-str
                                                               inlinks)
                                                       ", "))))))
                          (append-to-label link-list-text
                                           prefix)))))))
             (pcase instruction-type
               ('reference ; REFERENCE
                (setq color e-reference-color)
                (if (and parent
                         (and (eq (e--instruction-type parent) 'reference)
                              (not parent-bufferlevel)))
                    (append-to-label (format "SUBREFERENCE %s"
                                             (stylized-id-str (e--instruction-id instruction))))
                  (if is-bufferlevel
                      (append-to-label (format "BUFFER REFERENCE %s"
                                               (stylized-id-str (e--instruction-id instruction))))
                    (append-to-label (format "REFERENCE %s"
                                             (stylized-id-str (e--instruction-id instruction))))))
                (let* ((direct-tags (e--reference-tags instruction))
                       (inherited-tags (e--inherited-tags instruction))
                       (common-tags (cl-intersection inherited-tags direct-tags))
                       (unique-tags (cl-set-difference direct-tags common-tags)))
                  (cl-labels
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
                                         "TAGS: ")))))
                (append-links-to-label)
                (let ((commentary (string-trim (or (e--commentary-text instruction)
                                                   ""))))
                  (unless (string-empty-p commentary)
                    (append-to-label commentary "COMMENTARY: "))))
               ('directive ; DIRECTIVE
                (pcase (overlay-get instruction 'e-directive-status)
                  ('processing (append-to-label "PROCESSING"))
                  ('succeeded (append-to-label "SUCCEEDED"))
                  ('failed (append-to-label (overlay-get instruction
                                                         'e-directive-fail-reason)
                                            "FAILED: ")))
                (setq color (directive-color instruction))
                (let (sublabel
                      (directive-typename "DIRECTIVE"))
                  (if (and parent
                           (e--directivep parent))
                      (progn
                        (pcase (overlay-get parent 'e-directive-status)
                          ((or 'processing 'failed)
                           (if-let ((existing-typename (overlay-get instruction
                                                                    'e-subdirective-typename)))
                               (setq directive-typename existing-typename)
                             (setq directive-typename "HINT")))
                          ('succeeded (setq directive-typename "CORRECTION"))
                          (_ (setq directive-typename "HINT")))
                        (setf (overlay-get instruction 'e-subdirective-typename)
                              directive-typename))
                    (setf (overlay-get instruction 'e-subdirective-typename) nil))
                  (setq sublabel (concat
                                  sublabel
                                  (format "%s %s"
                                          directive-typename
                                          (stylized-id-str (e--instruction-id instruction)))))
                  (let ((directive (string-trim (or (overlay-get instruction 'e-directive)
                                                    ""))))
                    (if (string-empty-p directive)
                        (setq sublabel (concat "EMPTY " sublabel))
                      (setq sublabel (concat sublabel ": ")))
                    (setq label (concat
                                 label
                                 (unless (string-empty-p label)
                                   (concat "\n" padding))
                                 (e--fill-label-string directive
                                                       sublabel
                                                       padding
                                                       (overlay-buffer instruction))))
                    (unless (e--parent-instruction instruction 'directive)
                      (if-let ((query-string (overlay-get instruction
                                                          'e-directive-infix-tag-query-string)))
                          (append-to-label query-string "TAG QUERY: ")
                        (let (matchinfo)
                          (if e-empty-tag-query-matches-all
                              (setq matchinfo "REFERENCES ALL")
                            (if e-always-match-untagged-references
                                (setq matchinfo "REFERENCES UNTAGGED ONLY")
                              (setq matchinfo "REFERENCES NOTHING")))
                          (setq label (concat label "\n" padding matchinfo))))
                      (append-links-to-label))))))
             (let* ((default-fg (face-foreground 'default))
                    (default-bg (face-background 'default))
                    (bg-tint-intensity
                     (if (and parent (not parent-bufferlevel))
                         (* e-subinstruction-tint-coefficient e-instruction-bg-tint-intensity)
                       e-instruction-bg-tint-intensity))
                    (label-color (if is-bufferlevel
                                     (e--tint default-fg color e-instruction-label-tint-intensity)
                                   (let ((tint (e--tint default-fg
                                                        color
                                                        e-instruction-label-tint-intensity)))
                                     (dotimes (_  (- priority
                                                     e--default-instruction-priority))
                                       (setq tint (e--tint tint
                                                           color
                                                           e-instruction-label-tint-intensity)))
                                     tint)))
                    ;; We want to make sure that the buffer-level instructions don't superfluously
                    ;; tint the background.
                    (bg-color (if (and is-bufferlevel (eq instruction-type 'reference))
                                  default-bg
                                (let ((tint (e--tint default-bg
                                                     color
                                                     e-instruction-bg-tint-intensity)))
                                  (dotimes (_ (- priority
                                                 e--default-instruction-priority))
                                    (setq tint (e--tint tint color bg-tint-intensity)))
                                  tint))))
               (overlay-put instruction 'e-bg-color bg-color)
               (overlay-put instruction 'e-label-color label-color)
               (overlay-put instruction 'priority priority)
               (when (eq instruction
                         e--highlighted-instruction)
                 (setq bg-color
                       (e--tint default-bg
                                e-highlighted-instruction-color
                                e-highlighted-instruction-tint-intensity)))
               (let ((instruction-is-at-eol (with-current-buffer (overlay-buffer instruction)
                                              (save-excursion
                                                (goto-char (overlay-end instruction))
                                                (eolp)))))
                 ;; Propertize specific parts of the before-string of the label, to give
                 ;; the illusion that its a "sticker" in the buffer.
                 (cl-labels
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
                        (when-let ((parent (e--parent-instruction instruction)))
                          (colorize-region beg end
                                           (overlay-get parent 'e-label-color)
                                           (overlay-get parent 'e-bg-color)))))
                   (let ((before-string
                          (with-temp-buffer
                            (insert label)
                            (if (e--bodyless-instruction-p instruction)
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
                            (unless (e--bodyless-instruction-p instruction)
                              (let ((mark (point)))
                                (insert padding)
                                (colorize-region-as-parent mark (point))))
                            (buffer-string))))
                     (overlay-put instruction 'before-string before-string))))
               (overlay-put instruction 'face `(:extend t :background ,bg-color)))
             (when update-children
               (dolist (child (e--child-instructions instruction))
                 (aux child update-children (1+ priority) instruction)))))))
    (let ((instructions-conflicting (cl-some (lambda (instr)
                                               (and (not (eq instr instruction))
                                                    (e--instructions-congruent-p instruction
                                                                                 instr)))
                                             (e--instructions-at (overlay-start instruction)))))
      (if instructions-conflicting
          ;; This instruction is causing conflicts, and therefore must be deleted.
          (e--delete-instruction instruction)
        (let ((parent (e--parent-instruction instruction)))
          (let ((priority (if parent
                              (1+ (overlay-get parent 'priority))
                            e--default-instruction-priority)))
            (aux instruction update-children priority parent)))))))

(defun e--buffer-has-instructions-p (buffer)
  (assoc buffer e--instructions))

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

Does not return instructions that contain the region in its entirety."
  (with-current-buffer buffer
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'e-instruction)
                             (or (<= (overlay-start ov) start)
                                 (>= (overlay-end ov) end))
                             (not (and (<= (overlay-start ov) start)
                                       (>= (overlay-end ov) end)))))
                      (overlays-in start end))))

(defun e--instructions ()
  "Return a list of all currently loaded instructions."
  (e--foreach-instruction inst collect inst))

(cl-defun e--topmost-instruction (instruction &optional of-type pred)
  "Return the topmost instruction containing the INSTRUCTION, if any.

If OF-TYPE is non-nil, filter by the specified instruction OF-TYPE.
If OF-TYPE is nil, the instruction returned is the top-level one.

If PRED is non-nil, then the best instruction must also satisfy it.
The PRED must be a function which accepts an instruction."
  (unless instruction
    (cl-return-from e--topmost-instruction nil))
  (with-current-buffer (overlay-buffer instruction)
    (let ((best-instruction instruction))
      (cl-labels ((parent-instr (instr)
                    (if-let ((parent (e--parent-instruction instr)))
                        (progn
                          (when (and (or (null of-type) (eq of-type (e--instruction-type parent)))
                                     (or (null pred) (funcall pred parent)))
                            (setq best-instruction parent))
                          (parent-instr parent))
                      best-instruction)))
        (setq best-instruction (parent-instr instruction)))
      (if (and (or (null of-type) (eq of-type (e--instruction-type best-instruction)))
               (or (null pred) (funcall pred best-instruction)))
          best-instruction
        nil))))

(defun e--toplevel-instructions (&optional of-type)
  "Return the global top-level instructions across all buffers.

Returns only instructions of specific type if OF-TYPE is non-nil.
If OF-TYPE is non-nil, this function does _not_ return the \"next best\"
instruction of the matching type; i.e., the returned list consists only of
toplevel instructions that also match the specified type."
  (e--foreach-instruction instr
                          with toplevels = (make-hash-table)
                          with inferiors = (make-hash-table)
                          unless (or (gethash instr toplevels) (gethash instr inferiors))
                          do (with-current-buffer (overlay-buffer instr)
                               (let* ((instrs (e--instructions-at (overlay-start instr)))
                                      (topmost (car (cl-remove-if #'e--parent-instruction instrs)))
                                      (children (delq topmost instrs)))
                                 (puthash topmost t toplevels)
                                 (cl-loop for child in children do (puthash child t inferiors))))
                          finally (cl-return (if of-type
                                                 (cl-remove-if-not (lambda (instr)
                                                                     (eq (e--instruction-type instr)
                                                                         of-type))
                                                                   (hash-table-keys toplevels))
                                               (hash-table-keys toplevels)))))

(defun e--directive-text (directive)
  "Return the directive text of the DIRECTIVE overlay.

Returns an empty string if there is no directive text."
  (or (overlay-get directive 'e-directive) ""))

(defun e--commentary-text (reference)
  "Return the commentary text of the REFERENCE overlay.

Returns an empty string if there is no commentary."
  (or (overlay-get reference 'e-commentary) ""))

(defun e--read-directive (directive)
  "Prompt user to enter a directive text via minibuffer for DIRECTIVE."
  (let ((original-directive-text (e--directive-text directive))
        (original-directive-status (overlay-get directive 'e-directive-status)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((directive-text (minibuffer-contents)))
                        (overlay-put directive 'e-directive directive-text)
                        (e--update-instruction-overlay directive)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put directive 'e-directive (minibuffer-contents))
                      (overlay-put directive 'e-directive-status nil)
                      (e--update-instruction-overlay directive))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Directive: " original-directive-text)
        (quit
         (if (string-empty-p original-directive-text)
             (e--delete-instruction directive)
           (overlay-put directive 'e-directive original-directive-text)
           (overlay-put directive 'e-directive-status original-directive-status)
           (e--update-instruction-overlay directive nil))
         (signal 'quit nil))))))

(defun e--read-commentary (reference)
  "Prompt user to enter a commentary text via minibuffer for REFERENCE."
  (let ((original-commentary-text (e--commentary-text reference)))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook
                    (lambda ()
                      (let ((commentary-text (minibuffer-contents)))
                        (overlay-put reference 'e-commentary commentary-text)
                        (e--update-instruction-overlay reference)))
                    nil t)
          (add-hook 'after-change-functions
                    (lambda (_beg _end _len)
                      (overlay-put reference 'e-commentary (minibuffer-contents))
                      (e--update-instruction-overlay reference))
                    nil t))
      (condition-case _err
          (read-from-minibuffer "Commentary: " original-commentary-text)
        (quit
         (overlay-put reference 'e-commentary original-commentary-text)
         (e--update-instruction-overlay reference nil))
        (signal 'quit nil)))))

(defun e--toplevel-references ()
  "Fetch all toplevel reference instructions.

A toplevel reference instruction is one that has no parents."
  (seq-filter (lambda (instr)
                (and (null (e--parent-instruction instr))
                     (e--referencep instr)))
              (e--instructions)))

(cl-defun e--ancestral-instructions (instruction &optional of-type)
  "Return a list of ancestors for the current INSTRUCTION."
  (if-let ((parent (e--parent-instruction instruction)))
      (if (or (null of-type)
              (eq (e--instruction-type parent) of-type))
          (cons parent (e--ancestral-instructions parent of-type))
        (e--ancestral-instructions parent of-type))
    nil))

(defun e--context (&optional query directive)
  "Get context plist.

Returns plist with :summary and :references keys."
  (let* ((pred
          (lambda (instr)
            (e--reference-matches-query-p instr
                                          (or query
                                              (when directive
                                                (overlay-get directive
                                                             'e-directive-prefix-tag-query))))))
         (used-commentary-refs (make-hash-table))
         (toplevel-refs (e--foreach-instruction instr
                         when (and (e--referencep instr)
                                  (eq (e--topmost-instruction instr 'reference pred)
                                      instr))
                         collect instr))
         (linked-refs (let ((visited-refs (make-hash-table))
                           (independent-refs ())
                           (child-refmap
                            (let ((ht (make-hash-table)))
                              (cl-loop for tlr in toplevel-refs
                                      do (cl-loop for instr in (e--wholly-contained-instructions
                                                              (overlay-buffer tlr)
                                                              (overlay-start tlr)
                                                              (overlay-end tlr))
                                                when (and (not (eq instr tlr))
                                                         (e--referencep instr))
                                                do (puthash instr t ht)))
                              ht)))
                       (cl-labels ((collect-linked-references-recursively (ref)
                                   (puthash ref t visited-refs)
                                   (dolist (linked-id (e--instruction-outlinks ref))
                                     (let ((linked-ref (e--instruction-with-id linked-id)))
                                       (when (and linked-ref
                                                (e--referencep linked-ref)
                                                (not (gethash linked-ref visited-refs)))
                                         (unless (gethash linked-ref child-refmap)
                                           (push linked-ref independent-refs))
                                         (collect-linked-references-recursively linked-ref))))))
                         (mapc #'collect-linked-references-recursively
                               (cl-remove-duplicates
                                (append (when directive
                                        (e--ancestral-instructions directive 'reference))
                                      toplevel-refs
                                      (flatten-tree
                                       (mapcar (lambda (instr)
                                               (e--ancestral-instructions instr 'reference))
                                             toplevel-refs)))))
                         independent-refs)))
         (total-refs (cl-remove-if (lambda (ref)
                                    (and directive (e--subinstruction-of-p ref directive)))
                                  (cl-union toplevel-refs linked-refs)))
         (reference-alist (cl-loop for reference in total-refs with alist = ()
                                  do (push reference (alist-get (overlay-buffer reference) alist))
                                  finally (progn
                                           (cl-loop for (_ . references) in alist
                                                   do (sort references
                                                          (lambda (x y)
                                                            (< (overlay-start x)
                                                               (overlay-start y)))))
                                           (cl-return alist)))))
    (with-temp-buffer
      (insert (format "## Reference%s%s"
                     (if (> (length total-refs) 1) "s" "")
                     (if (and directive
                             (e--topmost-instruction directive 'reference pred))
                         " & Directive" "")))
      (cl-loop for (buffer . references) in reference-alist
               do (insert
                   (concat
                    "\n\n"
                    (format "### %s"
                            (if-let ((dir-file (when directive
                                                 (buffer-file-name (overlay-buffer directive)))))
                                (if-let ((buf-file (buffer-file-name buffer)))
                                    (format "File `%s`"
                                            (file-relative-name
                                             buf-file
                                             (file-name-parent-directory dir-file)))
                                  (format "Buffer `%s`" (buffer-name buffer)))
                              (format "Buffer `%s`" (buffer-name buffer))))))
               do (dolist (ref references)
                    (cl-destructuring-bind (ref-info-string ref-string)
                        (e--overlay-region-info ref)
                      (let ((markdown-delimiter
                             (e--delimiting-markdown-backticks ref-string)))
                        (insert
                         (concat
                          "\n\n"
                          (format "#### Reference #%d" (e--instruction-id ref))
                          "\n\n"
                          (format "In %s:" ref-info-string)
                          "\n\n"
                          (format "%s\n%s\n%s"
                                  markdown-delimiter
                                  ref-string
                                  markdown-delimiter)
                          (let ((commentary (e--commentary-text ref)))
                            (unless (string-empty-p commentary)
                              (puthash ref t used-commentary-refs)
                              (format "\n\nCommentary:\n\n%s"
                                      (e--markdown-enquote commentary))))))))))
      (list :summary (if reference-alist (buffer-string) "")
            :references reference-alist))))

(defun e--directive-llm-prompt (directive)
  "Craft the prompt for the LLM model associated with the DIRECTIVE."
  (when (e--directive-empty-p directive)
    (error "Directive %s is empty" directive))
  (let* ((is-programmer (derived-mode-p 'prog-mode))
         (context (e--context nil directive))
         (reference-count (length (flatten-tree (mapcar #'cdr (plist-get context :references)))))
         (directive-toplevel-reference (e--topmost-instruction directive 'reference))
         (directive-buffer (overlay-buffer directive))
         (directive-filename (buffer-file-name directive-buffer)))
    (cl-destructuring-bind (directive-region-info-string directive-region-string)
        (e--overlay-region-info directive)
      (let ((expanded-directive-text
             (let ((secondary-directives
                    (cl-remove-if-not (lambda (inst)
                                       (and (eq (e--instruction-type inst) 'directive)
                                            (not (eq inst directive))))
                                     (e--wholly-contained-instructions
                                      (overlay-buffer directive)
                                      (overlay-start directive)
                                      (overlay-end directive))))
                   (sd-typename (if (not (eq (overlay-get directive 'e-directive-status)
                                           'succeeded))
                                   "hint"
                                 "correction")))
               (concat
                (format "%s" directive-region-info-string)
                (if (string-empty-p directive-region-string)
                    "."
                  (let ((markdown-delimiter
                         (e--delimiting-markdown-backticks directive-region-string)))
                    (concat
                     (format ", which correspond%s to:"
                             (if (e--multiline-string-p directive-region-string) "" "s"))
                     "\n\n"
                     (format "%s\n%s\n%s"
                             markdown-delimiter
                             directive-region-string
                             markdown-delimiter))))
                "\n\n"
                (if (not (string-empty-p (e--directive-text directive)))
                    (format "My directive to you is:\n\n%s"
                            (e--markdown-enquote (overlay-get directive 'e-directive)))
                  (format "My directive to you is composed entirely out of %ss, so you should \
treat them as subdirectives, instead."
                          sd-typename))
                (cl-loop for sd in secondary-directives
                         when (not (string-empty-p (e--directive-text sd)))
                         concat (concat
                                "\n\n"
                                (cl-destructuring-bind (sd-region-info sd-region)
                                    (e--overlay-region-info sd)
                                  (concat
                                   (format "For %s"
                                           sd-region-info)
                                   (let ((sd-text (e--markdown-enquote
                                                  (overlay-get sd 'e-directive))))
                                     (if (e--bodyless-instruction-p sd)
                                         (format ", you have a %s:\n\n%s"
                                                 sd-typename
                                                 sd-text)
                                       (let ((markdown-delimiter
                                              (e--delimiting-markdown-backticks
                                               sd-region)))
                                         (concat
                                          (format ", which correspond%s to:\n\n%s"
                                                  (if (e--multiline-string-p sd-region)
                                                      "" "s")
                                                  (format "%s\n%s\n%s"
                                                          markdown-delimiter
                                                          sd-region
                                                          markdown-delimiter))
                                          (format "\n\nYou have the %s:\n\n%s"
                                                  sd-typename
                                                  sd-text)))))))))))))
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
            (unless (zerop reference-count)
              (format " Use the reference%s to complete my directive."
                      (if (> reference-count 1) "s" "")))
            (unless (zerop reference-count)
              (concat "\n\n"
                      (plist-get context :summary)))
            "\n\n## Directive\n\n"
            (format "For %s, %s"
                    (if directive-filename
                       (format "file `%s`"
                               (file-name-nondirectory directive-filename))
                      (format "buffer `%s`" (buffer-name directive-buffer)))
                    expanded-directive-text)
            (when directive-toplevel-reference
              (concat "\n\n"
                      "Recall that the directive is embedded within "
                      (format "reference #%d in %s."
                              (e--instruction-id directive-toplevel-reference)
                              directive-region-info-string)))
            "\n\n"
            "What you return must be enclosed within a Markdown block. I will \
then parse the content of your Markdown block, and then format and inject the result where it \
needs to go by myself."
            "\n\n"
            "If you cannot complete the directive or something is unclear to \
you, be it due to missing information or due to the directive asking something outside your \
abilities, do not guess or proceed. Instead, reply with a question or clarification that does not \
contain Markdown code blocks. I will treat a response without Markdown code blocks as invalidated, \
and will format its contents a failure reason. Be strict, and announce failure even at the \
slightest discrepancy."
            "\n\n"
            (if (e--bodyless-instruction-p directive)
                "Note that I will inject your response in the position the \
directive is embedded in, so be mindful not to return anything superfluous that surrounds the \
above region."
              (concat
               "Note that I deleted the original text region"
               (format " (%s), " directive-region-info-string)
               "so I expect you to return a replacement."))))
          (buffer-substring-no-properties (point-min) (point-max)))))))

(defun e--ancestral-commentators (instruction)
  "Return list of references which contain INSTRUCTION that have commentary.

The list is sorted with the topmost references first."
  (with-current-buffer (overlay-buffer instruction)
    (let* ((start (overlay-start instruction))
           (end (overlay-end instruction))
           (instructions (e--instructions-in start end 'reference))
           (filtered (cl-remove-if-not (lambda (instr)
                                         (and (not (string-empty-p (e--commentary-text instr)))
                                              (e--subinstruction-of-p instruction instr)))
                                       instructions))
           (sorted (sort filtered (lambda (a b) (e--subinstruction-of-p b a)))))
      sorted)))

(defun e--create-id ()
  (let ((id
         (if e--retired-ids
             (prog1
                 (car e--retired-ids)
               (setq e--retired-ids (cdr e--retired-ids)))
           (cl-incf e--id-counter))))
    (puthash id t e--id-usage-map )
    id))

(defun e--retire-id (id)
  (when (gethash id e--id-usage-map)
    (remhash id e--id-usage-map)
    (push id e--retired-ids)))

(defun e--reset-id-counter ()
  "Reset all custom variables to their default values."
  (setq e--id-counter 0)
  (setq e--id-usage-map (make-hash-table))
  (setq e--retired-ids ()))

(defun e--instruction-outlinks (instruction)
  "Return the :to links of INSTRUCTION."
  (plist-get (overlay-get instruction 'e-links) :to))

(defun e--instruction-inlinks (instruction)
  "Return the :from links of INSTRUCTION."
  (plist-get (overlay-get instruction 'e-links) :from))

(provide 'evedel-instructions)

;; Local Variables:
;; read-symbol-shorthands: (("e-"  . "evedel-"))
;; End:

;;; evedel-instructions.el ends here.