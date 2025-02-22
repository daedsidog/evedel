;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'gptel-context)
(require 'gptel-transient)

(require 'evedel-utilities)
(require 'evedel-instructions)

(defvar e--embed-context-to-gptel-request t)
(defvar e--gptel-query nil)

(defun e--gptel-status (response info)
  (let ((status (plist-get info :status)))
    (if (eq response 'abort)
        'aborted
      status)))

(defun e--gptel-process-directive (directive callback)
  "Send directives to model via gptel."
  (let ((cb (lambda (response info)
              (funcall callback response directive (e--gptel-status response info))))
        (prompt (e--directive-llm-prompt directive))
        (system-message (e--directive-llm-system-message directive)))
    (let ((e--embed-context-to-gptel-request nil))
      (gptel-request prompt
        :system system-message
        :dry-run nil
        :stream nil
        :in-place nil
        :callback cb))))

(cl-defmethod e--process-directive (directive callback)
  (e--gptel-process-directive directive callback))

(cl-defmethod e--llm-client-name ()
  "gptel")

(advice-add 'gptel-request :around
            (lambda (orig-fun prompt &rest args)
              (if e--embed-context-to-gptel-request
                  ;; Define a dummy contexts alist so that gptel won't complain down the line.
                  (let ((gptel-context--alist
                         `((,(current-buffer) . (,(make-overlay (point-min) (point-max)))))))
                    (apply orig-fun prompt args))
                (apply orig-fun prompt args))))

(advice-add 'gptel-context--string :around
            (lambda (orig-fun prompt &rest args)
              (if e--embed-context-to-gptel-request
                  (plist-get (e--context (e--tag-query-prefix-from-infix
                                          (read (format "(%s)" e--gptel-query))))
                             :summary)
                (apply orig-fun prompt args))))

(defun gptel--describe-infix-context ()
  "Evedel")

(transient-define-suffix gptel--infix-context-add-region ()
  :key "--r"
  :if (lambda () nil))
(transient-define-suffix gptel--infix-context-remove-all ()
  :key "--d"
  :if (lambda () nil))

(transient-define-suffix gptel--infix-use-context ()
  :if (lambda () nil)
  :key "--i")

(transient-define-infix gptel--infix-context-add-file ()
  :description "Include references"
  :class 'gptel-lisp-variable
  :variable 'gptel-use-context
  :format "%k %d %v"
  :set-value #'gptel--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (system . "with system message")
                 (user   . "with user prompt"))
  :key "-i"
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("No"                  . nil)
                              ("with system message" . system)
                              ("with user prompt"    . user)))
                   (destination (completing-read prompt choices nil t)))
              (cdr (assoc destination choices)))))

(transient-define-infix gptel--infix-context-add-buffer ()
  :class 'gptel-lisp-variable
  :display-nil ""
  :variable 'e--gptel-query
  :set-value #'gptel--set-with-scope
  :argument ""
  :prompt "Reference tag query: "
  :reader (lambda (prompt initial history)
            (e--read-tag-query e--gptel-query))
  :format "%k %d %v"
  :key "-q"
  :description "Tag search query")

(provide 'evedel-gptel)

;; Local Variables:
;; read-symbol-shorthands: (("e-"  . "evedel-"))
;; End:

;;; evedel-gptel.el ends here.