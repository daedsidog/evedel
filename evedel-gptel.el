;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)

(require 'evedel-instructions)

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
    (gptel-request prompt
      :system system-message
      :dry-run nil
      :stream nil
      :in-place nil
      :callback cb)))

(cl-defmethod e--process-directive (directive callback)
  (e--gptel-process-directive directive callback))

(cl-defmethod e--llm-client-name ()
  "gptel")

(provide 'evedel-gptel)

;; Local Variables:
;; read-symbol-shorthands: (("e-"  . "evedel-"))
;; End:

;;; evedel-gptel.el ends here.