((nil . ((eval . (progn
                   (require 'evedel)
                   (let ((instrs-file ".evedel"))
                     (when (and (file-exists-p instrs-file)
                                (zerop (evedel-instruction-count)))
                       (evedel-load-instructions instrs-file))
                     (add-hook 'after-save-hook
                               (lambda ()
                                 (when (file-exists-p instrs-file)
                                   (evedel-save-instructions instrs-file))))))))))
