;;; evedel.el --- Instructed LLM programmer/assistant -*- lexical-binding: t; -*-

;; Copyright (C) 2024 daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.4.20
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0"))
;; URL: https://github.com/daedsidog/evedel

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Evedel is a gptel extension aimed at managing and processing in-code
;; instructions.  It provides functionality to define, manipulate, and process
;; directives and references within a buffer.  Key features include creating,
;; deleting, and updating instructional overlays, as well as processing
;; responses from LLMs for directives.

;;; Code:

(defgroup evedel nil
  "Customization group for Evedel."
  :group 'tools)

;;;###autoload
(defun e-version (&optional here message)
  "Return the current version of Evedel.

Interactively, or when MESSAGE is non-nil, show it in echo area.  With prefix
argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version "v0.4.20"))
    (cond
     ((or message (called-interactively-p 'any)) (message "Evedel %s" version))
     (here (insert (format "Evedel %s" version)))
     (t version))))

(require 'evedel-gptel)
(require 'evedel-instructions)
(require 'evedel-restorer)

(provide 'evedel)

;; Local Variables:
;; read-symbol-shorthands: (("e-"  . "evedel-"))
;; End:

;;; evedel.el ends here.