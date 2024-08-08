;;; coderel.el --- Instructed LLM programmer

;; Copyright (C) 2024  daedsidog

;; Author: daedsidog <contact@daedsidog.com>
;; Version: 0.0.1
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (transient "0.4.0") (compat "29.1.4.1"))
;; URL: https://github.com/daedsidog/coderel

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; coderel is a gptel extension Emacs package specialized for ergonomic programming with an LLM.  It
;; aims to eliminate manual aspects of software development by relegating them to an LLM.  The
;; programmer acts as an overseer over their LLM by creating various instruction bookmarks across
;; their project.

;;; Code:

;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom cel-reference-color "yellow"
  "Color to be used as a tint for reference overlays."
  :type 'string
  :group 'coderel)

(defcustom cel-directive-color "orange"
  "Color to be used as a tint for directive overlays."
  :type 'string
  :group 'coderel)

(defcustom cel-result-color "cyan"
  "Color to be used as a tint for result overlays."
  :type 'string
  :group 'coderel)

(defun cel--tint (source-color-name tint-color-name)
  "Return the color resulting from tinting SOURCE-COLOR-NAME with TINT-COLOR-NAME."
  (let ((tint (color-name-to-rgb tint-color-name))
        (color (color-name-to-rgb source-color-name)))
    (cl-mapcar (lambda (color tint)
                 (+ (* 0.9 color) (* 0.1 tint)))
               color
               tint)))

(defun cel-create-instruction-overlay (type start end name)
  "Create an instruction overlay of TYPE from START to END with the given NAME."
  (let ((overlay (if (eq type 'buffer)
                     (make-overlay (point-min) (point-max) (current-buffer))
                   (cel--make-overlay-in-region (current-buffer) start end))))
    (overlay-put overlay 'name name)
    ;; TODO: create bookmark here
    overlay))

(defun cel-create-region-reference (start end name)
  "Create a region reference from START to END with the given NAME."
  (interactive "r\nsName: ")
  (cel-create-instruction-overlay 'region start end name))

(defun cel-create-buffer-reference (name)
  "Create a buffer reference for the current buffer with the given NAME."
  (interactive "sName: ")
  (cel-create-instruction-overlay 'buffer (point-min) (point-max) name))

(defun cel-create-reference (name)
  "Create a reference label with the given NAME."
  (interactive "sReference name: ")
  (let ((label (if (use-region-p)
                   (cel-create-region-reference (region-beginning) (region-end) name)
                 (cel-create-buffer-reference name))))
    label))

(defun cel--make-overlay-in-region (buffer start end)
  "Create an overlay in BUFFER from START to END."
  (with-current-buffer buffer
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'before-string "REFERENCE")
      (overlay-put overlay 'face 'highlight))))
  
(provide 'coderel)
;;; coderel.el ends here.

;; Local Variables:
;; read-symbol-shorthands: (("cel-" . "coderel-"));
;; End:
