;;; elec-pair-extra.el --- Provide extra rules to manipulate elec-pair behaviors -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
;; Homepage: https://github.com/liuyinz/elec-pair-extra

;; This file is not a part of GNU Emacs.

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

;; This file is not a part of GNU Emacs.

;;; Commentary:

;; Provide extra rules to manipulate elec-pair behaviors.

;;; Code:

(require 'cl-lib)
(require 'elec-pair)

;; add rules for major-mode
;; SEE https://emacs-china.org/t/html-electric-pair-mode-js/13904/6
(defcustom elec-pair-extra-rules nil
  "A alist of major-mode and related rule.
Each element is in the form of

\(MODE :pair    ( CHAR/(CHAR . PAIR-CHAR) ...)
       :inhibit ( CHAR/(CHAR . REGEXP/FUNCTION) ...)).

MODE: A major mode.
CHAR: A character to match the input.  for example: ?\{
\(CHAR . PAIR-CHAR): A pair of paired character.  for example: (?\{ . ?\})
\(CHAR . REGEXP): A character to match input, a regex pattern for inhibit
                  predicate by `looking-back'.  for example: (?\{ . \":{\")
\(CHAR . FUNCTION): A character to match input, a function accept the input
                    as parameter for inhibit predict.  for example:
                    (?\{ . (lambda (_c) (eq ?: (char-before (1- (point))))))"
  :group 'electricity
  :type '(alist :key-type symbol
                :value-type
                (plist :key-type symbol
                       :options (:pair :inhibit)
                       :value-type
                       (repeat (choice character
                                       (cons character
                                             (choice
                                              character
                                              string
                                              function)))))))

(defun elec-pair-extra-get (prop &optional mode)
  "Return value of PROP for MODE in `elec-pair-extra-rules'.
If optional arg MODE is nil,use `major-mode' instead."
  (plist-get (cdr (assq (or mode major-mode) elec-pair-extra-rules)) prop))

(defun elec-pair-extra-add-pair (pair)
  "Add pair auto completion for PAIR."
  (when pair
    (mapc (lambda (x) (modify-syntax-entry (car x) (cdr x)))
          (let ((left (or (and (characterp pair) pair) (car pair)))
                (right (and (consp pair) (cdr pair))))
            (if (or (characterp pair) (equal left right))
                (list (cons left "\""))
              (list (cons left (concat "(" (char-to-string right)))
                    (cons right (concat ")" (char-to-string left)))))))))

(defun elec-pair-extra-inhibit (char)
  "Inhibit predicate function for `elec-pair-extra'.
Return non-nil if CHAR should be inhibited."
  (or (cl-member char (elec-pair-extra-get :inhibit)
                 :test
                 (lambda (char it)
                   (cond
                    ((characterp it) (equal char it))
                    ((and (consp it) (equal char (car it)))
                     (cond ((stringp   (cdr it)) (looking-back (cdr it) 1))
                           ((functionp (cdr it)) (funcall (cdr it) char)))))))
      (funcall (or (and-let* ((func (get major-mode 'elec-pair-extra-orig))
                              ((functionp func)))
                     func)
                   #'electric-pair-default-inhibit)
               char)))

(defun elec-pair-extra-setup-mode ()
  "Setup elec-pair-extra features for current major-mode."
  (unless (get major-mode 'elec-pair-extra-added)
    (mapc #'elec-pair-extra-add-pair (elec-pair-extra-get :pair))
    (put major-mode 'elec-pair-extra-added t))
  (put major-mode 'elec-pair-extra-orig electric-pair-inhibit-predicate)
  (setq-local electric-pair-inhibit-predicate #'elec-pair-extra-inhibit))

;;;###autoload
(defun elec-pair-extra-setup ()
  "Add hooks for major modes defined in `elec-pair-extra-rules'."
  (dolist (mode (mapcar #'car elec-pair-extra-rules))
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              #'elec-pair-extra-setup-mode)))

(provide 'elec-pair-extra)
;;; elec-pair-extra.el ends here
