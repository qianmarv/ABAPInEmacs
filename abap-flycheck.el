;;; abap-flycheck.el --- flycheck setting for ABAP Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'abap)

(defun flycheck-abap--parse-error (alist checker)
  (let-alist alist
    (flycheck-error-new-at .line
                           .column
                           (intern .level)
                           .message)))

(defun flycheck-abap--start (checker callback)
  (condition-case err
      (let ((errors (delq nil
                          (mapcar
                           (lambda (alist)
                             (message "%s" alist)
                             (flycheck-abap--parse-error alist
                                                         checker))
                           (abap-check-source t)))))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err)))))

(defun flycheck-abap--verify (_checker)
  (list
   (flycheck-verification-result-new
    :label "ABAP Mode"
    :message (if (and abap-mode (abaplib-is-logged))
                 "enabled"
               "disabled")
    :face (if abap-mode 'success '(bold warning)))))



(defun flycheck-abap-setup()
  "setup Flycheck abap"
  (interactive)
  (flycheck-define-generic-checker 'abap
    "A syntax checker for ABAP using abap-mode"
    :start #'flycheck-abap--start
    :verify #'flycheck-abap--verify
    :modes 'abap-mode
    ;; :error-filter flycheck-abap-error-filter
    :predicate #'(lambda()
                   abap-mode))
  (add-to-list 'flycheck-checkers 'abap)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(provide 'abap-flycheck)
;;; abap-flycheck.el ends here
