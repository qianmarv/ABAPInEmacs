;;; abaplib-util.el --- utilities                    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: abbrev

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

(defun abaplib-util-current-dir ()
  (if buffer-file-name
      (file-name-directory buffer-file-name
    dired-directory))

(defun abaplib-util-get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun abaplib-util-sourcecode-parser()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string))
  )

(defun abaplib-util-goto-position (line column)
  (goto-char (point-min))
  (forward-line (- line 1))
  (move-to-column column))

(defun abaplib-util-xml-parser()
  (libxml-parse-xml-region (point-min) (point-max)))

;; TODO move to core
(defun abaplib-util-log-buf-write(log)
  (save-current-buffer
    (set-buffer (get-buffer-create abaplib--log-buffer))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert "\n\n")
    (insert (concat "Log at: "
                    (format-time-string "%Y-%m-%dT%T")
                    ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
                     (format-time-string "%z"))))
    ;; (insert "\n")
    (insert (format "%s" log))
    (setq buffer-read-only t)
    ))

(defun abaplib-util-log-buf-pop()
  (pop-to-buffer (get-buffer-create abaplib--log-buffer)))

(defun abaplib-util-jsonize-to-file(list file)
  (write-region (with-temp-buffer
                  (insert (json-encode list))
                  (json-pretty-print (point-min) (point-max))
                  (message (buffer-string))
                  (buffer-string)) nil file))

(defun abaplib-util-upsert-alists (alists pair)
  "Append/Update alist with pair"
  (let* ((key (car pair))
         (existp (assoc-string key alists)))
    (if existp
        (setcdr (assoc-string key alists) (cdr pair))
      (setq alists (append alists (list pair)))))
  alists)

(provide 'abaplib-util)
;;; abaplib-util.el ends here
