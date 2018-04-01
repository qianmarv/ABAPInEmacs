;;; abap-in-emacs.el --- ABAP Development in Emacs   -*- lexical-binding: t; -*-

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

(require 'abap-mode)
(require 'abaplib)
;; (require 'abap-program)

;; (defun abap-development ()
;;   (interactive)
;;   )

;;==============================================================================
;; Project
;;==============================================================================

(defun abap-create-project ()
  "Create new ABAP project"
  (interactive)
  (let* ((project-name (or (read-string "Project name (without blank): " )
                           (error "project name can't be empty")))
         (parent-dir (read-string "Workspace directory:" abap-workspace-dir))
         (project-dir (expand-file-name project-name parent-dir)))
    (unless (file-directory-p parent-dir)
      (make-directory parent-dir))
    (abaplib-create-project project-dir)))

(defun abap-init-project ()
  "Init ABAP project"
  (interactive)
  (let* ((current-dir (abaplib-util-current-dir))
         (project-dir (expand-file-name
                       (read-string "Init project: "
                                    (abaplib-project-init-propose current-dir)))))
    (abaplib-create-project project-dir)))

(defun abap-switch-project ()
  (interactive)
  (let ((project-dir (completing-read "Select Project: " (abaplib-get-project-list))))
    (abaplib-switch-project project-dir)))

(defun abap-get-current-project ()
  "Get current project, prompt user choose project if none"
  (unless abaplib--current-project
    (call-interactively 'abap-switch-project))
  abaplib--current-project)

(defun abap-add-server ()
  "Add server to current project"
  (interactive)
  (let ((server (read-string "Server https url: "))
        (project (abap-get-current-project)))
    (abaplib-add-server-to-project project server)))

(defun abap-retrieve-object ()
  "Retrieve ABAP objects"
  (interactive
   (let* ((object-name (read-string "Enter Search String: "))
          (object-list (abaplib-service-call 'search object-name))
          (selected-object (split-string
                            (completing-read "Maching Items: " object-list)
                            " "
                            t))
          (object-type (car selected-object))
          (object-name (car (cdr selected-object))))

     (cond ((string= object-type "PROG/P" ) (abaplib-core-pull-prog object-name))
           ((string= object-type "DDLS/DL") (message "TODO: Handle Retrieve CDS - Data Definition"))
           ((string= object-type "DDLS/DF") (message "TODO: Handle Retrieve CDS - Entity"))
           ((string= object-type "DCLS/DL") (message "TODO: Handle Retrieve CDS - Access Control"))
           ((string= object-type "CLAS/OC") (message "TODO: Handle Retrieve Class")))
     nil
     )
   ;; Retrieve Local Attribute File and Get ETag
   ;; Compose request call
   ;; Request to Server
   ;; Write File & Open File
   ))


(provide 'abap)
;;; abap-in-emacs.el ends here
