;;; abap.el --- ABAP programs            -*- lexical-binding: t; -*-

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

(require 'abaplib-core)
;; (require 'abaplib-program)


;; (defvar-local abap--abap-object nil
;; "ABAP development Object
;; alist with name and type")
;;==============================================================================
;; Project
;;==============================================================================

(defun abap-create-project ()
  "Create new ABAP project"
  (interactive)
  (let* ((project-name (or (read-string "Project name (without blank): " )
                           (error "project name can't be empty")))
         (parent-dir (read-directory-name "Workspace directory:" abap-workspace-dir))
         (project (expand-file-name project-name parent-dir)))
    (unless (file-directory-p parent-dir)
      (make-directory parent-dir))
    (abaplib-create-project project)
    (message "Project %s created and added to workspace." project)))

(defun abap-add-project ()
  "Add ABAP project into workspace"
  (interactive)
  (let* ((current-dir (abaplib-util-current-dir))
         (project(expand-file-name
                  (read-directory-name "Init project: "
                               (abaplib-project-init-propose current-dir)))))
    (abaplib-create-project project)
    (message "Project %s added to workspace." project)))

(defun abap-remove-project ()
  "Remove ABAP project from workspace.
  `Note:' this operation will not physically delete the project files."
  (interactive)
  (let ((project (completing-read "Select Project: "
                                  (abaplib-get-project-list))))
    (abaplib-remove-project project)
    (message "Project %s removed from workspace." project)))

(defun abap-switch-project ()
  "Switch ABAP project"
  (interactive)
  (let ((project (completing-read "Select Project: " (abaplib-get-project-list))))
    (abaplib-switch-project project)
    (dired project)))

(defun abap-get-current-project ()
  "Get current project, prompt user choose project if none"
  (unless abaplib--current-project
    (call-interactively 'abap-switch-project))
  abaplib--current-project)

(defun abap-add-server ()
  "Add server to current project"
  (interactive)
  (let ((project (abap-get-current-project))
        (server (read-string "Server https url: ")))
    (abaplib-add-server-to-project project server)
    (message "Severl url %s added to current project" server)))

(defun abap-login ()
  "Login to server"
  (interactive)
  (let* ((project (abap-get-current-project))
         (username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "SAP Client: "  ))
         (login-token (format "Basic %s" (base64-encode-string
                                          (concat username ":" password)))))
    (message "Connecting...")
    (abaplib-auth-login-with-token project login-token client)))

(defun abap-search-object ()
  "Retrieve ABAP objects"
  (interactive)
  (let* ((project (abap-get-current-project))
         (query-string (read-string "Enter Search String: "))
         (search-result (abaplib-core-do-search query-string))
         ;; (completing-list (mapcar (lambda(object-node)
         ;;                            (let ((name (xml-get-attribute object-node 'name))
         ;;                                  (type (xml-get-attribute object-node 'type))
         ;;                                  (description (xml-get-attribute object-node
         ;;                                                                  'description)))
         ;;                              (format "%-8s %-30s %s" type name description)))
         ;;                          search-result))
         ;; (selected-item (split-string (completing-read "Maching Items: " completing-list)
         ;; " " t))
         ;; (item-type (car selected-item))
         ;; (item-name (nth 1 selecetd-item))
         (completing-list)
         (index 1))
    (dolist (object-node search-result)
      (let ((name (xml-get-attribute object-node 'name))
            (description (xml-get-attribute object-node 'description)))
        (setq completing-list
              (append  completing-list
                       `((,(format "%-3s %-30s %s" index name description)))))
        (setq index (+ index 1))))
    (let* ((selected-item (completing-read "Maching Items: " completing-list))
           (selected-index (string-to-number (car (split-string selected-item " " t))))
           (selected-object (nth (- selected-index 1) search-result))
           (object-node (xml-get-children selected-object 'objectReference)))
      (abaplib-core-retrieve-object (xml-get-attribute selected-object 'name)
                                    (xml-get-attribute selected-object 'type)
                                    (xml-get-attribute selected-object 'uri)))))

(defun abap-retrieve-source (&optional abap-object)
  "Retrieve source"
  (interactive)
  (let ((source-name (file-name-nondirectory (buffer-file-name)))
        (object-name (abaplib-core-get-property 'name))
        (object-type (abaplib-core-get-property 'type))
        (object-uri  (abaplib-core-get-property 'uri)))
    ;; (abaplib-core-service-dispatch 'retrieve abap-object)
    (abaplib-core-retrieve-object object-name
                                  object-type
                                  object-uri
                                  source-name)))

(defun abap-check-source (&optional abap-object)
  "Check source"
  (interactive)
  (let ((abap-object (or abap-object
                         (abap-get-abap-object-from-file))))
    (abaplib-core-service-dispatch 'check abap-object)))


(defun abap-submit-source (&optional abap-object)
  "Submit source"
  (interactive)
  (let ((abap-object (or abap-object
                         (abap-get-abap-object-from-file))))
    (abaplib-core-service-dispatch 'submit abap-object)))

(defun abap-activate-source (&optional abap-object)
  "Activate source"
  (interactive)
  (let ((abap-object (or abap-object
                         (abap-get-abap-object-from-file))))
    (abaplib-core-service-dispatch 'activate abap-object)))

(provide 'abap)
;;; abap-in-emacs.el ends here
