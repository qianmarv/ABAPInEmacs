;;; abaplib_programs.el --- ABAP programs            -*- lexical-binding: t; -*-

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
;==============================================================================

(defvar-local abaplib-program--name nil
  "ABAP program name")

(defvar-local abaplib-program--properties-cache nil
  "ABAP program properties")

;; (defvar-local abaplib-program--subtype nil
;;   "ABAP program subtype")

(defconst abaplib-program--programs-uri-prefix "/sap/bc/adt/programs/programs")
(defconst abaplib-program--includes-uri-prefix "/sap/bc/adt/programs/includes")

(defvar abaplib-program--uri nil)
(defvar abaplib-program--source-uri nil)


;; FIXME 1. Add programe name to metdata file
;;       2. verify program name while retrieve from cache
(defun abaplib-program--get-properties (&optional program-name)
  " Get program properties"
  (let ((program-name (or program-name
                          abaplib-program--name)))
    (unless program-name
      (error "Program is nil"))
    (unless abaplib-program--properties-cache
      (let ((prop-file (expand-file-name (concat program-name ".prog.json")
                                         (abaplib-get-project-cache-dir))))
        (when (file-exists-p prop-file)
          (setq abaplib-program--properties-cache (json-read-file prop-file)))))
    abaplib-program--properties-cache))

(defun abaplib-program--get-property (key &optional program-name)
  " Get program property by key"
  (alist-get key (abaplib-program--get-properties program-name)))

(defun abaplib-program--set-properties(properties)
  " Get metadata from cache"
  (let ((prop-file (expand-file-name (concat abaplib-program--name ".prog.json")
                                     (abaplib-get-project-cache-dir))))
    (setq abaplib-program--properties-cache properties)
    (abaplib-util-jsonize-to-file (abaplib-program--get-properties) prop-file)))

(defun abaplib-program--set-property (key value)
  "Set property"
  (abaplib-program--set-properties
   (abaplib-util-upsert-alists (abaplib-program--get-properties) (cons key value))))

(defun abaplib-program--parse-metadata (xml-node)
  (let* ((program-props)
         (type (xml-get-attribute xml-node 'type))
         (version (xml-get-attribute xml-node 'version))
         (sourceUri (xml-get-attribute xml-node 'sourceUri))
         (etag))
    (let ((links (xml-get-children xml-node 'link)))
      (dolist (link links)
        (when (string= (xml-get-attribute link 'type) "text/plain")
          (setq etag (xml-get-attribute link 'etag))
          (return))))
    (setq program-props (append (list `(type . ,type)
                                      `(version . ,version)
                                      `(source-uri . ,sourceUri)
                                      `(etag . ,etag))))))

(defun abaplib-program--get-directory ()
  "Get program source directory"
  (let* ((major-type-dir (expand-file-name "Source Code"
                                           (abaplib-project-get-property 'path)))
         (minor-type-dir (expand-file-name "Programs"
                                           major-type-dir)))
    (unless (file-exists-p major-type-dir)
      (make-directory major-type-dir))
    (unless (file-exists-p minor-type-dir)
      (make-directory minor-type-dir))
    minor-type-dir))

(defun abaplib-program--get-source-file (programe-name)
  (expand-file-name (concat programe-name ".prog.abap")
                    (abaplib-program--get-directory)))


(defun abaplib-program--retrieve-properties (&optional etag)
  "Retrieve program metadata from server"
  ;; TODO Problebly not necessary to match etag as ADT always retrieve metadata
  ;;      Not sure whether etag will be verified in server side.
  (let* ((program-name abaplib-program--name)
         (url (abaplib-get-project-api-url abaplib-program--uri)))
    (abaplib--rest-api-call
     url
     (lambda (&rest rest)
       (let* ((response-data (cl-getf rest :data))
              (response (cl-getf rest :response))
              (status-code (request-response-status-code response))
              (metadata-etag (request-response-header response "ETag"))
              (properties))
         (unless (eq status-code 304) ;; Not modified
           (abaplib-program--set-properties (append
                                             (abaplib-program--parse-metadata response-data)
                                             (list `(metadata-etag . ,metadata-etag))))

           (abaplib-program--set-properties properties)
           (message "Program metadata refreshed."))))
     :parser 'abaplib-util-xml-parser
     :headers (list `("If-None-Match" . ,etag)))))

(defun abaplib-program--buffer-get-create (program-name)
  (get-buffer-create (format "*(Server) %s *" program-name)))

(defun abaplib-program--retrieve-source (etag &optional target-file)
  "Retrieve program source from server"
  (let* ((program-name abaplib-program--name)
         (url (abaplib-get-project-api-url abaplib-program--source-uri)))
    (abaplib--rest-api-call
     url
     (lambda (&rest rest)
       (let ((response-data (cl-getf rest :data))
             (status-code (request-response-status-code (cl-getf rest :response))))
         (if (eq status-code 304)
             (message "Program source remain unchanged in server.")
           (if target-file
               (write-region response-data nil (abaplib-program--get-source-file program-name))
             (let ((buffer (abaplib-program--buffer-get-create program-name)))
               (set-buffer buffer)
               (erase-buffer)
               (goto-char (point-min))
               (insert response-data)
               (switch-to-buffer buffer)))
           (message "Program source retrieved from server and overwrite local."))))
     :parser 'abaplib-util-sourcecode-parser
     :headers (list `("If-None-Match" . ,etag)
                    '("Content-Type" . "plain/text")))))

(defun abaplib-program-do-retrieve(abap-object)
  "Retrieve source code"
  (abaplib-program--init abap-object)

  (let ((source-etag (abaplib-program--get-property 'etag))
        (metadata-etag (abaplib-program--get-property 'metadata-etag))
        (source-file (abaplib-program--get-source-file abaplib-program--name)))
    (abaplib-program--retrieve-properties metadata-etag)
    (abaplib-program--retrieve-source source-etag source-file)))

(defun abaplib-program-do-check(abap-object)
  "Check syntax for program source
  TODO check whether source changed since last retrieved from server
       Not necessary to send the source code to server if no change."
  (abaplib-program--init abap-object)
  (let ((version (abaplib-program--get-property 'version))
        (adtcore-uri abaplib-program--uri)
        (chkrun-uri  abaplib-program--source-uri)
        (chkrun-content (base64-encode-string (buffer-substring-no-properties
                                               (point-min)
                                               (point-max)))))
    (abaplib-core-check-post version adtcore-uri chkrun-uri chkrun-content)))

(defun abaplib-program-do-submit(abap-object)
  "Submit source to server

   TODO Check source in server side if current source was changed based on an old version
   The submission should be cancelled"
  (abaplib-program--init abap-object)
  (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
         (csrf-token (abaplib-core-get-csrf-token))
         (lock-handle (abaplib-core-lock-sync abaplib-program--uri csrf-token)))
    (abaplib--rest-api-call
     (abaplib-get-project-api-url abaplib-program--source-uri)
     (lambda (&rest rest)
       (let* ((response (cl-getf rest :response))
              (ETag (request-response-header response "ETag")))
         (abaplib-program--retrieve-properties)
         (message "program submit to server success.")))
     :type "PUT"
     :data source
     :headers `(("Content-Type" . "text/plain")
                ("x-csrf-token" . ,csrf-token))
     :params `(("lockHandle" . ,lock-handle))
     )))

(defun abaplib-program-do-activate(abap-object)
  "Activate source in server"
  (abaplib-program--init abap-object)
  (let* ((adtcore-name abaplib-program--name)
         (adtcore-uri abaplib-program--uri))
    (abaplib-core-activate-post adtcore-name adtcore-uri)))


(defun abaplib-program--init (abap-object)
  ;; (message "called abaplib-program--init with: %s" abap-object)
  (let* ((program-name (alist-get 'name abap-object))
         (properties   (abaplib-program--get-properties program-name))
         (type (alist-get 'type properties))
         (sub-type (car (reverse (split-string type "/"))))
         (uri-prefix (case (intern sub-type)
                       ('P abaplib-program--programs-uri-prefix)
                       ('I abaplib-program--includes-uri-prefix))))

    (setq abaplib-program--name program-name)
    (setq abaplib-program--uri (concat uri-prefix "/" program-name))
    (setq abaplib-program--source-uri (concat uri-prefix "/" program-name "/source/main"))))


(provide 'abaplib-program)
;;; abaplib_programs.el ends here
