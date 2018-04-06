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

(defvar-local abaplib-program--subtype nil
  "ABAP program subtype")

(defconst abaplib-program--programs-uri-prefix "/sap/bc/adt/programs/programs")
(defconst abaplib-program--includes-uri-prefix "/sap/bc/adt/programs/includes")

(defvar abaplib-program--metadata-uri nil)
(defvar abaplib-program--source-uri nil)


(defun abaplib-program--get-properties ()
  " Get program properties"
  (unless abaplib-program--name
    (error "Program is nil"))
  (unless abaplib-program--properties-cache
    (let ((prop-file (expand-file-name (concat abaplib-program--name ".prog.json")
                                       (abaplib-get-project-cache-dir))))
      (when (file-exists-p prop-file)
        (setq abaplib-program--properties-cache (json-read-file prop-file)))))
  abaplib-program--properties-cache)

(defun abaplib-program--get-property (key)
  " Get program property by key"
  (alist-get key (abaplib-program--get-properties)))

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

(defun abaplib-program-submit()
  (let ((prog-name   (abaplib-object-get-name))
        (prog-source (buffer-substring-no-properties (point-min) (point-max)))
        (lock-handle (abaplib-core-lock)))
    (abaplib-service-call
     (abaplib-service-get-uri 'save-program-source prog-name)
     (lambda (&rest rest)
       (let* ((response (cl-getf rest :response))
              (ETag (request-response-header response "ETag")))
         (message (format "Succeed with ETAG:%s" ETag))
         (abaplib-core-unlock lock-handle)))
     :type "PUT"
     :data prog-source
     :headers `(("Content-Type" . "text/plain"))
     :params `(("lockHandle" . ,lock-handle))
     )))

(defun abaplib-program--retrieve-properties (&optional etag)
  "Retrieve program metadata from server"
  ;; TODO Problebly not necessary to match etag as ADT always retrieve metadata
  ;;      Not sure whether etag will be verified in server side.
  (let* ((program-name abaplib-program--name)
         (url (abaplib-get-project-api-url (url (abaplib-get-project-api-url abaplib-program--source-uri)))))
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
           (message "Program metadata retrieved."))))
     :parser 'abaplib-util-xml-parser
     :headers (list `("If-None-Match" . ,etag)))))

(defun abaplib-program--retrieve-source (&optional etag)
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
           (write-region response-data nil (abaplib-program--get-source-file program-name))
           (message "Program source retrieved from server and overwrite local."))))
     :parser 'abaplib-util-sourcecode-parser
     :headers (list `("If-None-Match" . ,etag)
                    '("Content-Type" . "plain/text")))))

(defun abaplib-program-do-retrieve(dev-object)
  "Retrieve source code"
  (abaplib-program--init dev-object)

  (let ((source-etag (abaplib-program--get-property 'etag))
        (metadata-etag (abaplib-program--get-property 'metadata-etag)))
    (abaplib-program--retrieve-properties metadata-etag)
    (abaplib-program--retrieve-source source-etag)))

(defun abaplib-program-do-check(dev-object)
  "Check syntax for program source "
  (abaplib-program--init dev-object)
  (let* ((version ((abaplib-program--get-property 'etag)))
         (adtcore-uri abaplib-program--metadata-uri)
         (chkrun-uri  abaplib-program--source-uri)
         (chkrun-content (base64-encode-string (buffer-substring-no-properties
                                                (point-min)
                                                (point-max)))))
    (abaplib-core-check-post version adtcore-uri chkrun-uri chkrun-content)))

(defun abaplib-program-do-submit()
  (interactive)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (lock-handle (abaplib-core-lock)))
    (abap--rest-call
     (abaplib-service-get-uri 'save-program-source prog-name)
     (lambda (&rest rest)
       (let* ((response (cl-getf rest :response))
              (ETag (request-response-header response "ETag")))
         (message (format "Succeed with ETAG:%s" ETag))
         (abaplib-core-unlock lock-handle)))
     :type "PUT"
     :data prog-source
     :headers `(("Content-Type" . "text/plain"))
     :params `(("lockHandle" . ,lock-handle))
     )))

(defun abaplib-program--init (dev-object)
  (let ((program-name (or (alist-get 'name dev-object)
                          abaplib-program--name))
        (subtype (car (last (split-string (alist-get 'type dev-object) "/"))))
        (resource-uri (case (intern abaplib-program--subtype)
                        ('P abaplib-program--resource-programs)
                        ('I abaplib-program--resource-includes))))
    (setq abaplib-program--name program-name)
    (setq abaplib-program--subtype subtype)
    (setq abaplib-program--metadata-uri (concat abaplib-program--programs-uri-prefix
                                                "/"
                                                program-name))
    (setq abaplib-program--source-uri (concat abaplib-program--programs-uri-prefix
                                              "/"
                                              program-name
                                              "/source/main"))))


(provide 'abaplib-program)
;;; abaplib_programs.el ends here
