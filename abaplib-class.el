;;; abaplib-class.el --- ABAP Class -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: ABAP source, class

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

(defvar-local abaplib-class--name nil
  "ABAP class name")

(defvar-local abaplib-class--properties-cache nil
  "ABAP class properties")

(defconst abaplib-class--uri-prefix "/sap/bc/adt/oo/classes")

(defvar abaplib-class--uri nil)
;; (defvar abaplib-class--source-uri nil)

(defconst abaplib-class--folder "Classes")

(defconst abaplib-class--source-file-suffix ".clas.abap")



(defun abaplib-class--get-root-directory ()
  "Get directory for source code of classes"
  (let* ((source-dir (expand-file-name abaplib-core--folder-S (abaplib-get-project-path)))
         (classes-dir  (expand-file-name abaplib-class--folder source-dir)))
    (unless (file-directory-p source-dir)
      (make-directory source-dir))
    (unless (file-directory-p classes-dir)
      (make-directory classes-dir))
    classes-dir))

(defun abaplib-class--get-directory(&optional class-name)
  "Get directory for a specfic class"
  (let* ((class-name (or class-name abaplib-class--name))
         (class-dir (expand-file-name class-name (abaplib-class--get-root-directory))) )
    (unless (file-directory-p class-dir)
      (make-directory class-dir))
    class-dir))

;; FIXME 1. Add programe name to metdata file
;;       2. verify program name while retrieve from cache
(defun abaplib-class--get-properties (&optional class-name)
  " Get program properties"
  (let* ((class-name (or class-name
                         abaplib-class--name
                         (error "Class is unknown!")))
         (class-dir (abaplib-class--get-directory class-name)))
    (unless abaplib-class--properties-cache
      (let ((prop-file (expand-file-name ".properties.json" class-dir)))
        (when (file-exists-p prop-file)
          (setq abaplib-class--properties-cache (json-read-file prop-file)))))
    abaplib-class--properties-cache))

(defun abaplib-class--get-property (key include &optional class-name)
  " Get program property by key"
  (let ((class-name (or class-name
                        abaplib-class--name
                        (error "Class is unknown!")))
        (properties (abaplib-class--get-properties class-name)))
    (when include
      (setq properties (alist-get include
                                  (alist-get 'includes properties))))
    (alist-get key properties)))


(defun abaplib-class--set-properties(properties &optional class-name)
  " Get metadata from cache"
  (let* ((class-name (or class-name
                         abaplib-class--name
                         (error "Class is unknown!")))
         (class-dir (abaplib-class--get-directory class-name))
         (prop-file (expand-file-name ".properties.json" class-dir)))
    (abaplib-util-jsonize-to-file properties prop-file)
    (setq abaplib-class--properties-cache properties)))

;; (defun abaplib-class--set-property (key value)
;;   "Set property"
;;   (abaplib-class--set-properties
;;    (abaplib-util-upsert-alists (abaplib-class--get-properties) (cons key value))))

(defun abaplib-class--parse-metadata (xml-node)
  ;; (message "Parse metadata.")
  (let* ((adtcore-type (xml-get-attribute xml-node 'type))
         (type-list (split-string adtcore-type "/"))
         (type (car type-list))
         (subtype (nth 1 type-list))
         (name (xml-get-attribute xml-node 'name))
         (description (xml-get-attribute xml-node 'description))
         (version (xml-get-attribute xml-node 'version))
         (package-node (car (xml-get-children xml-node 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (includes-node (xml-get-children xml-node 'include))
         (includes (mapcar
                    (lambda (include)
                      (let* ((version (xml-get-attribute include 'version))
                             (source-uri (xml-get-attribute include 'sourceUri))
                             (include-type (xml-get-attribute include 'includeType))
                             (type (xml-get-attribute include 'type))
                             (links (xml-get-children include 'link))
                             (file-name (concat include-type ".clas.abap"))
                             (etag))
                        (dolist (link links)
                          (when (string= (xml-get-attribute link 'type) "text/plain")
                            (setq etag (xml-get-attribute link 'etag))
                            (return)))
                        (cons file-name `((version . ,version)
                                          (source-uri . ,source-uri)
                                          (include-type . ,include-type)
                                          (type . ,type)
                                          (etag . ,etag)))))
                    includes-node)))
    `((name . ,name)
      (description . ,description)
      (type . ,type)
      (subtype . ,subtype)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(defun abaplib-class--retrieve-properties ()
  "Retrieve class metadata from server"
  (let* ((etag (abaplib-class--get-property 'metadata-etag nil))
         (class-name abaplib-class--name)
         (url (abaplib-get-project-api-url abaplib-class--uri))
         (data (abaplib--rest-api-call url
                                       nil
                                       :parser 'abaplib-util-xml-parser)))
    (abaplib-class--set-properties (abaplib-class--parse-metadata data))))

;; (defun abaplib-class--buffer-get-create (class-name)
;;   (get-buffer-create (format "*(Server) %s *" class-name)))

(defun abaplib-class--retrieve-source (source-name source-uri &optional etag)
  "Retrieve program source from server"
  (message "Etag: = %s" etag)
  (let* ((class-name abaplib-class--name)
         (url (abaplib-get-project-api-url (concat abaplib-class--uri
                                                   "/"
                                                   source-uri))))
    (abaplib--rest-api-call
     url
     (lambda (&rest rest)
       (let ((response-data (cl-getf rest :data))
             (status-code (request-response-status-code (cl-getf rest :response)))
             (source-file (expand-file-name (concat source-name
                                                    abaplib-class--source-file-suffix)
                                            (abaplib-class--get-directory class-name))))
         (if (eq status-code 304)
             (message "Source remain unchanged in server.")
           (write-region response-data nil source-file)
           (message "Source retrieved from server and overwrite local."))))
     :parser 'abaplib-util-sourcecode-parser
     :headers (list `("If-None-Match" . ,etag)
                    '("Content-Type" . "plain/text")))))

(defun abaplib-class-do-retrieve(abap-object)
  "Retrieve source code"
  (abaplib-class--init abap-object)
  (let* ((pre-includes (alist-get 'includes (abaplib-class--get-properties)))
         (curr-properties (abaplib-class--retrieve-properties))
         (includes (alist-get 'includes curr-properties)))
    ;; Retrieve latest properties
    (mapc (lambda (include)
            (let* ((include-type (car include))
                   (pre-include (alist-get (intern include-type) pre-includes))
                   (source-uri (alist-get 'source-uri include))
                   (pre-etag (alist-get 'etag pre-include)))
              (abaplib-class--retrieve-source include-type
                                              source-uri
                                              pre-etag)
              t))
          includes)))


(defun abaplib-class-do-check(abap-object)
  "Check syntax for program source
  TODO check whether source changed since last retrieved from server
       Not necessary to send the source code to server if no change."
  (abaplib-class--init abap-object)
  (let ((version (abaplib-class--get-property 'version))
        (adtcore-uri abaplib-class--uri)
        (chkrun-uri  abaplib-class--source-uri)
        (chkrun-content (base64-encode-string (buffer-substring-no-properties
                                               (point-min)
                                               (point-max)))))
    (abaplib-core-check-post version adtcore-uri chkrun-uri chkrun-content)))

;; (defun abaplib-class-do-submit(abap-object)
;;   "Submit source to server

;;    TODO Check source in server side if current source was changed based on an old version
;;    The submission should be cancelled"
;;   (abaplib-class--init abap-object)
;;   (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
;;          (csrf-token (abaplib-core-get-csrf-token))
;;          (lock-handle (abaplib-core-lock abaplib-class--uri csrf-token)))
;;     (abaplib--rest-api-call
;;      (abaplib-get-project-api-url abaplib-class--source-uri)
;;      (lambda (&rest rest)
;;        (let* ((response (cl-getf rest :response))
;;               (ETag (request-response-header response "ETag")))
;;          (abaplib-class--retrieve-properties)
;;          (message "program submit to server success.")))
;;      :type "PUT"
;;      :data source
;;      :headers `(("Content-Type" . "text/plain")
;;                 ("x-csrf-token" . ,csrf-token))
;;      :params `(("lockHandle" . ,lock-handle))
;;      )))

;; (defun abaplib-class-do-activate(abap-object)
;;   "Activate source in server"
;;   (abaplib-class--init abap-object)
;;   (let* ((adtcore-name abaplib-class--name)
;;          (adtcore-uri abaplib-class--uri))
;;     (abaplib-core-activate-post adtcore-name adtcore-uri)))


(defun abaplib-class--init (abap-object)
  ;; (message "called abaplib-class--init with: %s" abap-object)
  (message "abap-object: %s" abap-object)
  (let* ((class-name (alist-get 'name abap-object))
         (type (alist-get 'type abap-object))
         (sub-type (or (alist-get 'subtype abap-object)
                       (abaplib-class--get-property 'subtype class-name)))
         (uri-prefix abaplib-class--uri-prefix))
    (setq abaplib-class--name class-name)
    (setq abaplib-class--uri (concat uri-prefix "/" class-name ))))


(provide 'abaplib-class)
;;; abaplib-class.el ends here
