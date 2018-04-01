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

(require 'abaplib)
;;==============================================================================
;; Retrieve Program Source From Server
;; TODO Consider ETag
;; 1. Get Local Source Information (ETag)
;; 2. Retrieve with ETag in Header
;; 3. If Got 304 - Not Modified ==> Open Local Source File
;; 4. If Got 200 => Overwrite On Current Implementation
;; 4.1 => Overwrite Metadata File
;; TODO 4.2.1 => If Local File Exist => Compare (EDiff) and Prompt to User
;; 4.2.2 => No Local File => Directly Write to Local
;;==============================================================================
(defun abap-program-retrieve(programe-name)
  ;; Retrieve metadata
  (abaplib-service-call
   'retrieve
   '((type . prog)
     (name . programe-name))
   ;; (abaplib-service-get-uri 'get-program-metadata prog-name)
   (lambda (&rest data)
     (let ((prog-metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.xml" abaplib--project-config-dir prog-name)))
       (write-region prog-metadata nil file)
       ))
   :parser 'abaplib-util-sourcecode-parser
   )
  ;; Retrieve source
  (abaplib-service-call
   (abaplib-service-get-uri 'get-program-source prog-name)
   (lambda (&rest data)
     (let ((prog-source (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.abap" abaplib--project-dir prog-name)))
       (unless (string= prog-source "")
         (write-region prog-source nil file)
         nil
         )))
   :parser 'abaplib-util-sourcecode-parser
   ;; :headers (list '("If-None-Match" . "201704241108050011")
   ;;                '("Content-Type" . "plain/text"))
   ))

(defun abap-program-check-syntax (prog-name source &optional version )
  "Check ABAP program syntax based on local unsubmitted source"
  (let* ((version (or version "active"))
         (adtcore-uri (concat "/sap/bc/adt/programs/programs/" prog-name))
         (chkrun-uri  (concat adtcore-uri "/source/main"))
         (chkrun-content (base64-encode-string source))
         (post-data (abaplib-core-check-syntax-template
                     adtcore-uri
                     chkrun-uri version chkrun-content)))
    ;; before post
    ;; (message post-data)
    (abaplib-service-call
     (abaplib-service-get-uri 'checkrun)
     (lambda (&rest rest)
       (let* ((check-report (xml-get-children (cl-getf rest :data) 'checkReport))
              (message-list (xml-get-children (car check-report) 'checkMessageList))
              (messages (xml-get-children (car message-list) 'checkMessage)))
         (abaplib-core-check-show-message messages)
         ))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post-data
     :headers `(("Content-Type" . "application/vnd.sap.adt.checkobjects+xml"))
     )
    ))

(defun abap-program-submit()
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
(provide 'abaplib-programs)
;;; abaplib_programs.el ends here
