;;; sap-abap-lib.el --- sap abap server lib          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Marvin Qian

;; Author: Marvin Qian <qianmarv@gmail.com>
;; Keywords: SAP, ABAP, CDS

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

;;; Dev Log
;;; TODO Read Project List & Select Project & Read Server Information & Read User/Password

(require 'request)


(defcustom abap-workspace-dir
  "~/ws/ABAP"
  "ABAP Workspace Directory"
  :type 'string
  :group 'abap-mode)

;; (defcustom abap-use-cookie
;;   t
;;   "Use cookie"
;;   :type 'boolean
;;   :group 'abap-mode)

(defcustom abap-query-list-max-result
  "51"
  "Object Query List Maximum Result"
  :type 'string
  :group 'abap-mode)

;; (defvar abaplib--token nil
;;   "ABAP token used for authentication.")

(defvar abaplib--auth-data nil
  "System Login State")


(defvar abaplib--auth-client nil
  "ABAP system client used for login.")

;; (defvar abaplib-system-ID nil
;;   "ABAP system client used for login.")
(defvar abaplib--service-url nil
  "The address of the abap server host")

(defvar abaplib--project-name nil
  "Current ABAP Project")

(defvar abaplib--project-dir nil
  "ABAP Project Directory")

(defvar abaplib--project-config-dir nil
  "ABAP Project Configuration Directory")

(defvar-local abaplib--object-props nil
  "Local Variable: ABAP Object Properties")

(defun abaplib-current-directory ()
  (file-name-directory buffer-file-name))

;; (defun abaplib-retrieve-project-info (&optional working_dir)
;;   (let* ((working_dir (or working_dir (abaplib-current-directory)))
;;          )
;;     ))

(defun abaplib-setup-project ()
  "Setup ABAP Project"
  (interactive
   (let* ((system_id (read-string "System ID: "))
          (server_address (read-string "Server Address: "))
          (http_port (read-string "ICM HTTP Port: ")))
     (unless (file-directory-p abap-workspace-dir)
       (make-directory abap-workspace-dir))
     (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir system_id))
     (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
     (setq abaplib--project-name system_id)
     ;; (setq abaplib-system-ID system_id)
     (if (file-directory-p abaplib--project-dir)
         (error "Project %s already exist!" system_id)
       (let ((abap-config-file (format "%s/server.ini" abaplib--project-config-dir)))
         (make-directory abaplib--project-dir)
         (make-directory abaplib--project-config-dir)
         (write-region
          (concat
           (format "Server=%s\n" server_address)
           (format "HttpPort=%s\n" http_port))
          nil
          abap-config-file)
         (setq abaplib--service-url (format "http://%s:%s/" server_address http_port))
         (message "Project Initialized Successfully!")
         (abaplib-goto-project abaplib--project-name)
         ))
     )))

(defun abaplib-get-service-uri (service &optional object_name)
  (cond
   ((eq service 'search-object)
    (format "/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=%s&maxResults=%s" object_name abap-query-list-max-result))
   ((eq service 'get-program-metadata)
    (format "/sap/bc/adt/programs/programs/%s" object_name))
   ((eq service 'get-program-source)
    (format "/sap/bc/adt/programs/programs/%s/source/main" object_name))
   ((eq service 'checkrun)
    (format "/sap/bc/adt/checkruns?reporters=abapCheckRun"))
   ))

(defun abaplib-get-project-list ()
  (let ((files (directory-files abap-workspace-dir)))
    (seq-remove ;; Support Emacs 25.1 Or Above Only
     (lambda (file)
       (cond ((string= file "."))
             ((string= file ".."))
             ((not (file-directory-p (expand-file-name file abap-workspace-dir)))))
       )
     files))
  )

(defun abaplib-auth-set-login-token (projectp token)
  (let ((login_token (list (cons 'login-token (list token)))))
    (setcdr (assq 'ER9 abaplib--auth-data)
            (append login_token (alist-get 'ER9 abaplib--auth-data))
            )))

(defun abaplib-auth-get-login-token (projectp)
  (alist-get 'login-token (alist-get projectp abaplib--auth-data)))

(defun abaplib-auth-set-csrf-token (projectp token)
  (let ((csrf_token (list (cons 'csrf-token (list token)))))
    (setcdr (assq 'ER9 abaplib--auth-data)
            (append csrf_token (alist-get 'ER9 abaplib--auth-data))
            )))

(defun abaplib-auth-get-csrf-token (projectp)
  (alist-get 'csrf-token (alist-get projectp abaplib--auth-data)))



(defun abaplib-auth-login ()
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT"
  (interactive
   (let ((username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "Client: "  ))
         (projectp (intern abaplib--project-name)))

     (let* ((login_token (cons "Authorization"
                               (format "Basic %s"
                                       (base64-encode-string (concat username ":" password)))))
            (login_uri "/sap/bc/adt/core/discovery")
            (response (request
                       (concat  abaplib--service-url
                                (replace-regexp-in-string "^/*" "" login_uri))
                       :sync t
                       :headers (list login_token (cons "x-csrf-token" "fetch"))
                       :params (list (cons "sap-client" client))
                       ))
            (login_status (request-response-symbol-status response))
            (csrf_token (cons "x-csrf-token" (request-response-header response "x-csrf-token"))))

       (if (not (eq login_status 'success))
           (error "Login Failed!")
         "Init project auth data"
         (if (alist-get projectp abaplib--auth-data)
             ;; Remove previous login data
             (setcdr (assq projectp abaplib--auth-data)
                     (cons projectp t))
           (add-to-list 'abaplib--auth-data (cons projectp t))
           )

         (abaplib-auth-set-login-token projectp login_token)
         (abaplib-auth-set-csrf-token projectp csrf_token)

         (setq abaplib--auth-client client)
         (message "Login successfully!"))
       nil
       ))))

(defun abaplib-ensure-login ()
  ;; Ensure in a project
  (unless abaplib--project-name
    (call-interactively 'abaplib-select-project))
  (let* ((projectp (intern (or abaplib--project-name
                               (call-interactively 'abaplib-select-project))))
         (is_logged (alist-get projectp abaplib--auth-data)))
    (unless is_logged ;; Try to login for the first time
      (call-interactively 'abaplib-auth-login)))
  )

(defun abaplib-select-project ()
  "Select Existing Project"
  (interactive)
  (let ((project_name (completing-read "Select Project: " (abaplib-get-project-list))))
    (abaplib-goto-project project_name)
    )
  )

(defun abaplib-goto-project (project_name)
  "Go to project root directory"
  (unless (string= project_name abaplib--project-name)
    (setq abaplib--token nil)
    (let* ((abap-config-dir (expand-file-name (format "%s/.abap" project_name) abap-workspace-dir))
           (abap-config-file (expand-file-name "server.ini" abap-config-dir)))
      ;; Read server information
      (with-temp-buffer
        (insert-file-contents abap-config-file)
        (let ((server_address)
              (http_port))
          (mapcar
           (lambda (item)
             (let* ((config (split-string item "=" t))
                    (config_id (car config))
                    (config_value (car (cdr config))))
               (cond ((string= config_id "Server") (setq server_address config_value))
                     ((string= config_id "HttpPort") (setq http_port config_value)))
               ))
           (split-string (buffer-string) "\n" t))
          (unless (and server_address http_port)
            (error "Not a valid project!"))
          (cond
           ((string= http_port "80") (setq abaplib--service-url (format "http://%s/" server_address)))
           ((string= http_port "443") (setq abaplib--service-url (format "https://%s/" server_address)))
           (t (setq abaplib--service-url (format "http://%s:%s/" server_address http_port))))
          (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir project_name))
          (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
          (setq abaplib--project-name project_name)
          ))
      )
    )
  (helm-open-dired abaplib--project-dir)
  )

(defun abaplib-service-call (api success &rest args)
  "Invoke corresponding service API."
  (abaplib-ensure-login)
  ;; (unless abaplib--project-name
  ;;   (call-interactively 'abaplib-select-project))
  ;; (unless abaplib--token ;; Try to login for the first time
  ;;   (call-interactively 'abaplib-auth-login))

  ;; (unless (cl-getf args :type))
  (let* ((projectp (intern abaplib--project-name))
         (login_token (abaplib-auth-get-login-token projectp))
         (csrf_token (abaplib-auth-get-csrf-token projectp))
         (headers (cl-getf args :headers))
         (type    (cl-getf args :type)))
    ;; (message (format "header: %s" headers))
    (if (and type
             (not (string= type "GET")))
        (setq headers (append headers login_token csrf_token)))
    ;; Delete :headers from args
    (append (request-response-data
             (apply #'request (if (string-match "^http[s]*://" api) api
                                (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
                                        (replace-regexp-in-string "^/*" "" api)))
                    :sync (not success)
                    :headers headers
                    :status-code '((304 . (lambda (&rest _) (message "Source Not Modified")))
                                   (401 . (lambda (&rest _) (error "Not Authorized"))))
                    :params `((sap-client . ,abaplib--auth-client))
                    :success success
                    :error  (lambda (&key error-thrown &allow-other-keys &rest _)
                              (message "Got error: %S" error-thrown))
                    ;; :complete (lambda (&rest _) (message "Complete" ))
                    args))
            nil))
  )

;; (append (request-response-data
;;          (apply #'request (if (string-match "^http[s]*://" api) api
;;                             (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
;;                                     (replace-regexp-in-string "^/*" "" api)))
;;                 :sync nil
;;                 :headers `(,abaplib--token ("Content-Type" . "application/json"))
;;                 :params `((sap-client . ,abaplib--auth-client))
;;                 :parser 'buffer-string
;;                 :complete (lambda (&rest_) (message "Call \"%s\" complete" api))
;;                 args))
;;         nil)
;; )

;; 0. Query List
;; 1. Pull 
;; 2. Check Syntax
;; 3. Save to Server
;; 3.1 Lock
;; 4. Activate At Server

;; Supported Object Type
;;;; DCLS => Access Control
;;;; DDLS => Data Definition

;; ------------------------------------------Process----------------------------------------

;; Query Object List From Server
;;;; GET /sap/bc/adt/repository/informationsystem/objecttypes?maxItemCount=999&name=*&data=usedByProvider
;;;; GET /sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=I_CNSLDTNINTCORECNCL%2A&maxResults=51

;; Start to Edit: Send Lock Post
;;;; POST /sap/bc/adt/programs/programs/zmq_abap_test01?_action=LOCK&accessMode=MODIFY
;;;; Get LOCK_HANDLE

;; Check Syntax In Server
;;;; POST /sap/bc/adt/checkruns?reporters=abapCheckRun
;;;; Before Submit to Server, Need base64 encode source and wrap into posting xml
;;;; After Saved to Server, 
;;;; 1. Active Version
;;;; 2. Unsaved Version
;;;; 3. Saved in server version

;; Save Source In Server
;;;; PUT /sap/bc/adt/programs/programs/zmq_abap_test01/source/main?lockHandle=tdOnBGpU37goVbesZZSK6aK%2FG%2FA%3D HTTP/1.1

;; Activate Source
;; 1) Unlock
;;;; POST /sap/bc/adt/programs/programs/zmq_abap_test01?_action=UNLOCK&lockHandle=tdOnBGpU37goVbesZZSK6aK%2FG%2FA%3D HTTP/1.1
;; 2) Activate
;;;; POST /sap/bc/adt/activation?method=activate&preauditRequested=true HTTP/1.1

(defun abaplib-sourcecode-parser()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string))
  )
(defun abaplib-xml-parser()
  (libxml-parse-xml-region (point-min) (point-max)))



(defun abaplib-request-object-list (object-name)
  (xml-get-children
   (abaplib-service-call
    (abaplib-get-service-uri 'search-object (format "%s%%2A" object-name))
    nil
    :parser 'abaplib-xml-parser)
   'objectReference))

(defun abaplib-convert-object-plain-list (object-list)
  (mapcar (lambda (obj)
            (let* ((attrs       (xml-node-attributes obj))
                   (type        (cdr (assq 'type        attrs)))
                   (name        (cdr (assq 'name        attrs)))
                   (packageName (cdr (assq 'packageName attrs)))
                   (description (cdr (assq 'description attrs)))
                   )
              (list (format "%-8s%-31s%s" type name description))))
          object-list))

(defun abaplib-pull-abap-object ()
  (interactive
   (let* ((object-name (read-string "Enter Search String: "))
          (object-list (abaplib-request-object-list object-name))
          (object-plain-list (abaplib-convert-object-plain-list object-list))
          (selected-object (split-string
                            (completing-read "Maching Items: " object-plain-list)
                            " "
                            t))
          (object-type (car selected-object))
          (object-name (car (cdr selected-object))))

     (cond ((string= object-type "PROG/P" ) (abaplib-prog-get-source object-name))
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

(defun abaplib-prog-get-source (prog_name)
  ;; Retrieve metadata
  (abaplib-service-call
   (abaplib-get-service-uri 'get-program-metadata prog_name)
   (lambda (&rest data)
     (let ((prog_metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.xml" abaplib--project-config-dir prog_name)))
       (write-region prog_metadata nil file)
       ))
   :parser 'abaplib-sourcecode-parser
   )
  ;; Retrieve source
  (abaplib-service-call
   (abaplib-get-service-uri 'get-program-source prog_name)
   (lambda (&rest data)
     (let ((prog_source (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.abap" abaplib--project-dir prog_name)))
       (unless (string= prog_source "")
         (write-region prog_source nil file)
         nil
         )))
   :parser 'abaplib-sourcecode-parser
   ;; :headers (list '("If-None-Match" . "201704241108050011")
   ;;                '("Content-Type" . "plain/text"))
   ))

(defun abaplib-retrieve-object-properties()
  (let* ((file_name (file-name-nondirectory (buffer-file-name)))
         (components  (split-string file_name "\\." t))
         (object_name (car components))          ; Object Name
         (sub_type (car (cdr components)))       ; Sub Type  , prog/clas/ddls
         (source_type (car (last components)))   ; Major Type, abap/cds
         (property_file (format "%s%s.%s.xml"
                                abaplib--project-config-dir
                                object_name
                                sub_type)))
    (setq abaplib--object-props (with-temp-buffer
                                  (insert-file-contents property_file)
                                  (let* ((xml_root (libxml-parse-xml-region (point-min) (point-max)))
                                         (properties (xml-node-attributes xml_root)))
                                    properties
                                    )))
    ))

(defun abaplib-get-object-property(property-name)
  (unless abaplib--object-props
    (abaplib-retrieve-object-properties))
  (cdr (assq property-name abaplib--object-props ))
  )

(defun abaplib-get-object-name ()
  (abaplib-get-object-property 'name))

(defun abaplib-get-object-version()
  (abaplib-get-object-property 'version))

(defun abaplib-get-object-type()
  (abaplib-get-object-property 'type))
;; (let ((core_type (abaplib-get-object-property 'type)))
;;   (cond ((string= core_type "PROG/P") "prog")
;;         (t nil))))


(defun abaplib-check-syntax ()
  " Check syntax for source code in current buffer"
  (interactive)
  (let ((object_name (abaplib-get-object-name))
        (object_type (abaplib-get-object-type))
        (object_version (abaplib-get-object-version)))
    (cond ((string= object_type "PROG/P") (abaplib-prog-check-source-syntax
                                           object_name
                                           (buffer-string)
                                           object_version
                                           ))
          (t nil))
    )
  )

(defun abaplib-prog-check-source-syntax (prog_name source &optional version )
  "Check ABAP program syntax based on local unsubmitted source"
  (let* ((version (or version "active"))
         (adtcore_uri (concat "/sap/bc/adt/programs/programs/" prog_name))
         (chkrun_uri  (concat adtcore_uri "/source/main"))
         (chkrun_content (base64-encode-string source))
         (post_data (abaplib-template-check-object
                     adtcore_uri
                     chkrun_uri version chkrun_content)))
    (abaplib-service-call
     (abaplib-get-service-uri 'checkrun)
     (lambda (&rest data)
       (let ((check_report (xml-get-children (cl-getf data :data)))
             (file (format "%s/%s.check.el" abaplib--project-dir prog_name)))
         (unless (string= prog_source "")
           (write-region prog_source nil file)
           nil
           )))
     :parser 'abaplib-xml-parser
     :type "POST"
     :data post_data
     )
    ))

(defun abaplib-template-check-object (adtcore_uri chkrun_uri version &optional chkrun_content)
  "Return xml of checkObjects"
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<chkrun:checkObjectList xmlns:adtcore=\"http://www.sap.com/adt/core\" xmlns:chkrun=\"http://www.sap.com/adt/checkrun\">"
   (format "<chkrun:checkObject adtcore:uri=\"%s\" chkrun:version=\"%s\">"
           adtcore_uri
           version)
   (if chkrun_content
       (concat
        "<chkrun:artifacts>"
        (format "<chkrun:artifact chkrun:contentType=\"text/plain; charset=utf-8\" chkrun:uri=\"%s\">"
                chkrun_uri)
        (format "<chkrun:content>%s</chkrun:content>"
                chkrun_content)
        "</chkrun:artifact>"
        "</chkrun:artifacts>" )
     ""
     )
   "</chkrun:checkObject>"
   "</chkrun:checkObjectList>"
   ))

(provide 'abaplib)
;;; abaplib.el ends here
