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
;; TODO combine below project/server related variables into a prop variable
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


(defconst abaplib--log-buffer "*ABAP Log*"
  "ABAP log buffer")
;;==============================================================================
;; Utilities
;;==============================================================================
(defun abaplib-util-current-dir ()
  (file-name-directory buffer-file-name))

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

;; (defun abaplib-util-log-buf-init ()
;;   (save-current-buffer
;;     (set-buffer (get-buffer-create abaplib--log-buffer))
;;     (setq buffer-read-only t)
;;     ))

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
;;==============================================================================
;; Project
;;==============================================================================
(defun abaplib-project-setup ()
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
         (abaplib-project-switch-context abaplib--project-name)
         ))
     )))


(defun abaplib-project-get-list ()
  (let ((files (directory-files abap-workspace-dir)))
    (seq-remove ;; Support Emacs 25.1 Or Above Only
     (lambda (file)
       (cond ((string= file "."))
             ((string= file ".."))
             ((not (file-directory-p (expand-file-name file abap-workspace-dir)))))
       )
     files))
  )

(defun abaplib-project-select(&optional silent?)
  "Select Existing Project"
  (interactive)
  (let ((project_name (completing-read "Select Project: " (abaplib-project-get-list))))
    (abaplib-project-switch-context project_name)
    (unless silent?
      (helm-open-dired abaplib--project-dir))
    )
  )

(defun abaplib-project-switch-context (project_name)
  "Go to project root directory"
  (unless (string= project_name abaplib--project-name)
    ;; (setq abaplib--token nil)
    (let* ((abap-config-dir (expand-file-name (format "%s/.abap" project_name) abap-workspace-dir))
           (abap-config-file (expand-file-name "server.ini" abap-config-dir)))
      ;; Create buffer
      ;; (abaplib-util-log-buf-init)

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
           ((string= http_port "44300") (setq abaplib--service-url (format "https://%s:%s/" server_address http_port)))
           ((string= http_port "443") (setq abaplib--service-url (format "https://%s/" server_address)))
           (t (setq abaplib--service-url (format "http://%s:%s/" server_address http_port))))
          (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir project_name))
          (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
          (setq abaplib--project-name project_name)
          ))
      )
    )
  )
(defun abaplib-project-ensure-inside-project()
  "Ensure in a project"
  (let* ((proj_dir (abaplib-util-current-dir))
         (folders (split-string proj_dir "/" t))
         (proj_name (car (last folders)))
         (val_dir (expand-file-name (concat abap-workspace-dir "/" proj_name "/"))))
    (if (string= proj_dir val_dir)
        (abaplib-project-switch-context proj_name)
      (error "Not in a valid project"))
    )
  ;; Still not, open interactively
  ;; (unless abaplib--project-name
  ;;   (abaplib-project-switch t))
  )
;; (defun abaplib-project-auto-detect (&optional file)
;;   (let ((file (or file
;;                   buffer-file-name)))
;;     ))


;;==============================================================================
;; Core Services
;;==============================================================================

(defun abaplib-service-get-uri (service &optional object_name)
  (cond
   ((eq service 'search-object)
    (format "/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=%s&maxResults=%s" object_name abap-query-list-max-result))
   ((eq service 'get-program-metadata)
    (format "/sap/bc/adt/programs/programs/%s" object_name))
   ((eq service 'get-program-source)
    (format "/sap/bc/adt/programs/programs/%s/source/main" object_name))
   ((eq service 'save-program-source)
    (format "/sap/bc/adt/programs/programs/%s/source/main" object_name))
   ((eq service 'checkrun)
    (format "/sap/bc/adt/checkruns?reporters=abapCheckRun"))
   ))

(defun abaplib-service-call (api success &rest args)
  "Invoke corresponding service API."

  (abaplib-auth-ensure-login)

  (let* ((projectp (intern abaplib--project-name))
         (login_token (abaplib-auth-get-login-token projectp))
         (csrf_token (abaplib-auth-get-csrf-token projectp))
         (headers (cl-getf args :headers))
         (type    (cl-getf args :type)))

    ;; For method like POST, PUT, DELETE, etc Required to Validate CSRF Token
    (unless (or (not type)
                (string= type "GET"))
      (setq headers (append headers csrf_token login_token)))
    ;; (let* ((response (request
    ;;                  (concat  abaplib--service-url
    ;;                           (replace-regexp-in-string "^/*" "" "/sap/bc/adt/core/discovery"))
    ;;                  :sync t
    ;;                  :headers (list (cons "X-CSRF-Token" "Fetch"))
    ;;                  :params `((sap-client . ,abaplib--auth-client))
    ;;                  ))
    ;;        (csrf_token (list (cons "x-csrf-token"
    ;;                                (request-response-header response "x-csrf-token")))))
    ;;   (setq headers (append headers csrf_token login_token))))

    ;; TODO Delete :headers from args
    (append (request-response-data
             (apply #'request (if (string-match "^http[s]*://" api) api
                                (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
                                        (replace-regexp-in-string "^/*" "" api)))
                    :sync (not success)
                    :headers headers
                    :status-code '((304 . (lambda (&rest _) (message "Source Not Modified")))
                                   (401 . (lambda (&rest _) (error "Not Authorized")))
                                   (403 . (lambda (&rest _) (error "Session Expired, Try Refresh Session."))))
                    ;; :params `((sap-client . ,abaplib--auth-client))
                    :success success
                    :error  (lambda (&key error-thrown &allow-other-keys &rest _)
                              (message "Got error: %S" error-thrown))
                    ;; :complete (lambda (&rest _) (message "Complete" ))
                    args))
            nil))
  )


;;==============================================================================
;; Authentication
;;==============================================================================

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

(defun abaplib-auth-refresh-session ()
  (interactive)
  (let* ((projectp (intern abaplib--project-name))
         (login_token (car (abaplib-auth-get-login-token projectp))))
    (if login_token
        (abaplib-auth-login-with-token login_token)
      (abaplib-auth-login))))

(defun abaplib-auth-login ()
  (interactive)
  (let* ((username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "Client: "  ))
         (login_token (cons "Authorization"
                            (format "Basic %s"
                                    (base64-encode-string (concat username ":" password)))))
         )
    (abaplib-auth-login-with-token login_token client)))

(defun abaplib-auth-login-with-token (login_token &optional client)
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT"
  (message "Connecting...")
  (let* ((projectp (intern abaplib--project-name))
         (client (or client abaplib--auth-client))
         (login_uri "/sap/bc/adt/core/discovery")
         (response (request
                    (concat  abaplib--service-url
                             (replace-regexp-in-string "^/*" "" login_uri))
                    :sync t
                    :headers (list login_token (cons "X-CSRF-Token" "Fetch"))
                    :params (list (cons "sap-client" client))
                    ))
         (login_status (request-response-symbol-status response))
         (csrf_token (cons "x-csrf-token" (request-response-header response "x-csrf-token"))))

    (if (not (eq login_status 'success))
        (error "Connect to server Failed!")
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
      (message "Connected to server!"))
    nil
    ))

(defun abaplib-auth-ensure-login ()
  "Ensure logged In"
  (abaplib-project-ensure-inside-project)
  (let* ((projectp (intern (or abaplib--project-name
                               (call-interactively 'abaplib-project-switch))))
         (is_logged (alist-get projectp abaplib--auth-data)))
    (unless is_logged ;; Try to login if not logged in
      (call-interactively 'abaplib-auth-login)))
  )

;;==============================================================================
;; Services - Search
;;==============================================================================

(defun abaplib-srv-search-objects (object-name)
  (xml-get-children
   (abaplib-service-call
    (abaplib-service-get-uri 'search-object (format "%s%%2A" object-name))
    nil
    :parser 'abaplib-util-xml-parser)
   'objectReference))

(defun abaplib-srv-search--compose-sel-list (object-list)
  (mapcar (lambda (obj)
            (let* (
                   ;; (attrs       (xml-node-attributes obj))
                   (type        (xml-get-attribute obj 'type))
                   (name        (xml-get-attribute obj 'name))
                   ;; (packageName (xml-get-attribute obj 'packageName))
                   (description (xml-get-attribute obj 'description))
                   )
              (list (format "%-8s%-31s%s" type name description))))
          object-list))

;;==============================================================================
;; Services - Pull Object From Server
;;==============================================================================
(defun abaplib-srv-pull-object ()
  (interactive
   (let* ((object-name (read-string "Enter Search String: "))
          (object-list (abaplib-srv-search-objects object-name))
          (object-plain-list (abaplib-srv-search--compose-sel-list object-list))
          (selected-object (split-string
                            (completing-read "Maching Items: " object-plain-list)
                            " "
                            t))
          (object-type (car selected-object))
          (object-name (car (cdr selected-object))))

     (cond ((string= object-type "PROG/P" ) (abaplib-srv-pull-prog object-name))
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

(defun abaplib-srv-pull-prog (prog_name)
  ;; Retrieve metadata
  (abaplib-service-call
   (abaplib-service-get-uri 'get-program-metadata prog_name)
   (lambda (&rest data)
     (let ((prog_metadata (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.xml" abaplib--project-config-dir prog_name)))
       (write-region prog_metadata nil file)
       ))
   :parser 'abaplib-util-sourcecode-parser
   )
  ;; Retrieve source
  (abaplib-service-call
   (abaplib-service-get-uri 'get-program-source prog_name)
   (lambda (&rest data)
     (let ((prog_source (format "%s" (cl-getf data :data)))
           (file (format "%s/%s.prog.abap" abaplib--project-dir prog_name)))
       (unless (string= prog_source "")
         (write-region prog_source nil file)
         nil
         )))
   :parser 'abaplib-util-sourcecode-parser
   ;; :headers (list '("If-None-Match" . "201704241108050011")
   ;;                '("Content-Type" . "plain/text"))
   ))

;;==============================================================================
;; Describe Object - Bufer/File Related
;;==============================================================================
(defun abaplib-object-describe()
  (abaplib-project-ensure-inside-project)
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

(defun abaplib-object--get-property(property-name)
  (unless abaplib--object-props
    (abaplib-object-describe))
  (cdr (assq property-name abaplib--object-props ))
  )

(defun abaplib-object-name ()
  (abaplib-object--get-property 'name))

(defun abaplib-object-get-version()
  (abaplib-object--get-property 'version))

(defun abaplib-object-get-type()
  (abaplib-object--get-property 'type))
;; (let ((core_type (abaplib-object--get-property 'type)))
;;   (cond ((string= core_type "PROG/P") "prog")
;;         (t nil))))


;;==============================================================================
;; Service - Syntax Check
;;==============================================================================

(defun abaplib-srv-check-syntax-template (adtcore_uri chkrun_uri version &optional chkrun_content)
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


(defun abaplib-srv-check-syntax ()
  " Check syntax for source code in current buffer"
  (interactive)
  (message "Syntax check...")
  (let ((object_name (abaplib-object-name))
        (object_type (abaplib-object-get-type))
        (object_version (abaplib-object-get-version))
        (object_source (buffer-substring-no-properties (point-min) (point-max))))
    (cond ((string= object_type "PROG/P") (abaplib-srv-check-prog
                                           object_name
                                           object_source
                                           object_version
                                           ))
          (t nil))
    )
  )


(defun abaplib-srv-check-render-type-text(type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other"))
  )

(defun abaplib-srv-check-render-pos(position &optional target_buffer)
  (let* ((target_buffer (or target_buffer (current-buffer)))
         (pos_list (split-string position ","))
         (line (string-to-number (car pos_list)))
         (column (string-to-number (car (cdr pos_list))))
         (map (make-sparse-keymap))
         (fn_follow_pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target_buffer)
                           (abaplib-util-goto-position ,line ,column))
                        ))
    (define-key map (kbd "<down-mouse-1>") fn_follow_pos)
    (define-key map (kbd "<RET>") fn_follow_pos)
    (propertize position
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

(defun abaplib-srv-check-show-message (messages)
  (let ((severity_level "I")
        (output_log))
    (dolist (message messages severity_level output_log)
      (let* ((uri (xml-get-attribute message 'uri))
             (type (xml-get-attribute message 'type))
             (text (xml-get-attribute message 'shortText))
             (position (progn
                         (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                         (match-string 1 uri))))

        (if (or (and (string= type "W") (string= severity_level "I"))
                (and (string= type "E") (or (string= severity_level "W")
                                            (string= severity_level "I"))))
            (setq severity_level type))

        (setq output_log
              (concat output_log "\n"
                      (concat (format "[%s] " (abaplib-srv-check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib-srv-check-render-pos position))
                              text)))
        ))

    (if output_log
        (abaplib-util-log-buf-write output_log))

    (cond ((string= severity_level "I")
           (message "Syntax check completed with `success' result."))
          ((string= severity_level "W")
           (message "Syntax check completed with `warning' messages."))
          ((string= severity_level "E")
           (progn
             (message "Syntax check completed with `error' messages.")
             (abaplib-util-log-buf-pop))))
    ))

(defun abaplib-srv-check-prog (prog_name source &optional version )
  "Check ABAP program syntax based on local unsubmitted source"
  (let* ((version (or version "active"))
         (adtcore_uri (concat "/sap/bc/adt/programs/programs/" prog_name))
         (chkrun_uri  (concat adtcore_uri "/source/main"))
         (chkrun_content (base64-encode-string source))
         (post_data (abaplib-srv-check-syntax-template
                     adtcore_uri
                     chkrun_uri version chkrun_content)))
    ;; before post
    ;; (message post_data)
    (abaplib-service-call
     (abaplib-service-get-uri 'checkrun)
     (lambda (&rest data)
       (let* ((check_report (xml-get-children (cl-getf data :data) 'checkReport))
              (message_list (xml-get-children (car check_report) 'checkMessageList))
              (messages (xml-get-children (car message_list) 'checkMessage)))
         (abaplib-srv-check-show-message messages)
         ))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post_data
     :headers (list (cons "Content-Type" "application/vnd.sap.adt.checkobjects+xml"))
     )
    ))
;;========================================================================
;; Service - Push Source
;;========================================================================
(defun abaplib-srv-push-prog ()
  (interactive)
  (let ((prog_name   (abaplib-get-object-name))
        (prog_source (buffer-substring-no-properties (point-min) (point-max))))
    (abaplib-service-call
     (abaplib-get-service-uri 'save-program-source prog_name)
     (lambda (&rest response)
       (let ((ETag (request-response-header response "ETag")))
         (message (format "ETAG:%s" ETag))))
     :type "PUT"
     :data prog_source
     ;; :headers (list (cons ""))
     )))

(provide 'abaplib)
;;; abaplib.el ends here
