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
  "ABAP Object Properties")

;; (defvar-local abaplib--lock-handle nil
;;   "Lock Handle")

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
   (let* ((system-id (read-string "System ID: "))
          (server-address (read-string "Server Address: "))
          (http-port (read-string "ICM HTTP Port: ")))
     (unless (file-directory-p abap-workspace-dir)
       (make-directory abap-workspace-dir))
     (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir system-id))
     (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
     (setq abaplib--project-name system-id)
     ;; (setq abaplib-system-ID system-id)
     (if (file-directory-p abaplib--project-dir)
         (error "Project %s already exist!" system-id)
       (let ((abap-config-file (format "%s/server.ini" abaplib--project-config-dir)))
         (make-directory abaplib--project-dir)
         (make-directory abaplib--project-config-dir)
         (write-region
          (concat
           (format "Server=%s\n" server-address)
           (format "HttpPort=%s\n" http-port))
          nil
          abap-config-file)
         (setq abaplib--service-url (format "http://%s:%s/" server-address http-port))
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
  (let ((project-name (completing-read "Select Project: " (abaplib-project-get-list))))
    (abaplib-project-switch-context project-name)
    (unless silent?
      (helm-open-dired abaplib--project-dir))
    )
  )

(defun abaplib-project-switch-context (project-name)
  "Go to project root directory"
  (unless (string= project-name abaplib--project-name)
    ;; (setq abaplib--token nil)
    (let* ((abap-config-dir (expand-file-name (format "%s/.abap" project-name) abap-workspace-dir))
           (abap-config-file (expand-file-name "server.ini" abap-config-dir)))
      ;; Create buffer
      ;; (abaplib-util-log-buf-init)

      ;; Read server information
      (with-temp-buffer
        (insert-file-contents abap-config-file)
        (let ((server-address)
              (http-port))
          (mapcar
           (lambda (item)
             (let* ((config (split-string item "=" t))
                    (config-id (car config))
                    (config-value (car (cdr config))))
               (cond ((string= config-id "Server") (setq server-address config-value))
                     ((string= config-id "HttpPort") (setq http-port config-value)))
               ))
           (split-string (buffer-string) "\n" t))
          (unless (and server-address http-port)
            (error "Not a valid project!"))
          (cond
           ((string= http-port "80") (setq abaplib--service-url (format "http://%s/" server-address)))
           ((string= http-port "44300") (setq abaplib--service-url (format "https://%s:%s/" server-address http-port)))
           ((string= http-port "443") (setq abaplib--service-url (format "https://%s/" server-address)))
           (t (setq abaplib--service-url (format "http://%s:%s/" server-address http-port))))
          (setq abaplib--project-dir (format "%s/%s" abap-workspace-dir project-name))
          (setq abaplib--project-config-dir (format "%s/.abap/" abaplib--project-dir))
          (setq abaplib--project-name project-name)
          ))
      )
    )
  )
(defun abaplib-project-ensure-inside-project()
  "Ensure in a project"
  (let* ((proj-dir (abaplib-util-current-dir))
         (folders (split-string proj-dir "/" t))
         (proj-name (car (last folders)))
         (val-dir (expand-file-name (concat abap-workspace-dir "/" proj-name "/"))))
    (if (string= proj-dir val-dir)
        (abaplib-project-switch-context proj-name)
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

(defun abaplib-service-get-uri (service &optional object-name)
  (cond
   ((eq service 'search-object)
    (format "/sap/bc/adt/repository/informationsystem/search?operation=quickSearch&query=%s&maxResults=%s" object-name abap-query-list-max-result))
   ((eq service 'get-program-metadata)
    (format "/sap/bc/adt/programs/programs/%s" object-name))
   ((eq service 'get-program-source)
    (format "/sap/bc/adt/programs/programs/%s/source/main" object-name))
   ((eq service 'save-program-source)
    (format "/sap/bc/adt/programs/programs/%s/source/main" object-name))
   ((eq service 'checkrun)
    (format "/sap/bc/adt/checkruns?reporters=abapCheckRun"))
   ((eq service 'lock)
    (format "/sap/bc/adt/programs/programs/%s?_action=LOCK&accessMode=MODIFY" object-name))
   ((eq service 'unlock)
    (format "/sap/bc/adt/programs/programs/%s?_action=UNLOCK" object-name))
   ((eq service 'activate)
    (format "/sap/bc/adt/activation?method=activate&preauditRequested=true"))
   (t (error "Service not implemented!"))
   ))

(defun abaplib-service-call (api success &rest args)
  "Invoke corresponding service API."

  (abaplib-auth-ensure-login)

  (let* ((projectp (intern abaplib--project-name))
         (login-token (abaplib-auth-get-login-token projectp))
         (csrf-token (abaplib-auth-get-csrf-token projectp))
         (headers (cl-getf args :headers))
         (type    (cl-getf args :type)))

    ;; For method like POST, PUT, DELETE, etc Required to Validate CSRF Token
    (unless (or (not type)
                (string= type "GET"))
      (setq headers (append headers
                            ;; login-token
                            csrf-token)))
    ;; (let* ((response (request
    ;;                  (concat  abaplib--service-url
    ;;                           (replace-regexp-in-string "^/*" "" "/sap/bc/adt/core/discovery"))
    ;;                  :sync t
    ;;                  :headers (list (cons "X-CSRF-Token" "Fetch"))
    ;;                  :params `((sap-client . ,abaplib--auth-client))
    ;;                  ))
    ;;        (csrf-token (list (cons "x-csrf-token"
    ;;                                (request-response-header response "x-csrf-token")))))
    ;;   (setq headers (append headers csrf-token login-token))))

    ;; TODO Delete :headers from args
    (append (request-response-data
             (apply #'request (if (string-match "^http[s]*://" api) api
                                (concat (replace-regexp-in-string "/*$" "/" abaplib--service-url)
                                        (replace-regexp-in-string "^/*" "" api)))
                    :sync (not success)
                    :headers headers
                    :status-code '((304 . (lambda (&rest -) (message "304 Source Not Modified")))
                                   (401 . (lambda (&rest -) (message "401 Not Authorized")))
                                   (403 . (lambda (&rest -) (message "403 Forbidden")))
                                   )
                    ;; :params `((sap-client . ,abaplib--auth-client))
                    :success success
                    :error  (lambda (&key error-thrown &allow-other-keys &rest -)
                              (let ((error-message)))
                              (if error-thrown
                                  (setq error-message ((lambda (exception-node) 
                                                         (car (last
                                                               (car (xml-get-children exception-node 'localizedMessage)))))
                                                       error-thrown))
                                (setq error-message "Unknown"))
                              (message error-message))
                    ;; :complete (lambda (&rest -) (message "Complete" ))
                    args))
            nil))
  )


;;==============================================================================
;; Authentication
;;==============================================================================

(defun abaplib-auth-set-login-token (projectp token)
  (let ((login-token (list (cons 'login-token (list token)))))
    (setcdr (assq 'ER9 abaplib--auth-data)
            (append login-token (alist-get 'ER9 abaplib--auth-data))
            )))

(defun abaplib-auth-get-login-token (projectp)
  (alist-get 'login-token (alist-get projectp abaplib--auth-data)))

(defun abaplib-auth-set-csrf-token (projectp token)
  (let ((csrf-token (list (cons 'csrf-token (list token)))))
    (setcdr (assq 'ER9 abaplib--auth-data)
            (append csrf-token (alist-get 'ER9 abaplib--auth-data))
            )))

(defun abaplib-auth-get-csrf-token (projectp)
  (alist-get 'csrf-token (alist-get projectp abaplib--auth-data)))

(defun abaplib-auth-refresh-session ()
  (interactive)
  (let* ((projectp (intern abaplib--project-name))
         (login-token (car (abaplib-auth-get-login-token projectp))))
    (if login-token
        (abaplib-auth-login-with-token login-token)
      (abaplib-auth-login))))

(defun abaplib-auth-login ()
  (interactive)
  (let* ((username (upcase (read-string "Username: ")))
         (password (read-passwd "Password: "))
         (client   (read-string "Client: "  ))
         (login-token (cons "Authorization"
                            (format "Basic %s"
                                    (base64-encode-string (concat username ":" password)))))
         )
    (abaplib-auth-login-with-token login-token client)))

(defun abaplib-auth-login-with-token (login-token &optional client)
  "Login into ABAP Server as user USERNAME with PASSWORD and CLIENT"
  ;; FIXME login couldn't refresh CSRF Token
  (message "Connecting...")
  (let* ((projectp (intern abaplib--project-name))
         (client (or client abaplib--auth-client))
         (login-uri "/sap/bc/adt/core/discovery")
         (response (request
                    (concat  abaplib--service-url
                             (replace-regexp-in-string "^/*" "" login-uri))
                    :sync t
                    :headers (list login-token (cons "X-CSRF-Token" "Fetch"))
                    :params (list (cons "sap-client" client))
                    ))
         (login-status (request-response-symbol-status response))
         (csrf-token (cons "x-csrf-token" (request-response-header response "x-csrf-token"))))

    (if (not (eq login-status 'success))
        (error "Connect to server Failed!")
      "Init project auth data"
      (if (alist-get projectp abaplib--auth-data)
          ;; Remove previous login data
          (setcdr (assq projectp abaplib--auth-data)
                  t)
        (add-to-list 'abaplib--auth-data (cons projectp t))
        )

      (abaplib-auth-set-login-token projectp login-token)
      (abaplib-auth-set-csrf-token projectp csrf-token)

      (setq abaplib--auth-client client)
      (message "Connected to server!"))
    nil
    ))

(defun abaplib-auth-ensure-login ()
  "Ensure logged In"
  (abaplib-project-ensure-inside-project)
  (let* ((projectp (intern (or abaplib--project-name
                               (call-interactively 'abaplib-project-switch))))
         (is-logged (alist-get projectp abaplib--auth-data)))
    (unless is-logged ;; Try to login if not logged in
      (call-interactively 'abaplib-auth-login)))
  )

;;==============================================================================
;; Core Services - Search
;;==============================================================================

(defun abaplib-core-search-objects (object-name)
  (xml-get-children
   (abaplib-service-call
    (abaplib-service-get-uri 'search-object (format "%s%%2A" object-name))
    nil
    :parser 'abaplib-util-xml-parser)
   'objectReference))

(defun abaplib-core-search--compose-sel-list (object-list)
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
;; Core Services - Pull Object From Server
;; TODO Consider ETag
;;==============================================================================
(defun abaplib-core-pull-object ()
  (interactive
   (let* ((object-name (read-string "Enter Search String: "))
          (object-list (abaplib-core-search-objects object-name))
          (object-plain-list (abaplib-core-search--compose-sel-list object-list))
          (selected-object (split-string
                            (completing-read "Maching Items: " object-plain-list)
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

(defun abaplib-core-pull-prog (prog-name)
  ;; Retrieve metadata
  (abaplib-service-call
   (abaplib-service-get-uri 'get-program-metadata prog-name)
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

;;==============================================================================
;; Describe Object - Bufer/File Related
;;==============================================================================
(defun abaplib-object-describe()
  (abaplib-project-ensure-inside-project)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (components  (split-string file-name "\\." t))
         (object-name (car components))          ; Object Name
         (sub-type (car (cdr components)))       ; Sub Type  , prog/clas/ddls
         (source-type (car (last components)))   ; Major Type, abap/cds
         (property-file (format "%s%s.%s.xml"
                                abaplib--project-config-dir
                                object-name
                                sub-type)))
    (setq abaplib--object-props (with-temp-buffer
                                  (insert-file-contents property-file)
                                  (let* ((xml-root (libxml-parse-xml-region (point-min) (point-max)))
                                         (properties (xml-node-attributes xml-root)))
                                    properties
                                    )))
    ))

(defun abaplib-object--get-property(property-name)
  (unless abaplib--object-props
    (abaplib-object-describe))
  (cdr (assq property-name abaplib--object-props ))
  )

(defun abaplib-object-get-name ()
  (abaplib-object--get-property 'name))

(defun abaplib-object-get-version()
  (abaplib-object--get-property 'version))

(defun abaplib-object-get-type()
  (abaplib-object--get-property 'type))
;; (let ((core-type (abaplib-object--get-property 'type)))
;;   (cond ((string= core-type "PROG/P") "prog")
;;         (t nil))))


;;==============================================================================
;; Service - Syntax Check
;;==============================================================================

(defun abaplib-core-check-syntax-template (adtcore-uri chkrun-uri version &optional chkrun-content)
  "Return xml of checkObjects"
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<chkrun:checkObjectList xmlns:adtcore=\"http://www.sap.com/adt/core\" xmlns:chkrun=\"http://www.sap.com/adt/checkrun\">"
   (format "<chkrun:checkObject adtcore:uri=\"%s\" chkrun:version=\"%s\">"
           adtcore-uri
           version)
   (if chkrun-content
       (concat
        "<chkrun:artifacts>"
        (format "<chkrun:artifact chkrun:contentType=\"text/plain; charset=utf-8\" chkrun:uri=\"%s\">"
                chkrun-uri)
        (format "<chkrun:content>%s</chkrun:content>"
                chkrun-content)
        "</chkrun:artifact>"
        "</chkrun:artifacts>" )
     ""
     )
   "</chkrun:checkObject>"
   "</chkrun:checkObjectList>"
   ))


(defun abaplib-core-check-syntax ()
  " Check syntax for source code in current buffer"
  (interactive)
  (message "Syntax check...")
  (let ((object-name (abaplib-object-get-name))
        (object-type (abaplib-object-get-type))
        (object-version (abaplib-object-get-version))
        (object-source (buffer-substring-no-properties (point-min) (point-max))))
    (cond ((string= object-type "PROG/P") (abaplib-core-check-prog
                                           object-name
                                           object-source
                                           object-version
                                           ))
          (t nil))
    )
  )


(defun abaplib-core-check-render-type-text(type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other"))
  )

(defun abaplib-core-check-render-pos(position &optional target-buffer)
  (let* ((target-buffer (or target-buffer (current-buffer)))
         (pos-list (split-string position ","))
         (line (string-to-number (car pos-list)))
         (column (string-to-number (car (cdr pos-list))))
         (map (make-sparse-keymap))
         (fn-follow-pos `(lambda ()
                           (interactive)
                           (pop-to-buffer ,target-buffer)
                           (abaplib-util-goto-position ,line ,column))
                        ))
    (define-key map (kbd "<down-mouse-1>") fn-follow-pos)
    (define-key map (kbd "<RET>") fn-follow-pos)
    (propertize position
                'face 'underline
                'mouse-face 'highlight
                'keymap map)))

(defun abaplib-core-check-show-message (messages)
  (let ((severity-level "I")
        (output-log))
    (dolist (message messages severity-level output-log)
      (let* ((uri (xml-get-attribute message 'uri))
             (type (xml-get-attribute message 'type))
             (text (xml-get-attribute message 'shortText))
             (position (progn
                         (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                         (match-string 1 uri))))

        (if (or (and (string= type "W") (string= severity-level "I"))
                (and (string= type "E") (or (string= severity-level "W")
                                            (string= severity-level "I"))))
            (setq severity-level type))

        (setq output-log
              (concat output-log "\n"
                      (concat (format "[%s] " (abaplib-core-check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib-core-check-render-pos position))
                              text)))
        ))

    (if output-log
        (abaplib-util-log-buf-write output-log))

    (cond ((string= severity-level "I")
           (message "Syntax check completed with `success' result."))
          ((string= severity-level "W")
           (message "Syntax check completed with `warning' messages."))
          ((string= severity-level "E")
           (progn
             (message "Syntax check completed with `error' messages.")
             (abaplib-util-log-buf-pop))))
    ))

(defun abaplib-core-check-prog (prog-name source &optional version )
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
;;========================================================================
;; Service - Lock & Unlock
;;========================================================================

(defun abaplib-core-lock()
  ;; (interactive)  ;;  "Testing phase"
  ;; Should be invoked in sync way
  ;; (setq abaplib--lock-handle nil)
  (let* ((prog-name (abaplib-object-get-name))
         (root-node (abaplib-service-call
                     (abaplib-service-get-uri 'lock prog-name)
                     nil
                     :parser 'abaplib-util-xml-parser
                     :type "POST"
                     :headers `(("X-sap-adt-sessiontype" . "stateful"))
                     ))
         (node-name (xml-node-name root-node)))
    (if (string= node-name "abap")
        ;; (setq abaplib--lock-handle
        ((lambda (abap-node) ;; Get lock handle
           (car (last
                 (car (xml-get-children
                       (car (xml-get-children
                             (car (xml-get-children abap-node 'values))
                             'DATA))
                       'LOCK_HANDLE))))) root-node)
      ;; )
      ;; Request lock failed
      (let ((error-message
             ((lambda (exception-node)
                (car (last
                      (car (xml-get-children exception-node 'localizedMessage)))))
              root-node)))
        (error  error-message))
      )
    ))

;; (defun abaplib-core-lock-get-handle()
;;   (unless abaplib--lock-handle
;;     (abaplib-core-lock))
;;   abaplib--lock-handle
;;   )

(defun abaplib-core-unlock (lock-handle)
  (let ((prog-name (abaplib-object-get-name)))
    (abaplib-service-call
     (abaplib-service-get-uri 'unlock prog-name)
     (lambda (&rest response)
       (message "Unlocked."))
     :type "POST"
     :headers `(("X-sap-adt-sessiontype" . "stateless"))
     :params `(("lockHandle" . ,lock-handle))
     )))

;;========================================================================
;; Service - Push Source
;;========================================================================

(defun abaplib-core-push-prog ()
  (interactive)
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

;;========================================================================
;; Service - Push Source
;;========================================================================
(defun abaplib-core-activate-template (adtcore-name adtcore-uri)
  "Return xml of checkObjects"
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<adtcore:objectReferences xmlns:adtcore=\"http://www.sap.com/adt/core\">"
   (format "<adtcore:objectReference adtcore:name=\"%s\" adtcore:uri=\"%s\"/>"
           adtcore-name
           adtcore-uri)
   "</adtcore:objectReferences>"
   ))

(defun abaplib-core-activate-show-message (messages)
  (let ((severity-level "I")
        (output-log))
    (dolist (message messages severity-level output-log)
      (let* ((uri (xml-get-attribute message 'href))
             (type (xml-get-attribute message 'type))
             (text (car (last (car (xml-get-children
                                    (car (xml-get-children message 'shortText))
                                    'txt)))))
             (position (progn
                         (string-match "#start=\\([0-9]+,[0-9]+\\)" uri)
                         (match-string 1 uri))))
        (if (or (and (string= type "W") (string= severity-level "I"))
                (and (string= type "E") (or (string= severity-level "W")
                                            (string= severity-level "I"))))
            (setq severity-level type))

        (setq output-log
              (concat output-log "\n"
                      (concat (format "[%s] " (abaplib-core-check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib-core-check-render-pos position))
                              text)))
        ))

    (if output-log
        (abaplib-util-log-buf-write output-log))

    (cond ((string= severity-level "I")
           (message "Activation successful"))
          ((string= severity-level "W")
           (message "Activation successful with `warnings'"))
          ((string= severity-level "E")
           (progn
             (message "Activation failed with `errors'")
             (abaplib-util-log-buf-pop))))
    ))

(defun abaplib-core-activate-prog ()
  (interactive)
  (let* ((prog-name   (abaplib-object-get-name))
         (adtcore-name (upcase prog-name))
         (adtcore-uri (concat "/sap/bc/adt/programs/programs/" prog-name))
         (post-xml (abaplib-core-activate-template adtcore-name adtcore-uri)))
    (abaplib-service-call
     (abaplib-service-get-uri 'activate)
     (lambda (&rest rest)
       (let* ((messages (xml-get-children (cl-getf rest :data) 'msg)))
         (abaplib-core-activate-show-message messages)
         ))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post-xml
     ))
  )

(provide 'abaplib)
;;; abaplib.el ends here
