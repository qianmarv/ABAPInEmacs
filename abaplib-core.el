;;; abaplib-core.el --- sap abap server lib          -*- lexical-binding: t; -*-

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
;; TODO Most of service function shouldn't bind tight with project
;;      the `abaplib--current-project' should be considered as always
;;      have value which guaranteed by outside consumer, otherwise consider
;;      as an exception.

;; (eval-when-compile (require 'cl))
(require 'request)
(require 'abaplib-util)


;; (defgroup abap nil
;;   "ABAP development environment in Emacs.")

(defcustom abap-workspace-dir "~/ws-abap/"
  "ABAP workspace directory"
  :type 'string
  :group 'abap)

(defcustom abap-save-login-credential t
  "Save logon data"
  :type 'boolean
  :group 'abap)

(defcustom abap-login-timeout 1800
  "login timeout (seconds)"
  :type 'number
  :group 'abap)


(defcustom abap-search-list-max-results 51
  "Object query list maximum result"
  :type 'number
  :group 'abap)

(defvar abap-log-level nil
  "Logging level for abap
   One of value `debug'")

(defvar abaplib--login-token-cache nil
  "ABAP token used for authentication.")

(defvar abaplib--sap-client-cache nil
  "ABAP system logon client")

(defvar abaplib--login-last-time nil
  "Last login timestamp")

(defconst abaplib-core--not-a-type "ZZZZ")

(defconst abaplib-core--supported-type  '(PROG CLAS DCLS DDLS ZZZZ)
  "Supported ABAP Development Object Type")

;; (defvar abaplib--auth-data nil
;;   "System Login State")

;; (defvar abaplib--auth-client nil
;;   "ABAP system client used for login.")

;; (defvar abaplib-system-ID nil
;;   "ABAP system client used for login.")
(defvar abaplib--workspace-descriptor-cache nil
  "Cache for workspace descriptor")

;; (defvar abaplib--current-server nil
;;   "The address of the abap server host with port
;;    For instance: https://yourabapserver:44300/")

(defvar abaplib--current-project nil
  "Current working project")

;; (defvar abaplib--project-data nil
;;   "ABAP project data")

;; (defvar abaplib--project-name nil
;;   "Current ABAP Project")

;; (defvar abaplib--project-dir nil
;;   "ABAP Project Directory")

;; (defvar abaplib--project-config-dir nil
;;   "ABAP Project Configuration Directory")

(defvar-local abaplib--object-props nil
  "ABAP Object Properties")

;; (defvar-local abaplib--lock-handle nil
;;   "Lock Handle")

(defconst abaplib--log-buffer "*ABAP Log*"
  "ABAP log buffer")

(defconst abaplib-core--root-uri nil
  "root uri of restful API")

(defconst abaplib-core--uri-login "/sap/bc/adt/core/discovery")

(defconst abaplib-core--folder-S "Source Code Library")
(defconst abaplib-core--folder-C "Core Data Services")

;;==============================================================================
;; Project
;;==============================================================================
(defun abaplib-get-ws-describe-file()
  "Get workspace description file"
  (let* ((config-dir (expand-file-name ".abap" abap-workspace-dir))
         (describe-file (expand-file-name "projects.json" config-dir)))
    (unless (file-directory-p config-dir) ;; Initialize for first time
      (make-directory config-dir))
    (unless (file-exists-p describe-file)
      (abaplib-util-jsonize-to-file nil describe-file))
    describe-file))

(defun abaplib-save-workspace-descriptor (descriptor)
  "Save descriptor to workspace in `overwrite' way"
  (abaplib-util-jsonize-to-file
   descriptor
   (abaplib-get-ws-describe-file))
  (setq abaplib--workspace-descriptor-cache descriptor))

(defun abaplib-get-workspace-descriptor()
  "Get workspace descriptor"
  (unless abaplib--workspace-descriptor-cache
    (setq abaplib--workspace-descriptor-cache
          (json-read-file (abaplib-get-ws-describe-file))))
  abaplib--workspace-descriptor-cache)

(defun abaplib-get-project-props (project)
  "Get project properties by project key.
   In current implemention should be symbol of project dir"
  (assoc-string project (abaplib-get-workspace-descriptor)))

(defun abaplib-project-set-property (project property)
  "Set property to project, property should be a key value pair"
  (let* ((project-props (abaplib-get-project-props project))
         (new-props (abaplib-util-upsert-alists project-props property)))
    (abaplib-upsert-project new-props)))

(defun abaplib-project-get-property (key &optional project)
  (let ((project (or project abaplib--current-project)))
    (unless project
      (error "Missing project"))
    (cdr (assoc-string key
                     (cdr (abaplib-get-project-props project))))))

(defun abaplib-upsert-project(project-props)
  "When create or init project, add project information into workspace descriptor"
  (let* ((descriptor (abaplib-get-workspace-descriptor))
         (new-descriptor (abaplib-util-upsert-alists descriptor project-props)))
    (abaplib-save-workspace-descriptor new-descriptor)))

(defun abaplib-create-project (project-dir)
  "Create project, if already exist, do nothing"
  (let* ((project-dir (replace-regexp-in-string "/$" "" project-dir))
         (project-props-curr (abaplib-get-project-props project-dir))
         (project-props-new (or project-props-curr
                            (cons (intern project-dir)
                                  (list (cons 'path project-dir))))))
    ;; Warning if already exist
    (if project-props-curr
        (error "Project %s already exist!" project-dir)
      (progn
        ;; Create project directory
        (unless (file-directory-p project-dir)
          (make-directory project-dir))
        (abaplib-upsert-project project-props-new)
        (abaplib-switch-project project-dir)))))

(defun abaplib-project-init-propose (dir)
  "Propose project automatically"
  (if dir (cond
           ((locate-dominating-file dir ".git"))
           ((locate-dominating-file dir ".abap"))
           (t abap-workspace-dir))
    abap-workspace-dir))

(defun abaplib-get-project-list ()
  "Get project list described in workspace descriptor file <workspace_dir>/.abap_workspace"
  (let ((descriptor (abaplib-get-workspace-descriptor)))
    (mapcar
     (lambda (project-props)
       (car project-props))
     descriptor)))

(defun abaplib-switch-project (project)
  "Switch variable `abaplib--current-project' and go to project directory"
  (setq abaplib--current-project project))

(defun abaplib-remove-project (project)
  "Remove project from workspace"
  (let* ((descriptor (abaplib-get-workspace-descriptor))
         (new-descriptor (seq-filter
                          (lambda (project-props)
                            (not (eq (car project-props) (intern project))))
                          descriptor)))
    (abaplib-save-workspace-descriptor new-descriptor)
    (setq abaplib--current-project nil)))

(defun abaplib-get-project-api-url (uri)
  "Compose full API url"
  (if (string-match "^http[s]*://" uri)
      uri
    (concat (replace-regexp-in-string "/*$" "/" (abaplib-project-get-property 'server))
            (replace-regexp-in-string "^/*" "" uri))))

(defun abaplib-get-project-cache-dir ()
  "Get project cache directory"
  (let ((cache-dir (expand-file-name
                    ".cache"
                    (abaplib-project-get-property 'path))))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir))
    cache-dir))

(defun abaplib-get-project-path ()
  "Get project path"
  (abaplib-project-get-property 'path))

;;==============================================================================
;; Authentication
;;==============================================================================

(defun abaplib-add-server-to-project (project server)
  (abaplib-project-set-property project (cons 'server server)))

(defun abaplib-auth-login-with-token(project login-token sap-client)
  "Login into ABAP Server with token"
  (let ((url ;; "http://ldcier9.wdf.sap.corp:50000/sap/bc/adt/core/discovery"
         (abaplib-get-project-api-url abaplib-core--uri-login))
        (server (abaplib-project-get-property 'server project))
        (login-status))
    (unless server
      (error "Project %s not bind to any server" project))

    (message "Login...")

    ;; First loing with token to get cookie
    (request
     url
     :sync t
     :headers (list (cons "Authorization" login-token))
     :params (list (cons "sap-client" sap-client)))

    ;; Login with cookie
    (setq login-status
          (request-response-symbol-status
           (request
            url
            :sync t)))

    (unless (eq login-status 'success)
      (error "Connect to server Failed!"))

    ;; Login succeed
    (setq abaplib--login-last-time (time-to-seconds (current-time)))
    (when abap-save-login-credential
      (let ((login-credential (cons 'login_token login-token))
            (sap-client       (cons 'sap_client sap-client)))
        (message "%s" login-credential)
        (abaplib-project-set-property project login-credential)
        (abaplib-project-set-property project sap-client)))
    (message "Connected to server!")))

(defun abaplib-get-login-token ()
  "Get login token and validate `abaplib--login-token-cache'"
  (let ((login-token (or abaplib--login-token-cache
                         (abaplib-project-get-property 'login_token))))
    (unless login-token
      (error "Please login first"))
    (setq abaplib--login-token-cache login-token)))

(defun abaplib-get-sap-client ()
  "Get sap client and validate `abaplib--sap-client-cache'"
  (let ((sap-client (or abaplib--sap-client-cache
                        (abaplib-project-get-property 'sap_client))))
    (unless sap-client
      (error "Please login first"))
    (setq abaplib--sap-client-cache sap-client)))


;;==============================================================================
;; Core Services
;;==============================================================================
(defun abaplib-core-service-dispatch (service abap-object)
  " ABAP Service Dispatch
    Paramters:
      service: could be one of
        - `search'
        - `retrieve'
        - `check'
        - `submit'
        - `activate'
   There're specific implenmentation for each service need to be done."
  (let* ((object-type (when (listp abap-object)
                        (alist-get 'type abap-object)))
         (service-function (abaplib-core--get-service-func service object-type)))
    (apply service-function (list abap-object))))

(defun abaplib-core--compose-func-name (service impl-prefix)
  "Internal function: compose function name"
  (concat "abaplib-"
          impl-prefix
          "-do-"
          (symbol-name service)))

(defun abaplib-core--get-service-func (service object-type)
  "Internal function: get service function"
  (let* ((impl-prefix (case (intern object-type)
                        ('PROG "program")
                        ('CLAS "class")
                        ('DLCS "cds")
                        (t "core")))
         (service-function (intern (abaplib-core--compose-func-name service impl-prefix)))
         (fallback-function (intern (abaplib-core--compose-func-name service "core"))))
    ;; (message service-function)
    (if (fboundp service-function)
        service-function
      fallback-function)))

(defun abaplib-core-get-csrf-token ()
  (let* ((login-token (cons "Authorization" (abaplib-get-login-token)))
         (sap-client (abaplib-get-sap-client))
         (login-url (abaplib-get-project-api-url abaplib-core--uri-login))
         (response (request
                    login-url
                    :sync t
                    :headers (list (cons "X-CSRF-Token" "Fetch") login-token)
                    :params `((sap-client . ,sap-client)))))
    (request-response-header response "x-csrf-token")))

(defun abaplib--rest-api-call(uri success-callback &rest args)
  "Call service API."
  (let* ((url (abaplib-get-project-api-url uri))
         (login-token (cons "Authorization" (abaplib-get-login-token)))
         (headers (cl-getf args :headers))
         (type    (or (cl-getf args :type) "GET"))
         (params (cl-getf args :params)))

    ;; Verify whether need to login with token
    ;; (let ((now (time-to-seconds (current-time))))
    ;;   (when (or (not abaplib--login-last-time)
    ;;             (> (- now abaplib--login-last-time) abap-login-timeout))
    ;;     (abaplib-auth-login-with-token abaplib--current-project
    ;;                                    login-token
    ;;                                    (abaplib-get-sap-client))))
    ;; For method like POST, PUT, DELETE, required to get CSRF Token first
    ;; (message "headers:= %s" headers)
    (if (string= type "GET")
        (setq headers (append headers (list login-token)))
      (unless (assoc-string 'x-csrf-token headers)
        (let ((csrf-token (abaplib-core-get-csrf-token)))
          (setq headers
                (append headers
                        (list (cons "x-csrf-token" csrf-token)))))))

    ;; TODO Delete :headers from args as we have explicitly put headers here
    ;; (setq params (append params
    ;;                      (list (cons "sap-client" sap-client))))
    (append (request-response-data
             (apply #'request
                    url
                    :sync (not success-callback)
                    :headers headers
                    :status-code '(
                                   ;; (304 . (lambda (&rest _) (message "304 Source Not Modified")))
                                   (401 . (lambda (&rest _) (message "Got 401: Not Authorized")))
                                   (403 . (lambda (&rest _) (message "Got 403: Forbidden"))))
                    :params params
                    :success success-callback
                    :error  (lambda (&key error-thrown &allow-other-keys &rest _)
                              (let ((error-message)))
                              (if error-thrown
                                  (setq error-message ((lambda (exception-node)
                                                         (car (last
                                                               (car (xml-get-children exception-node 'localizedMessage)))))
                                                       error-thrown))
                                (setq error-message "Unknown error occured."))
                              (message "%s" error-message))
                    ;; :complete (lambda (&rest -) (message "Complete" ))
                    args)))))


;;==============================================================================
;; Services Implementation - Search ABAP Object
;;==============================================================================

(defun abaplib-core-do-search (query-string)
  "Search ABAP objects in server in synchronouse call"
  (let* ((url (abaplib-get-project-api-url "/sap/bc/adt/repository/informationsystem/search"))
         (params `((operation . "quickSearch")
                   (query . ,(concat query-string "*"))
                   (maxResults . ,abap-search-list-max-results)))
         (data (abaplib--rest-api-call url
                                       nil
                                       :params params
                                       :parser 'abaplib-util-xml-parser))
         (object-list (xml-get-children data 'objectReference)))
    ;; (mapcar (lambda (obj)
    ;;           (let* (
    ;;                  (type        (xml-get-attribute obj 'type))
    ;;                  (name        (xml-get-attribute obj 'name))
    ;;                  ;; (packageName (xml-get-attribute obj 'packageName))
    ;;                  (description (xml-get-attribute obj 'description))
    ;;                  )
    ;;             (list (format "%-8s%-31s%s" type name description))))
    object-list))

(defun abaplib-core--raise-fallback-error (abap-object)
  (let ((object-type (alist-get 'type abap-object)))
    (error "service not implemented for abap development type %s." object-type)))

(defun abaplib-core-do-retrieve(abap-object)
  "Retrieve source: Fallback service call"
  (abaplib-core--raise-fallback-error abap-object))

(defun abaplib-core-do-check(abap-object)
  "Check source: Fallback service call"
  (abaplib-core--raise-fallback-error abap-object))


(defun abaplib-core-do-submit()
  "Submit source: Fallback service call"
  (abaplib-core--raise-fallback-error abap-object))

(defun abap-activate-source ()
  "Activate source: Fallback service call"
  (abaplib-core--raise-fallback-error abap-object))



;;==============================================================================
;; Service - Syntax Check
;;==============================================================================

(defun abaplib-core-check-template (adtcore-uri chkrun-uri version chkrun-content)
  "Return xml of checkObjects"
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<chkrun:checkObjectList xmlns:adtcore=\"http://www.sap.com/adt/core\" xmlns:chkrun=\"http://www.sap.com/adt/checkrun\">"
   (format "<chkrun:checkObject adtcore:uri=\"%s\" chkrun:version=\"%s\">"
           adtcore-uri
           version)
   (when chkrun-content
     (concat
      "<chkrun:artifacts>"
      (format "<chkrun:artifact chkrun:contentType=\"text/plain; charset=utf-8\" chkrun:uri=\"%s\">"
              chkrun-uri)
      (format "<chkrun:content>%s</chkrun:content>"
              chkrun-content)
      "</chkrun:artifact>"
      "</chkrun:artifacts>" ))
   "</chkrun:checkObject>"
   "</chkrun:checkObjectList>"))

(defun abaplib-core-check-post (version adtcore-uri chkrun-uri chkrun-content)
  (let ((post-data (abaplib-core-check-template adtcore-uri chkrun-uri version chkrun-content)))
    (abaplib--rest-api-call
     (abaplib-get-project-api-url "/sap/bc/adt/checkruns")
     (lambda (&rest rest)
       (let* ((check-report (xml-get-children (cl-getf rest :data) 'checkReport))
              (message-list (xml-get-children (car check-report) 'checkMessageList))
              (messages (xml-get-children (car message-list) 'checkMessage)))
         (abaplib-core-check-show-message messages)))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post-data
     :params '((reporters . abapCheckRun))
     :headers `(("Content-Type" . "application/vnd.sap.adt.checkobjects+xml")))))


(defun abaplib-core-check-render-type-text(type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other")))

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
                              text)))))

    (if output-log
        (abaplib-util-log-buf-write output-log))

    (cond ((string= severity-level "I")
           (message "Syntax check completed with `success' result."))
          ((string= severity-level "W")
           (message "Syntax check completed with `warning' messages."))
          ((string= severity-level "E")
           (progn
             (message "Syntax check completed with `error' messages.")
             (abaplib-util-log-buf-pop))))))


;;========================================================================
;; Service - Lock & Unlock
;;========================================================================
;;_action=LOCK&accessMode=MODIFY
(defun abaplib-core-lock-sync(uri csrf-token)
  (let* ((root-node (abaplib--rest-api-call
                     (abaplib-get-project-api-url uri)
                     nil
                     :parser 'abaplib-util-xml-parser
                     :params '((_action . LOCK)
                               (accessMode . MODIFY))
                     :type "POST"
                     :headers `(("X-sap-adt-sessiontype" . "stateful")
                                ("x-csrf-token" . ,csrf-token))))
         (node-name (xml-node-name root-node)))
    (if (string= node-name "abap")
        ((lambda (abap-node) ;; Get lock handle
           (car (last
                 (car (xml-get-children
                       (car (xml-get-children
                             (car (xml-get-children abap-node 'values))
                             'DATA))
                       'LOCK_HANDLE))))) root-node)
      ;; Request lock failed
      (let ((error-message
             ((lambda (exception-node)
                (car (last
                      (car (xml-get-children exception-node 'localizedMessage)))))
              root-node)))
        (error  error-message)))))

;; (defun abaplib-core-lock-get-handle()
;;   (unless abaplib--lock-handle
;;     (abaplib-core-lock))
;;   abaplib--lock-handle
;;   )

(defun abaplib-core-unlock-async (uri lock-handle)
  (let ((prog-name (abaplib-object-get-name)))
    (abaplib--rest-api-call
     (abaplib-get-project-api-url uri)
     (lambda (&rest response)
       (message "Unlocked."))
     :type "POST"
     :headers `(("X-sap-adt-sessiontype" . "stateless"))
     :params `(("lockHandle" . ,lock-handle)))))

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
                              text)))))

    (if output-log
        (abaplib-util-log-buf-write output-log))

    (cond ((string= severity-level "I")
           (message "Activation successful"))
          ((string= severity-level "W")
           (message "Activation successful with `warnings'"))
          ((string= severity-level "E")
           (progn
             (message "Activation failed with `errors'")
             (abaplib-util-log-buf-pop))))))

(defun abaplib-core-activate-post(adtcore-name adtcore-uri)
  (let ((post-body (abaplib-core-activate-template adtcore-name adtcore-uri)))
    (abaplib--rest-api-call
     (abaplib-get-project-api-url "/sap/bc/adt/activation")
     (lambda (&rest rest)
       (let* ((messages (xml-get-children (cl-getf rest :data) 'msg)))
         (abaplib-core-activate-show-message messages)))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :params '((method . activate)
               (preauditRequested . true))
     :data post-body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-core--retrieve-properties ()
  "Retrieve class metadata from server"
  (let* ((etag (abaplib-class--get-property 'metadata-etag nil))
         (class-name abaplib-class--name)
         (url (abaplib-get-project-api-url abaplib-class--uri))
         (data (abaplib--rest-api-call url
                                       nil
                                       :parser 'abaplib-util-xml-parser)))
    (abaplib-class--set-properties (abaplib-class--parse-metadata data))))

(provide 'abaplib-core)
;;; abaplib.el ends here
