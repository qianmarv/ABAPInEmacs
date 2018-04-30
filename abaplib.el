;;; abaplib.el --- sap abap server lib          -*- lexical-binding: t; -*-

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

;; (eval-when-compile (require 'cl))
(require 'request)
(require 'xml)


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

;; NOT Used Yet
(defvar abap-log-level nil
  "Logging level for abap
   One of value `debug'")

(defvar abaplib--login-token-cache nil
  "ABAP token used for authentication.")

(defvar abaplib--sap-client-cache nil
  "ABAP system logon client")

(defvar abaplib--login-last-time nil
  "Last login timestamp")

;; (defconst abaplib--not-a-type "ZZZZ")

;; (defconst abaplib-property-file ".property.json")

;; (defconst abaplib--supported-type  '(PROG CLAS DCLS DDLS ZZZZ)
;;   "Supported ABAP Development Object Type")

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

(defvar-local abaplib--abap-object-properties nil
  "ABAP Object Properties")

;; (defvar-local abaplib--lock-handle nil
;;   "Lock Handle")

(defconst abaplib--log-buffer "*ABAP Log*"
  "ABAP log buffer")

(defconst abaplib--uri-login "/sap/bc/adt/core/discovery")

(defconst abaplib--property-file ".properties.json")


;;==============================================================================
;; Module - Project
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


(defun abaplib-add-server-to-project (project server)
  (abaplib-project-set-property project (cons 'server server)))

(defun abaplib-get-project-path ()
  "Get project path"
  (abaplib-project-get-property 'path))

;;==============================================================================
;; Module - Authentication
;;==============================================================================

(defun abaplib-auth-login-with-token(project login-token sap-client)
  "Login into ABAP Server with token"
  (let ((url ;; "http://ldcier9.wdf.sap.corp:50000/sap/bc/adt/core/discovery"
         (abaplib-get-project-api-url abaplib--uri-login))
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

(defun abaplib-get-csrf-token ()
  (let* ((login-token (cons "Authorization" (abaplib-get-login-token)))
         (sap-client (abaplib-get-sap-client))
         (login-url (abaplib-get-project-api-url abaplib--uri-login))
         (response (request
                    login-url
                    :sync t
                    :headers (list (cons "X-CSRF-Token" "Fetch") login-token)
                    :params `((sap-client . ,sap-client)))))
    (request-response-header response "x-csrf-token")))


;;==============================================================================
;; Module - Tools & Utilities
;;==============================================================================
(defun abaplib-util-current-dir ()
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun abaplib-util-xml-parser()
  (libxml-parse-xml-region (point-min) (point-max)))

(defun abaplib-util-sourcecode-parser()
  (progn
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))
    (buffer-string)))

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

(defun abaplib-util-jsonize-to-file(list file)
  (write-region (with-temp-buffer
                  (insert (json-encode list))
                  (json-pretty-print (point-min) (point-max))
                  (message (buffer-string))
                  (buffer-string)) nil file))



(defun abaplib-util-goto-position (line column)
  (goto-char (point-min))
  (forward-line (- line 1))
  (move-to-column column))

(defun abaplib--get-local-properties ()
  "Load property file on current directory for current buffer"
  (let ((property-file (expand-file-name abaplib--property-file)))
    ;; Ensure propert file exist
    (unless (file-exists-p property-file)
      (error "Missing property file, please user `search' to retrieve again!"))
    (setq abaplib--abap-object-properties (json-read-file property-file))))

(defun abaplib-get-property (name &optional source_name)
  (unless abaplib--abap-object-properties
    (abaplib--get-local-properties))
  (if source_name
      (let* ((sources (alist-get 'sources abaplib--abap-object-properties))
             (source-properties (alist-get (intern source_name) sources)))
        (alist-get name source-properties))
    (alist-get name abaplib--abap-object-properties)))

(defun abaplib-get-path (type &optional extra-directory)
  (let* ((--dir-source-code "Source Code Library")
         (type-list (split-string type "/"))
         (major-type (intern (car type-list)))
         (minor-type (intern (nth 1 type-list)))
         (parent-directory)
         (sub-directory)
         (final-directory))

    (case major-type
      ('CLAS (progn
               (setq parent-directory --dir-source-code)
               (setq sub-directory    "Classes" )))
      ('PROG (progn
               (setq parent-directory --dir-source-code)
               (setq sub-directory "Programs" ))))

    (let* ((parent-path (expand-file-name parent-directory
                                          (abaplib-get-project-path)))
           (sub-path (expand-file-name sub-directory parent-path)))
      (unless (file-exists-p parent-path)
        (make-directory parent-path))
      (unless (file-exists-p sub-path)
        (make-directory sub-path))
      (if extra-directory
          (let ((extra-path (expand-file-name extra-directory sub-path)))
            (unless (file-exists-p extra-path)
              (make-directory extra-path))
            extra-path)
        sub-path))))

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
        (let ((csrf-token (abaplib-get-csrf-token)))
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
;; Module - Core Services - Search
;;==============================================================================
(defun abaplib-do-search (query-string)
  "Search ABAP objects in server in synchronouse call"
  (let* ((search-uri "/sap/bc/adt/repository/informationsystem/search")
         (params `((operation . "quickSearch")
                   (query . ,(concat query-string "*"))
                   (maxResults . ,abap-search-list-max-results)))
         (data (abaplib--rest-api-call search-uri
                                       nil
                                       :params params
                                       :parser 'abaplib-util-xml-parser))
         (object-list (xml-get-children data 'objectReference)))
    object-list))


;;==============================================================================
;; Module - Core Services - Syntax Check
;;==============================================================================
(defun abaplib-do-check(version uri source-uri source-code)
  "Check syntax for program source
  TODO check whether source changed since last retrieved from server
       Not necessary to send the source code to server if no change."
  (let ((chkrun-uri (concat uri "/" source-uri))
        (chkrun-content (base64-encode-string source-code)))
    (abaplib--check-post version uri chkrun-uri chkrun-content)))

(defun abaplib--check-template (adtcore-uri chkrun-uri version chkrun-content)
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

(defun abaplib--check-post (version adtcore-uri chkrun-uri chkrun-content)
  (let ((check-uri "/sap/bc/adt/checkruns")
        (post-data (abaplib--check-template adtcore-uri chkrun-uri version chkrun-content)))
    (abaplib--rest-api-call
      check-uri
     (lambda (&rest rest)
       (let* ((check-report (xml-get-children (cl-getf rest :data) 'checkReport))
              (message-list (xml-get-children (car check-report) 'checkMessageList))
              (messages (xml-get-children (car message-list) 'checkMessage)))
         (abaplib-check-show-message messages)))
     :parser 'abaplib-util-xml-parser
     :type "POST"
     :data post-data
     :params '((reporters . abapCheckRun))
     :headers `(("Content-Type" . "application/vnd.sap.adt.checkobjects+xml")))))


(defun abaplib--check-render-type-text(type)
  (cond ((string= type "E") (propertize "Error"       'face '(bold (:foreground "red"))))
        ((string= type "W") (propertize "Warning"     'face '(bold (:foreground "orange"))))
        ((string= type "I") (propertize "Information" 'face '(bold (:foreground "green"))))
        (t "Other")))

(defun abaplib--check-render-pos(position &optional target-buffer)
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

(defun abaplib-check-show-message (messages)
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
                      (concat (format "[%s] " (abaplib--check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib--check-render-pos position))
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

;;==============================================================================
;; Module - Core Services - Lock
;;==============================================================================
(defun abaplib--lock-sync(uri csrf-token)
  (let* ((root-node (abaplib--rest-api-call
                     uri
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

;; Seems not required as next call of lock will automatically do the unlock
(defun abaplib-unlock-async (uri lock-handle)
  (let ((prog-name (abaplib-object-get-name)))
    (abaplib--rest-api-call
     uri
     (lambda (&rest response)
       (message "Unlocked."))
     :type "POST"
     :headers `(("X-sap-adt-sessiontype" . "stateless"))
     :params `(("lockHandle" . ,lock-handle)))))

;;========================================================================
;; Module - Core Services - Activate Server Side Source
;;========================================================================
(defun abaplib-do-activate(name uri)
  (abaplib--activate-post name uri))

(defun abaplib--activate-post(adtcore-name adtcore-uri)
  (let* ((preaudit-result (abaplib--activate-preaudit adtcore-name adtcore-uri))
         (result-type (car preaudit-result)))
    (case result-type
      ('messages (abaplib-activate-show-message (xml-get-childre preaudit-result
                                                                 'msg)))
      ('inactiveObjects (abaplib--activate-postaudit (xml-get-children preaudit-result
                                                                       'entry)))
      (t (message "succeed.")))))

(defun abaplib--activate-preaudit (adtcore-name adtcore-uri)
  (let ((activate-uri "/sap/bc/adt/activation")
        (post-body (abaplib--activate-preaudit-template adtcore-name adtcore-uri)))
    (abaplib--rest-api-call activate-uri
                            nil
                            :parser 'abaplib-util-xml-parser
                            :type "POST"
                            :params '((method . activate)
                                      (preauditRequested . true))
                            :data post-body)))

(defun abaplib--activate-preaudit-template (adtcore-name adtcore-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
   "<adtcore:objectReferences xmlns:adtcore=\"http://www.sap.com/adt/core\">"
   (format "<adtcore:objectReference adtcore:name=\"%s\" adtcore:uri=\"%s\"/>"
           adtcore-name
           adtcore-uri)
   "</adtcore:objectReferences>"))

(defun abaplib--activate-postaudit (inactive-objects)
  (let ((activate-uri "/sap/bc/adt/activation")
        (post-body (abaplib--activate-postaudit-template inactive-objects)))
    (abaplib--rest-api-call activate-uri
                            (lambda (&rest rest)
                              (message "succeed."))
                            :parser 'abaplib-util-xml-parser
                            :type "POST"
                            :params '((method . activate)
                                      (preauditRequested . false))
                            :data post-body)))

(defun abaplib--activate-postaudit-template (inactive-objects)
  (let ((post-xml ""))
    (setq post-xml (concat post-xml
                           "<?xml version=\"1.0\" encoding=\"UTF-8\"?><adtcore:objectReferences xmlns:adtcore=\"http://www.sap.com/adt/core\">"))
    (dolist (entry inactive-objects)
      (let* ((object (car (xml-get-children entry 'object)))
             (object-ref (car (xml-get-children object 'ref))))
        (when object-ref
          (let ((name (xml-get-attribute object-ref 'name))
                (type (xml-get-attribute object-ref 'type))
                (uri (xml-get-attribute object-ref 'uri))
                (parent-uri (xml-get-attribute object-ref 'parentUri))
                (object-ref-str))
            (if (string= parent-uri "")
                (setq object-ref-str
                      (format "<adtcore:objectReference adtcore:uri=\"%s\" adtcore:type=\"%s\" adtcore:name=\"%s\"/>" uri type name ))
              (setq object-ref-str
                    (format "<adtcore:objectReference adtcore:uri=\"%s\" adtcore:type=\"%s\" adtcore:name=\"%s\" adtcore:parentUri=\"%s\"/>" uri type name parent-uri)))
            (setq post-xml (concat post-xml object-ref-str))))))
    (setq post-xml (concat post-xml
                           "</adtcore:objectReferences>"))
    post-xml))

(defun abaplib-activate-show-message (messages)
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
                      (concat (format "[%s] " (abaplib--check-render-type-text type))
                              (format "at position (%s): "
                                      (abaplib--check-render-pos position))
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

;;========================================================================
;; Module - Core Services - Handle Change Request
;;========================================================================
(defun abaplib-retrieve-trans-request (full-source-uri)
  "Check and retrieve transport request"
  (let* ((transcheck-uri "/sap/bc/adt/cts/transportchecks")
         (post_data (abaplib--transport-check-template full-source-uri))
         (xml-root (abaplib--rest-api-call
                    transcheck-uri
                    nil
                    :type "POST"
                    :data post_data
                    :headers `(("Content-Type" . "application/vnd.sap.as+xml"))
                    :parser 'abaplib-util-xml-parser))
         (value-node (car (xml-get-children xml-root 'values)))
         (data-node (car (xml-get-children value-node 'DATA)))
         (req-node (car (xml-get-children data-node 'REQUESTS)))
         (requests (xml-get-children req-node 'CTS_REQUEST)))
    (mapcar (lambda (request)
              (let* ((req_header (car (xml-get-children request 'REQ_HEADER)))
                     (tr-number (car (last (car (xml-get-children req_header 'TRKORR)))))
                     (text (car (last (car (xml-get-children req_header 'AS4TEXT))))))
                (format "%s | %s" tr-number text)))
            requests)))

(defun abaplib--transport-check-template (full-source-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?><asx:abap xmlns:asx=\"http://www.sap.com/abapxml\" version=\"1.0\">"
   "<asx:values>"
   "<DATA>"
   "<PGMID/>"
   "<OBJECT/>"
   "<OBJECTNAME/>"
   "<DEVCLASS/>"
   "<OPERATION/>"
   "<URI>" full-source-uri "</URI>"
   "</DATA>"
   "</asx:values>"
   "</asx:abap>"))

(defun abaplib-post-cm-checkrun (tr-number full-source-uri)
  (message "Debug==> tr-number: %s" tr-number)
  (let* ((cm-checkrun-uri "/sap/bc/adt/solutionmanager/cm/checkruns")
         (post_data (abaplib--solutionmanager-check-template tr-number full-source-uri))
         (xml-root (abaplib--rest-api-call
                    cm-checkrun-uri
                    nil
                    :type "POST"
                    :data post_data
                    :headers `(("Content-Type" . "application/vnd.sap.adt.cts.solman.checkobjects.v1+xml"))
                    :parser 'abaplib-util-xml-parser))
         (status-text (xml-get-attribute xml-root 'statusText)))))

(defun abaplib--solutionmanager-check-template (tr-number full-source-uri)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?><csol:checkObjects xmlns:csol=\"http://www.sap.com/solman/check\">"
   "<csol:trRequest>" tr-number "</csol:trRequest>"
   "<csol:checkObject operation=\"Change\" uri=\"" full-source-uri "\"/>"
   "</csol:checkObjects>"))
;;========================================================================
;; Module - Core Services - Retrieve Metadata & Source Code
;;========================================================================
(defun abaplib-do-retrieve (name type uri &optional source-name)
  " Retrieve ABAP object"
  ;; 1. Retrieve metadata from uri -- common
  ;; 2. Parse metadata -- specific
  ;; 3. Save properties & Old properties -- common
  ;; 4. Parse source part --> source type/uri/etag -- specific
  ;; 5. Retrieve source
  ;; TODO Preserver previous Etag Etag
  (let* ((object-path (abaplib-get-path type name))
         (property-file (expand-file-name abaplib--property-file object-path))
         (properties (abaplib--retrieve-metadata uri type property-file))
         (sources (alist-get 'sources properties)))
    (mapc (lambda (source-property)
            (let* ((local-source-name (car source-property))
                   (source-uri (alist-get 'source-uri source-property))
                   (full-source-uri (concat uri "/" source-uri))
                   (etag nil)
                   (file-path (expand-file-name local-source-name object-path)))
              (when (or (not source-name)
                        (string= local-source-name source-name))
                (abaplib--retrieve-source source-name
                                          full-source-uri
                                          etag
                                          file-path))))
          sources)
    object-path))


(defun abaplib--retrieve-metadata (uri type &optional file-name)
  (let* ((property-file (abaplib-get-path type))
         (major-type (substring type 0 4))
         (metadata-raw (abaplib--rest-api-call uri
                                               nil
                                               :parser 'abaplib-util-xml-parser))
         (impl-func (intern (concat "abaplib-"
                                    (downcase major-type)
                                    "-metadata-parser")))
         (properties (apply impl-func (list metadata-raw))))
    (push (cons 'uri uri) properties)
    (setq abaplib--abap-object-properties properties)
    (when file-name
      (abaplib-util-jsonize-to-file properties file-name))
    properties))

(defun abaplib--retrieve-source (name uri etag &optional file-path)
  (abaplib--rest-api-call
   uri
   (lambda (&rest rest)
     (let ((response-data (cl-getf rest :data))
           (status-code (request-response-status-code (cl-getf rest :response))))
       (if (eq status-code 304)
           (message "Source remain unchanged in server.")
         (if file-path
             (write-region response-data nil file-path)
           (let ((source-buffer (get-buffer-create name)))
             (set-buffer source-buffer)
             (insert response-data)))
         (message "Source retrieved from server and overwrite local."))))
   :parser 'abaplib-util-sourcecode-parser
   :headers (list `("If-None-Match" . ,etag)
                  '("Content-Type" . "plain/text"))))

;;========================================================================
;; Module - Core Services - Submit Source to Server
;;========================================================================
(defun abaplib-do-submit(full-source-uri source-code &optional tr-number)
  "Submit source to server
   TODO Check source in server side if current source was changed based on an old version
   The submission should be cancelled"
  (let* ((csrf-token (abaplib-get-csrf-token))
         (lock-handle (abaplib--lock-sync full-source-uri csrf-token))
         (params `(("lockHandle" . ,lock-handle)))
         (headers `(("Content-Type" . "text/plain")
                    ("x-csrf-token" . ,csrf-token))))
    (when tr-number
      (push `(corrNr . ,tr-number) params))
    (abaplib--rest-api-call
     full-source-uri
     (lambda (&rest rest)
       (let* ((response (cl-getf rest :response))
              (ETag (request-response-header response "ETag")))
         ;;TODO Refresh properties
         (message "Submit source to server succeed.")))
     :type "PUT"
     :data source-code
     :headers headers
     :params params)))

;;========================================================================
;; Module - Core Services - Format Source From Server
;;========================================================================
(defun abaplib-do-format (source-code)
  "Format Source Code"
  (let* ((format-uri "/sap/bc/adt/abapsource/prettyprinter"))
    (abaplib--rest-api-call format-uri
                            nil
                            :type "POST"
                            :data source-code
                            :parser 'abaplib-util-sourcecode-parser)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - ABAP Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun abaplib-clas-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         ;; (type-list (split-string adtcore-type "/"))
         ;; (type (car type-list))
         ;; (subtype (nth 1 type-list))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (includes-node (xml-get-children metadata 'include))
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
      (type . ,adtcore-type)
      ;; (subtype . ,subtype)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module - Object Type Specific - ABAP Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abaplib-prog-metadata-parser (metadata)
  (let* ((adtcore-type (xml-get-attribute metadata 'type))
         ;; (type-list (split-string adtcore-type "/"))
         ;; (type (car type-list))
         ;; (subtype (nth 1 type-list))
         (name (xml-get-attribute metadata 'name))
         (description (xml-get-attribute metadata 'description))
         (version (xml-get-attribute metadata 'version))
         (package-node (car (xml-get-children metadata 'packageRef)))
         (package (xml-get-attribute package-node 'name))
         (file-name "main.prog.abap")
         (includes (list (cons file-name `((version . ,version)
                                           (source-uri . ,(xml-get-attribute metadata 'sourceUri))
                                           (include-type . main)
                                           (type . ,adtcore-type)
                                           (etag . ,nil)
                                           )))))
    `((name . ,name)
      (description . ,description)
      (type . ,adtcore-type)
      ;; (subtype . ,subtype)
      (version . ,version)
      (package . ,package)
      (sources . ,includes))))

(provide 'abaplib)
;;; abaplib.el ends here
