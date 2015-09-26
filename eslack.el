;;; eslack.el --- Slack instant messaging client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience, tools

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

;; The entry point is M-x eslack, which will prompt you for an API
;; developer token to a Slack team.
;;
;; For the moment that is the only way to connect to a Slack team,
;; later on other authentication methods may be devised.
;;
;; To see a list of Slack tokens to the teams you're signed on to,
;; visit https://api.slack.com/web and navigate to the end of the
;; page.

;;; Code:

(require 'websocket)
(require 'url)
(require 'lui)
(require 'tracking)
(require 'json)
(require 'eieio)


;;; Utils
;;;
(defvar eslack--debug nil)

(defun eslack--debug (format-control &rest format-args)
  (display-warning 'eslack (apply #'format (concat "[eslack] " format-control) format-args) :debug))

(defun eslack--message (format-control &rest format-args)
  (message "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--warning (format-control &rest format-args)
  (display-warning 'eslack (apply #'format (concat "[eslack] " format-control) format-args) :warning))

(defun eslack--error (format-control &rest format-args)
  (error "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--get-internal (object keys)
  ""
  (cl-loop for key in keys
           for a = object then (cdr res)
           for res = (progn
                       (unless (listp a)
                         (eslack--error "expected %s to be an alist" a))
                       (assoc key a))
           while key
           unless res
           do (eslack--error "can't find %s in %s" key a)
           finally return res))

(defun eslack--get (object key &rest more)
  "Search OBJECT for the value of KEY.
OBJECT is an JSON alist"
  (cdr (eslack--get-internal object (cons key more))))

(defun eslack--has (object key)
  "Check if OBJECT has KEY"
  (assoc key object))

(gv-define-setter eslack--get (value object &rest more)
  `(setcdr (eslack--get-internal ,object (list ,@more))
           ,value))

(cl-defun eslack--find (prop seq &key (key 'id) (test #'string=))
  "Find PROP in sequence SEQ.
SEQ is a JSON sequence."
  (cl-find prop seq :key (lambda (thing) (eslack--get thing key)) :test test))

(defvar eslack-completing-read-function 'ido-completing-read)

(defun eslack--completing-read (prompt choices &optional
                                       predicate
                                       require-match
                                       initial-input
                                       hist
                                       def
                                       inherit-input-method)
  (funcall eslack-completing-read-function
           prompt
           choices
           predicate
           require-match
           initial-input
           hist
           def
           inherit-input-method))

(defun eslack--keywordize (string)
  (intern (concat ":"
                  (replace-regexp-in-string "_" "-" string))))


;;; Connections
;;; 

;; test, out-of-date by now
;;
;; Team	User	Token	 
;; SISCOG	joaot	xoxp-7449361824-7490502034-7536719025-df3a9e

(defvar eslack--token "xoxp-7449361824-7490502034-7536719025-df3a9e")

(defvar eslack--connections (list)
  "Global list of connections.")

(defvar eslack--dispatching-connection nil
  "Connection used within a lexical scope.
Intended to be let-bound.")
(defvar eslack--buffer-connection nil
  "Connection active in a particular buffer.
Intended to be buffer-local")
(defvar eslack--buffer-room nil
  "Server chatroom active in a particular buffer.
Intended to be buffer-local")
(defvar eslack--last-state nil
  "For debug purposes")

(defclass eslack--connection-object ()
  ((websocket  :initarg :websocket  :accessor eslack--connection-websocket)
   (token :initarg :token :accessor eslack--connection-token)
   (state :initarg :state :accessor eslack--connection-state))
  (:documentation "Represents an eslack connection.
WEBSOCKET is a WEBSOCKET object wrapping the actual Emacs process.
TOKEN is the security token.
STATE is a JSON alist returned by the server on first contact."))

(cl-defun eslack--connection-name (&optional (connection (eslack--connection)))
  (eslack--get (eslack--connection-state connection) 'team 'name))

(defun eslack--prompt-for-connection-maybe ()
  "Prompt user for a connection in `eslack--connections'."
  (cond ((and eslack--connections
              (null (cdr eslack--connections)))
         (first eslack--connections))
        (eslack--connections
         (let ((team-name (eslack--completing-read
                           "[eslack] Connection? "
                           (mapcar #'eslack--connection-name 
                                   eslack--connections)
                           nil t nil nil nil nil)))
           (cl-find team-name eslack--connections
                    :key (lambda (conn)
                           (eslack--get (eslack--connection-state conn)
                                        'team 'name)))))
        (t
         (eslack--error "No connections, start one with `%s'"
                        (substitute-command-keys "\\[eslack]")))))

(eval-and-compile
  (defmacro eslack--define-connection-accessors ()
    `(progn
       ,@(cl-loop for prop in '(users channels groups ims bots)
                  collect `(cl-defun ,(intern (concat "eslack--" (symbol-name prop)))
                               (&optional (connection (eslack--connection)))
                             ,(format "Retrieve CONNECTION's %S" prop)
                             (let ((state (eslack--connection-state connection)))
                               (eslack--get state ',prop))))))

  (eslack--define-connection-accessors))

(defun eslack--connection ()
  "Current connection.
First try `eslack--dispatching-connection', then a buffer's
connection, then the first of the global connection list."
  (or
   eslack--dispatching-connection
   eslack--buffer-connection
   (first eslack--connections)
   (eslack--error "no usable connections")))

(defun eslack-close-all ()
  (interactive)
  (cl-loop for conn in eslack--connections
           do (websocket-close (eslack--connection-websocket conn)))
  (setq eslack--connections nil))

(defun eslack--read-token ()
  (read-from-minibuffer "Token: "
                        (with-temp-buffer
                             (clipboard-yank)
                             (buffer-string))))

(defun eslack (token)
  "Start an eslack connection to a server, identified by TOKEN"
  (interactive (list (eslack--read-token)))
  (url-retrieve
   (format "https://slack.com/api/rtm.start?token=%s"
           token)
   (lambda (status)
     (let ((error (plist-get status :error))
           (redirect (plist-get status :redirect)))
       (when error
         (signal (car error) (cdr error)))
       (when redirect
         (error "slack requests that you try again to %a" redirect))
       (search-forward "\n\n")
       (let ((state (json-read)))
         (setq eslack--last-state state)
         (eslack--start-websockets state token))))))

(defun eslack--opened (connection)
  (eslack--message "Connection %s established!" connection)
  (push connection eslack--connections))

(defun eslack--closed (connection)
  (eslack--message "Connection closed!" connection))

(defun eslack--handle-websocket-error (_connection args)
  (eslack--message "Ooops something went wrong")
  (apply #'websocket-default-error-handler args))

(defvar eslack-log-events t
  "If non-nil log events for each connection into a temporary
  buffer.")

(defun eslack-events-buffer (connection &optional pop-to-buffer)
  "Return or create the eslack event log buffer."
  (interactive (list (eslack--connection) t))
  (let ((buffer (get-buffer-create (format "*eslack events (%s)*"
                             (eslack--connection-name connection)))))
    (when pop-to-buffer
      (pop-to-buffer buffer))
    buffer))

(defun eslack--pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(defun eslack--log-event (event connection)
  "Record the fact that EVENT occurred in PROCESS."
  (when eslack-log-events
    (with-current-buffer (eslack-events-buffer connection)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (eslack--pprint-event event (current-buffer)))
      (goto-char (point-max)))))

(defun eslack--process (connection frame)
  (eslack--debug "a frame %s" frame)
  (let ((payload (json-read-from-string (websocket-frame-payload frame)))
        (eslack--dispatching-connection connection))
    (eslack--log-event payload connection)
    (eslack--event (if (eslack--has payload 'reply_to)
                       :reply-to
                     (eslack--keywordize (eslack--get payload 'type)))
                   (and (eslack--has payload 'subtype)
                        (eslack--keywordize (eslack--get payload 'subtype)))
                   payload)))

(defun eslack--start-websockets (state token)
  (let ((url (eslack--get state 'url))
        (connection nil))
    (websocket-open url 
                    :on-open (lambda (ws)
                               (setq connection (make-instance 'eslack--connection-object
                                                               :websocket ws
                                                               :token token
                                                               :state state))
                               (eslack--opened connection))
                    :on-message (lambda (_ws f)
                                  (eslack--process connection f))
                    :on-close (lambda (_ws)
                                (eslack--closed connection))
                    :on-error (lambda (&rest args)
                                (eslack--handle-websocket-error connection args)))))


;;; Major mode
;;;
(defvar eslack--next-message-id 0)

(define-derived-mode eslack-mode lui-mode "eslack"
  "A major mode for eslack rooms"
  (setq-local left-margin-width 0)
  (setq-local lui-input-function 'eslack--send-message)
  (set-buffer-multibyte t)
  (lui-set-prompt "\n: ")
  (add-hook 'post-self-insert-hook 'eslack--send-typing-indicator-maybe
            'append 'local))


;;; Rooms
;;;
(defun eslack--rooms ()
  (cl-mapcan (lambda (seq) (mapcar #'identity seq))
             (list (eslack--groups) (eslack--channels) (eslack--ims))))

(defun eslack--im-room-p (room)
  (and
   (assoc 'is_im room)
   (eq (eslack--get room 'is_im) t)))

(defun eslack--room-name (room)
  (cond ((eslack--im-room-p room)
         (let* ((id (eslack--get room 'user))
                (user (or (eslack--find id (eslack--users))
                          (eslack--find id (eslack--bots)))))
           (unless user
             (eslack--error "can't find user %s in team state" id))
           (eslack--get user 'name)))
        (t
         (eslack--get room 'name))))

(defun eslack--prompt-for-room ()
  (let* ((rooms (eslack--rooms))
         (room-name (eslack--completing-read
                     "[eslack] Room name? "
                     (mapcar #'eslack--room-name rooms)
                     nil t))
         (room (cl-find room-name rooms :key #'eslack--room-name)))
    room))

(defun eslack--buffer-name (connection room)
  (format "*%s (%s)*"
          (eslack--room-name room)
          (eslack--connection-name connection)))

(defun eslack--call-with-room-buffer (connection room fn)
  (let ((buffer (get-buffer-create (eslack--buffer-name connection room))))
    (with-current-buffer buffer
      (unless (eq major-mode 'eslack-mode)
        (eslack-mode)
        (setq-local eslack--buffer-room room)
        (setq-local eslack--buffer-connection connection))
      (funcall fn))))

(cl-defmacro eslack--with-room-buffer ((connection room) &body body)
  (declare (debug (sexp sexp &rest form))
           (indent 1))
  `(eslack--call-with-room-buffer ,connection ,room (lambda () ,@body)))

(defun eslack-join-room (connection room)
  (interactive (let* ((connection (eslack--prompt-for-connection-maybe)))
                 (list connection
                       (let ((eslack--dispatching-connection connection))
                         (eslack--prompt-for-room)))))
  (eslack--post :channels.join
                `((name . ,(eslack--get room 'name)))
                (lambda (object)
                  (eslack--with-room-buffer (connection room)
                    (pop-to-buffer (current-buffer))))))


;;; More utils
;;;
(defvar eslack--image-cache (make-hash-table :test #'equal))

(defun eslack--insert-image (marker url)
  (cl-flet ((insert-it
             (image marker)
             (with-current-buffer
                 (marker-buffer marker)
               (save-excursion
                 (goto-char marker)
                 (let ((inhibit-read-only t))
                   (insert (propertize "[avatar]" 'display image)))))))
    (let ((res (gethash url eslack--image-cache)))
      (cond ((and (listp res)
                  (eq 'image (car res)))
             (insert-it res marker))
            ((and (consp nil)
                  (cl-every #'markerp res))
             (setcdr res (cons marker (cdr res))))
            (t
             (puthash url (list marker) eslack--image-cache)
             (url-retrieve url
                           (lambda (status)
                             (if (plist-get status :error)
                                 (eslack--warning "cannot get avatar for %s" url)
                               (search-forward "\n\n")
                               (let ((image (create-image (buffer-substring-no-properties
                                                           (point) (point-max))
                                                          nil 'data-p)))
                                 (cl-loop for marker in (gethash url eslack--image-cache)
                                          do (insert-it image marker))
                                 (puthash url image eslack--image-cache))))))))))

(cl-defun eslack--web-request (url method &key
                                   json-params
                                   params
                                   on-success
                                   (on-error (lambda (_sym data)
                                               (eslack--error
                                                "Web API fail %s" data))))
  (let ((url-request-method (upcase (if (keywordp method)
                                        (substring (symbol-name method) 1)
                                      (symbol-name method))))
        (url-request-extra-headers (if json-params
                                       '(("Content-Type" . "application/json"))))
        (url-request-data
         (if json-params
             (json-encode json-params)))
        (url
         (concat url "?"
                 (cl-loop for (avp . rest) on params
                          for (a . v) = avp
                          concat (format "%s" a)
                          concat "="
                          concat (format "%s" v)
                          when rest concat "&"
                          ))))
    (url-retrieve url (lambda (status)
                        (let ((oops (plist-get status :error)))
                          (cond (oops
                                 (funcall on-error status))
                                ((plist-get status :redirect)
                                 (eslack--web-request (plist-get status :redirect) method :json-params params
                                                      :params params
                                                      :on-success on-success
                                                      :on-error on-error))
                                (on-success
                                 (search-forward "\n\n")
                                 (let ((object (json-read)))
                                   (funcall on-success status object)))))))))

(defun eslack--post (method params on-success)
  (eslack--web-request (format "https://slack.com/api/%s"
                               (substring (symbol-name method) 1))
                       :post
                       :params (append `((token . ,(eslack--connection-token (eslack--connection))))
                                       params)
                       :on-success (lambda (_status object)
                                     (cond ((eq (eslack--get object 'ok) :json-false)
                                            (eslack--error "posting to %s returned: %s"
                                                           method
                                                           (eslack--get object 'error)))
                                           (t
                                            (funcall on-success object))))))


;;; Event processing
;;;
(cl-defgeneric eslack--event (type message))

(cl-defmethod eslack--event ((_type (eql :bla)) _subtype _message)
  "yo")

(cl-defmethod eslack--event ((_type (eql :hello)) _subtype _message)
  (eslack--message "%s says hello!" (eslack--connection-name)))

(cl-defmethod eslack--event ((_type (eql :message)) (subtype (eql nil)) message)
  (let ((room (eslack--find (eslack--get message 'channel) (eslack--rooms))))
    (eslack--with-room-buffer ((eslack--connection) room)
      ;; (pop-to-buffer (current-buffer))
      (tracking-add-buffer (current-buffer))
      (let ((user (eslack--find (eslack--get message 'user) (eslack--users)))
            (avatar-marker (copy-marker lui-output-marker)))
        (set-marker-insertion-type avatar-marker nil)
        (lui-insert (propertize
                     (format "%s: %s"
                             (propertize (eslack--get user 'name)
                                         'eslack--user user)
                             (decode-coding-string (eslack--get message 'text) 'utf-8))
                     'eslack--message message))
        (eslack--insert-image avatar-marker (eslack--get user 'profile 'image_24))))))

(cl-defmethod eslack--event ((_type (eql :message)) subtype message)
  (eslack--debug "subtype %s of message is unimplemented: %s" subtype message))

(cl-defmethod eslack--event ((_type (eql :user-typing)) _subtype message)
  "A channel member is typing a message"
  (when eslack--buffer-room
    (let ((room (eslack--find (eslack--get message 'channel) (eslack--rooms)))
          (user (eslack--find (eslack--get message 'user) (eslack--users))))
      (when (eq room eslack--buffer-room)
        (eslack--message "%s is typing" (eslack--get user 'name))))))

(cl-defmethod eslack--event ((type (eql :channel-marked)) _subtype message)
  "Your channel read marker was updated"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-created)) _subtype message)
  "A team channel was created"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-joined)) _subtype message)
  "You joined a channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-left)) _subtype message)
  "You left a channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-deleted)) _subtype message)
  "A team channel was deleted"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-rename)) _subtype message)
  "A team channel was renamed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-archive)) _subtype message)
  "A team channel was archived"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-unarchive)) _subtype message)
  "A team channel was unarchived"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :channel-history-changed)) _subtype message)
  "Bulk updates were made to a channel's history"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :im-created)) _subtype message)
  "A direct message channel was created"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :im-open)) _subtype message)
  "You opened a direct message channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :im-close)) _subtype message)
  "You closed a direct message channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :im-marked)) _subtype message)
  "A direct message read marker was updated"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :im-history-changed)) _subtype message)
  "Bulk updates were made to a DM channel's history"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-joined)) _subtype message)
  "You joined a private group"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-left)) _subtype message)
  "You left a private group"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-open)) _subtype message)
  "You opened a group channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-close)) _subtype message)
  "You closed a group channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-archive)) _subtype message)
  "A private group was archived"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-unarchive)) _subtype message)
  "A private group was unarchived"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-rename)) _subtype message)
  "A private group was renamed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-marked)) _subtype message)
  "A private group read marker was updated"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :group-history-changed)) _subtype message)
  "Bulk updates were made to a group's history"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-created)) _subtype message)
  "A file was created"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-shared)) _subtype message)
  "A file was shared"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-unshared)) _subtype message)
  "A file was unshared"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-public)) _subtype message)
  "A file was made public"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-private)) _subtype message)
  "A file was made private"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-change)) _subtype message)
  "A file was changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-deleted)) _subtype message)
  "A file was deleted"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-comment-added)) _subtype message)
  "A file comment was added"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-comment-edited)) _subtype message)
  "A file comment was edited"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :file-comment-deleted)) _subtype message)
  "A file comment was deleted"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :pin-added)) _subtype message)
  "A pin was added to a channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :pin-removed)) _subtype message)
  "A pin was removed from a channel"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :presence-change)) _subtype message)
  "A team member's presence changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :manual-presence-change)) _subtype message)
  "You manually updated your presence"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :pref-change)) _subtype message)
  "You have updated your preferences"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :user-change)) _subtype message)
  "A team member's data has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-join)) _subtype message)
  "A new team member has joined"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :star-added)) _subtype message)
  "A team member has starred an item"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :star-removed)) _subtype message)
  "A team member removed a star"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :reaction-added)) _subtype message)
  "A team member has added an emoji reaction to an item"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :reaction-removed)) _subtype message)
  "A team member removed an emoji reaction"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :emoji-changed)) _subtype message)
  "A team custom emoji has been added or changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :commands-changed)) _subtype message)
  "A team slash command has been added or changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-plan-change)) _subtype message)
  "The team billing plan has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-pref-change)) _subtype message)
  "A team preference has been updated"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-rename)) _subtype message)
  "The team name has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-domain-change)) _subtype message)
  "The team domain has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :email-domain-changed)) _subtype message)
  "The team email domain has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :bot-added)) _subtype message)
  "An integration bot was added"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :bot-changed)) _subtype message)
  "An integration bot was changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :accounts-changed)) _subtype message)
  "The list of accounts a user is signed into has changed"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((type (eql :team-migration-started)) _subtype message)
  "The team is being migrated between servers"
  (eslack--debug "%s is unimplemented: %s" type message))


;;; Sending texts and other stuff
;;;
(defface eslack-pending-message-face
  '((t (:inherit font-lock-doc-face)))
  "Used for messages not yet acknowledged"
  :group 'eslack)

(defface eslack-own-message-face
  '((t (:inherit default)))
  "Face used for sent messages"
  :group 'eslack)

(defvar eslack--awayting-reply (make-hash-table)
  "A hash table integer -> sent message")

(defun eslack--send-message (text)
  (let* ((id (cl-incf eslack--next-message-id))
         (message `((:text . ,text)
                    (:id . ,id)
                    (:channel . ,(eslack--get eslack--buffer-room 'id))
                    (:type . :message)))
         (start (copy-marker lui-output-marker))
         end)
    (set-marker-insertion-type lui-output-marker nil)
    (lui-insert (propertize
                     (format "%s: %s"
                             "me"
                             text)
                     'eslack--message message
                     'face 'eslack-pending-message-face))
    (setq end (copy-marker lui-output-marker))
    (puthash id (list message start end) eslack--awayting-reply)
    (websocket-send-text (eslack--connection-websocket (eslack--connection))
                         (json-encode message))))

(defvar eslack--last-typing-indicator-timestamp (current-time))

(defun eslack--send-typing-indicator-maybe ()
  (unless (< (time-to-seconds
              (time-since
               eslack--last-typing-indicator-timestamp))
             3)
    (let* ((id (cl-incf eslack--next-message-id))
           (message `((:id . ,id)
                      (:channel . ,(eslack--get eslack--buffer-room 'id))
                      (:type . :typing))))
      (websocket-send-text (eslack--connection-websocket (eslack--connection))
                           (json-encode message))
      (setq-local eslack--last-typing-indicator-timestamp (current-time)))))

(defun eslack--property-regions (beg end property &optional predicate)
  "Search BEG and END for subregions with text property PROPERTY.
Returns a list of pairs ((SUBREGION-BEG SUBREGION-END) ...) for
the subregions wherein the property value verifies
PREDICATE. PREDICATE defaults to `identity'"
  (cl-loop with predicate = (or predicate
                                #'identity)
           for start = beg then (next-single-property-change next property nil end)
           for start-value = (get-text-property start property)
           for next = (and start
                           (if (funcall predicate start-value)
                               (next-single-property-change start property nil end)
                             start))
           for next-value = (get-text-property next property)
           while (and start next (< start end))
           when (and (funcall predicate start-value)
                     (not (funcall predicate next-value)))
           collect (list start next)))

(cl-defmethod eslack--event ((_type (eql :reply-to)) _subtype message)
  "Handle Slack's confirmation to a sent message.
The `reply-to' type doesn't really exist in the Slack API, this
particular method is hack, albeit a pacific one."
  (let* ((id (eslack--get message 'reply_to))
         (probe (gethash id eslack--awayting-reply)))
    (if probe
        (cl-destructuring-bind (_message start end)
            probe
          (with-current-buffer (marker-buffer start)
            (let ((inhibit-read-only t))
              (cl-loop for (start end) in (eslack--property-regions start end 'eslack--message)
                       do (add-text-properties start end '(face eslack-own-message-face)))
              (remhash id eslack--awayting-reply))))
      (eslack--error "Reply for unknown sent message"))))



(provide 'eslack)
;;; eslack.el ends here

: 
