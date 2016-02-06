;;; eslack.el --- Slack instant messaging client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  João Távora

;; Version: 0.1
;; Author: João Távora <joaotavora@gmail.com>
;; Package-Requires: ((circe) (websocket) (markdown-mode))
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
(require 'markdown-mode)
(require 'url)
(require 'lui)
(require 'tracking)
(require 'json)
(require 'eieio)
(require 'cl-lib)
(require 'hi-lock)


;;; Utils
;;;
(defvar eslack--debug nil)

(defun eslack--debug (format-control &rest format-args)
  (display-warning 'eslack (apply #'format (concat "[eslack] " format-control) format-args) :debug))

(defun eslack--message (format-control &rest format-args)
  (substitute-command-keys
   (message "%s" (apply #'format (concat "[eslack] " format-control) format-args))))

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

(defun eslack--put (object key value)
  "Put a KEY-VALUE pair in OBJECT.
Don't invalidate other references to object."
  (let* ((existing (assoc key object))
         (temp (and (not existing)
                    (cons (car object) (cdr object)))))
    (if existing
        (setcdr existing value)
      (setcdr object temp)
      (setcar object (cons key value)))))

(defun eslack--get (object key &rest more)
  "Search OBJECT for the value of KEY.
OBJECT is an JSON alist"
  (cdr (eslack--get-internal object (cons key more))))

(defun eslack--has (object key)
  "Check if OBJECT has KEY"
  (assoc key object))

(defun eslack--merge (object new-object)
  "Update OBJECT with NEW-OBJECT properties,
but keep any OBJECT's properties that new-object doesn't have.
Also don't invalidate any references to object"
  (let ((extra (cl-loop for (key . value) in object
                        unless (eslack--has new-object key)
                        collect (cons key value))))
  (setcar object (car new-object))
  (setcdr object (append (cdr new-object)
                         extra))
  object))

;; Promising, but not used
;; 
;; (gv-define-setter eslack--get (value object &rest more)
;;   `(setcdr (eslack--get-internal ,object (list ,@more))
;;            ,value))

(cl-defun eslack--find (prop seq &key (key 'id) (test #'string=))
  "Find PROP in sequence SEQ.
SEQ is a JSON sequence."
  (cl-find prop seq :key (lambda (thing) (eslack--get thing key)) :test test))

(cl-defun eslack--find-message (what &key (key 'ts) (test #'string=))
  "Find a message containing WHAT in a buffer by KEY.
KEY defaults to the 'ts."
  (cl-loop for start = (point-min) then next
           for probe = (get-text-property start 'eslack--message)
           for next = (next-single-property-change start 'eslack--message)
           while next
           for prop = (ignore-errors (eslack--get probe key))
           when (and probe
                     (funcall test what prop))
           return probe))

(defun eslack--find-channel-buffer (channel-or-group-or-im)
  (let ((room (eslack--find channel-or-group-or-im
                            (eslack--rooms))))
    (unless room
      (eslack--warning "Hey can't find a room for %s"
                       channel-or-group-or-im))
    (get-buffer
     (eslack--buffer-name (eslack--connection)
                          room))))

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

(cl-defun eslack--flash-region (start end
                                      &key (interval 0.1)
                                      (face 'highlight)
                                      (times 1)
                                      finally)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end))
        (buffer (current-buffer)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'priority 1000)
    (run-with-timer
     interval nil
     (lambda ()
       (with-current-buffer buffer
         (delete-overlay overlay)
         (if (> times 1)
             (run-with-timer
              interval nil
              (lambda ()
                (with-current-buffer buffer
                  (eslack--flash-region start end
                                        :interval interval
                                        :face face
                                        :times (1- times)
                                        :finally finally))))
           (when finally
             (funcall finally))))))))


;;; Connections
;;; 
(defvar eslack--connections (list)
  "Global list of connections.")

(defvar eslack--dispatching-connection nil
  "Connection used within a lexical scope.
Intended to be let-bound.")

(defvar eslack--buffer-connection nil
  "Connection active in a particular buffer.
Intended to be buffer-local")

(defvar eslack--default-connection nil
  "Default connection to use when no dispatching or buffer
  connection." )

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

(defun eslack--connection-live-p (connection)
  ;; fixme: still not spectacularly implemented
  (let ((openp (websocket-openp (eslack--connection-websocket connection))))
    (when openp 
      (prog1 t
        (cl-assert (memq connection eslack--connections))))))

(cl-defmacro eslack--checking-connection ((connection) &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  `(progn
     (let* ((fn (lambda () ,@body))
            (use-this (lambda (connection)
                        (if eslack--buffer-connection
                            (setq-local eslack--buffer-connection connection))
                        (if eslack--dispatching-connection
                            (setq eslack--dispatching-connection connection))))
            (buffer (current-buffer))
            (deadp (not (eslack--connection-live-p ,connection)))
            (reusable
             (let ((candidate
                    (cl-find ,connection
                             (cl-remove ,connection eslack--connections)
                             :test #'eslack--connection-equal)))
               (if (and candidate
                        (eslack--connection-live-p candidate))
                   candidate))))
       (cond ((and deadp
                   reusable)
              (eslack--message "Auto-switching buffer connection to live %s" (eslack--connection-name reusable))
              (funcall use-this reusable)
              (funcall fn))
             ((and deadp
                   (y-or-n-p (format "%s is dead. Try re-connecting?"
                                     (eslack--connection-name ,connection))))
              (eslack (eslack--connection-token ,connection)
                      (lambda (connection)
                        (with-current-buffer buffer
                          (funcall use-this connection)
                          (funcall fn)
                          (eslack--message "Reconnected to %s."
                                           (eslack--connection-name connection))))))
             (deadp
              (eslack--message "%s is dead. Use \\[eslack] to start a new connection..."
                               (eslack--connection-name ,connection)))
             (t
              (funcall fn))))))

(defun eslack--specific-symbol-p (key)
  (string-match "^eslack-" (symbol-name key)))

(defun eslack--websocket-send (connection message)
  (let ((stripped (cl-loop for (key . value) in message
                           unless (eslack--specific-symbol-p key)
                           collect (cons key value))))
    (eslack--log-event stripped connection :outgoing-wss)
    (websocket-send-text (eslack--connection-websocket connection)
                         (json-encode stripped))))

(defun eslack-list-connections ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "*eslack-connections*")
    (let ((p (point))
          (inhibit-read-only t))
      (erase-buffer)
      (tabulated-list-mode)
      (set (make-local-variable 'tabulated-list-format)
       `[("Default" 8) ("Name" 24) ("Token" 24) ("Live" 8 t)])
      (tabulated-list-init-header)
      (set (make-local-variable 'tabulated-list-entries)
           (mapcar
            (lambda (conn)
              (list conn
                    `[,(if (eq eslack--default-connection conn) "*" " ")
                      ,(eslack--connection-name conn)
                      ,(eslack--connection-token conn)
                      ,(if (eslack--connection-live-p conn) "yes" "no")
                      ]))
            eslack--connections))
      (tabulated-list-print)
      (goto-char p)
      (pop-to-buffer (current-buffer)))))

(defmacro eslack--define-connection-accessors ()
  `(progn
     ,@(cl-loop for prop in '(self users channels groups ims bots)
                collect `(cl-defun ,(intern (concat "eslack--" (symbol-name prop)))
                             (&optional (connection (eslack--connection)))
                           ,(format "Retrieve CONNECTION's %S" prop)
                           (let ((state (eslack--connection-state connection)))
                             (eslack--get state ',prop))))))

(eslack--define-connection-accessors)

(defun eslack--connection ()
  "Current connection.
First try `eslack--dispatching-connection', then a buffer's
connection, then the first of the global connection list."
  (or
   eslack--dispatching-connection
   eslack--buffer-connection
   eslack--default-connection
   (eslack--error "no usable connections")))

(defun eslack-close-all ()
  (interactive)
  (cl-loop for conn in eslack--connections
           do (websocket-close (eslack--connection-websocket conn))))

(defun eslack--read-token ()
  (read-from-minibuffer "Token: "
                        (with-temp-buffer
                             (clipboard-yank)
                             (buffer-string))))

(defun eslack (token &optional continuation)
  "Start an eslack connection to a server, identified by TOKEN.
Non-interactively, continuation is a function of a single
argument, an `eslack--connection' called when everything goes OK."
  (interactive (list (eslack--read-token)
                     (lambda (connection)
                       (eslack--message "Connected to %s.  Use \\[eslack-enter-room] to start chatting"
                                        (eslack--connection-name connection)))))
  (let ((sig (cl-gensym "eslack--")))
    (eslack--log-event `(:connection-attempt
                         .
                         ((:token . ,token)
                          (:sig . ,sig)))
                       token
                       :outgoing-attempt)
    (url-retrieve
     (format "https://slack.com/api/rtm.start?token=%s"
             token)
     (lambda (status)
       (eslack--log-event `((:sig . ,sig)
                            (:http-status . ,status))
                          token
                          :incoming-http)
       (let ((error (plist-get status :error))
             (redirect (plist-get status :redirect)))
         (when error
           (signal (car error) (cdr error)))
         (when redirect
           (error "slack requests that you try again to %a" redirect))
         (search-forward "\n\n")
         (let ((state (json-read)))
           (setq eslack--last-state state)
           (eslack--start-websockets state token continuation)))))))

(defun eslack--opened (connection)
  (eslack--debug "Connection to %s established" (eslack--connection-name connection))
  (when eslack--default-connection
    (eslack--warning "%s is now the new default connection" (eslack--connection-name connection)))
  (setq eslack--default-connection connection)
  (push connection eslack--connections))

(defun eslack--closed (connection)
  (eslack--message "Connection to %s closed" (eslack--connection-name connection))
  (setq eslack--connections
        (delete connection eslack--connections))
  (when (eq eslack--default-connection connection)
    (setq eslack--default-connection nil)))

(defun eslack--handle-websocket-error (_connection args)
  (eslack--warning "Ooops something went wrong")
  (apply #'websocket-default-error-handler args))

(defvar eslack-log-events t
  "If non-nil log events for each connection into a temporary
  buffer.")

(defvar eslack--events-buffer-token nil)

(defun eslack-events-buffer (connection-or-token &optional pop-to-buffer)
  "Return or create the eslack event log buffer."
  (interactive (list (eslack--connection) t))
  (let* ((connection (and (eslack--connection-object-p connection-or-token)
                          connection-or-token))
         (token (if connection
                    (eslack--connection-token connection)
                  connection-or-token))
         (buffer (cl-find token (buffer-list)
                         :test (lambda (token buffer)
                                 (with-current-buffer buffer
                                   (and (boundp 'eslack--events-buffer-token)
                                        (string= eslack--events-buffer-token
                                                 token)))))))
    (unless buffer
      (setq buffer
            (with-current-buffer (generate-new-buffer "*eslack events*")
              (emacs-lisp-mode) ; fixme: perhaps use some other mode
              (setq-local eslack--events-buffer-token token)
              (current-buffer))))
    (with-current-buffer buffer
      (when (and connection
                 (not eslack--buffer-connection))
        (setq-local eslack--buffer-connection connection)
        (rename-buffer (format "*eslack events (%s)*" (eslack--connection-name connection)) 'unique)))
    (when pop-to-buffer
      (pop-to-buffer buffer))
    buffer))

(defun eslack--pprint-event (event buffer)
  "Pretty print EVENT in BUFFER with limited depth and width."
  (let ((print-length 20)
	(print-level 6)
	(pp-escape-newlines t))
    (pp event buffer)))

(cl-defun eslack--log-event (event connection-or-token type)
  "Record the fact that EVENT occurred in PROCESS.
CONNECTION-OR-TOKEN can also be a string, the API token in use
for this connection"
  (when eslack-log-events
    (with-current-buffer (eslack-events-buffer connection-or-token)
      ;; trim?
      (when (> (buffer-size) 100000)
        (goto-char (/ (buffer-size) 2))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (save-excursion
        (eslack--pprint-event `(,type
                                ,event)
                              (current-buffer)))
      (goto-char (point-max)))))

(defun eslack--process (connection frame)
  (eslack--debug "an incoming frame %s" frame)
  (let ((payload (json-read-from-string (websocket-frame-payload frame)))
        (eslack--dispatching-connection connection))
    (eslack--log-event payload connection :incoming-wss)
    (eslack--event (if (eslack--has payload 'reply_to)
                       :reply-to
                     (eslack--keywordize (eslack--get payload 'type)))
                   (and (eslack--has payload 'subtype)
                        (eslack--keywordize (eslack--get payload 'subtype)))
                   payload)))

(defun eslack--start-websockets (state token &optional continuation)
  (let ((url (eslack--get state 'url))
        (connection nil))
    (websocket-open url 
                    :on-open (lambda (ws)
                               (setq connection (make-instance 'eslack--connection-object
                                                               :websocket ws
                                                               :token token
                                                               :state state))
                               (eslack--opened connection)
                               (when continuation
                                 (funcall continuation connection)))
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

(defun eslack--room-typep (room key)
  (and
   (eslack--has room key)
   (eq (eslack--get room key) t)))

(defun eslack--room-name (room)
  (cond ((eslack--room-typep room 'is_im)
         (let* ((id (eslack--get room 'user))
                (user (or (eslack--find id (eslack--users))
                          (eslack--find id (eslack--bots)))))
           (unless user
             (eslack--error "can't find user %s in team state" id))
           (concat "@" (eslack--get user 'name))))
        ((eslack--room-typep room 'is_group)
         (concat "$" (eslack--get room 'name)))
        ((eslack--room-typep room 'is_channel)
         (concat "#" (eslack--get room 'name)))))

(cl-defun eslack--prompt-for-room (&optional (rooms-fn #'eslack--rooms)
                                             (prompt "Room name?"))
  (let* ((rooms (funcall rooms-fn))
         (room-name (eslack--completing-read
                     (format "[eslack] %s " prompt)
                     (mapcar #'eslack--room-name rooms)
                     nil t))
         (room (cl-find room-name rooms :key #'eslack--room-name
                        :test #'string=)))
    (cl-assert room nil "Can't find room %s" room-name)
    room))

(defun eslack--buffer-name (connection room)
  (format "%s (%s)"
          (eslack--room-name room)
          (eslack--connection-name connection)))

(defun eslack--connection-equal (conn1 conn2)
  (and conn1 conn2
       (string= (eslack--connection-token conn1)
                (eslack--connection-token conn2))))

(defun eslack--ensure-room-buffer (connection room)
  (let ((probe
         (cl-find-if
          (lambda (buffer)
            (with-current-buffer buffer
              (and eslack--buffer-connection
                   eslack--buffer-room
                   (eslack--connection-equal connection eslack--buffer-connection)
                   (string= (eslack--get room 'id)
                            (eslack--get eslack--buffer-room 'id)))))
          (buffer-list))))
    (or probe
        (get-buffer-create (eslack--buffer-name connection room)))))

(defun eslack--call-with-room-buffer (connection room fn)
  (with-current-buffer (eslack--ensure-room-buffer connection room)
    (unless (eq major-mode 'eslack-mode)
      (eslack-mode)
      (setq-local eslack--buffer-room room)
      (setq-local eslack--buffer-connection connection)
      (eslack-refresh-room room))
    (funcall fn)
    (goto-char (point-max))))

(cl-defmacro eslack--with-room-buffer ((connection room) &body body)
  (declare (debug (sexp sexp &rest form))
           (indent 1))
  `(eslack--call-with-room-buffer ,connection ,room (lambda () ,@body)))

(defun eslack-join-channel (connection channel)
  (interactive (let* ((connection (eslack--prompt-for-connection-maybe)))
                 (list connection
                       (let ((eslack--dispatching-connection connection))
                         (eslack--prompt-for-room #'eslack--channels "Channel name?")))))
  (eslack--post :channels.join
                `((name . ,(eslack--get channel 'name)))
                (lambda (_object)
                  (eslack--with-room-buffer (connection channel)
                    (pop-to-buffer (current-buffer))))))

(defun eslack-enter-room (connection room)
  (interactive (let* ((connection (eslack--prompt-for-connection-maybe)))
                 (list connection
                       (let ((eslack--dispatching-connection connection))
                         (eslack--prompt-for-room)))))
  (eslack--with-room-buffer (connection room)
    (pop-to-buffer (current-buffer))))

(defun eslack-refresh-room (room)
  (interactive (list eslack--buffer-room))
  (eslack--post (format "%s.history"
                        (cond ((eslack--room-typep room 'is_im) "im")
                              ((eslack--room-typep room 'is_group) "groups")
                              ((eslack--room-typep room 'is_channel) "channels")
                              (t
                               (eslack--error "strange kinda room %s" room))))
                `((channel . ,(eslack--get room 'id)))
                (lambda (result)
                  (let ((messages (eslack--get result 'messages)))
                    (let ((inhibit-read-only t))
                      (save-restriction
                        (widen)
                        (delete-region (point-min) lui-output-marker)
                        (cl-loop for message across (reverse messages)
                                 for user = (or (eslack--find
                                                 (and (eslack--has message 'user)
                                                      (eslack--get message 'user))
                                                 (eslack--users))
                                                (eslack--find
                                                 (and (eslack--has message 'bot_id)
                                                      (eslack--get message 'bot_id))
                                                 (eslack--bots)))
                                 for timestamp = (eslack--get message 'ts)
                                 for lui-time-stamp-time = (seconds-to-time
                                                            (string-to-number
                                                             timestamp))
                                 when (string= (eslack--get message 'type)
                                               "message")
                                 do (eslack--insert-message user
                                                            message))))))))


;;; More utils
;;;
(defvar eslack--image-cache (make-hash-table :test #'equal))

(defun eslack--insert-image (marker url)
  "Insert image of URL at MARKER.
Do it immediately if it's cached, or schedule insertion for
later.  If MARKER exists in a propertized region of
`eslack--image-target', use that region, otherwise create a new
region."
  (cl-flet ((insert-it
             (image marker)
             (with-current-buffer
                 (marker-buffer marker)
               (save-excursion
                 (goto-char marker)
                 (let ((inhibit-read-only t)
                       (end (and (get-text-property marker 'eslack--image-target)
                                 (next-single-property-change marker 'eslack--image-target))))
                   (if end
                       (add-text-properties marker end `(display ,image))
                       (insert (propertize "[image]" 'display image
                                           'eslack--image-target 'eslack--synthesized))))))))
    (let ((probe (gethash url eslack--image-cache)))
      (cond ((eq 'image (car probe))
             ;; The image is already there, insert it
             ;; 
             (insert-it probe marker))
            (probe
             ;; A list of markers, insert outseves into it
             ;; 
             (setcdr probe (cons marker (cdr probe))))
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
  (let ((sig (cl-gensym "eslack--"))
        (connection (eslack--connection))
        (buffer (current-buffer)))
    (eslack--log-event `((:url . ,url)
                         (:method . ,method)
                         (:sig . ,sig)
                         (:json-params . ,json-params)
                         (:params . ,params))
                       connection
                       :outgoing-http)
    (let ((url-show-status nil)
          (url-request-method (upcase (if (keywordp method)
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
                          (eslack--log-event `((:sig . ,sig)
                                               (:http-status . ,status))
                                             connection
                                             :incoming-http)
                          (let ((oops (plist-get status :error)))
                            (cond (oops
                                   (with-current-buffer
                                       buffer (funcall on-error status)))
                                  ((plist-get status :redirect)
                                   (eslack--web-request (plist-get status :redirect) method :json-params params
                                                        :params params
                                                        :on-success on-success
                                                        :on-error on-error))
                                  (on-success
                                   (search-forward "\n\n")
                                   (let ((object (json-read)))
                                     (eslack--log-event `((:sig . ,sig)
                                                          (:json . ,object))
                                                        connection
                                                        :incoming-http-success)
                                     (with-current-buffer buffer
                                       (funcall on-success status object)))))))))))

(cl-defun eslack--post (method params &optional on-success on-error)
  "Call METHOD on the Slack API with params"
  (let ((connection (eslack--connection)))
    (eslack--checking-connection (connection)
      (eslack--web-request (format "https://slack.com/api/%s"
                                   (cond ((keywordp method)
                                          (substring (symbol-name method) 1))
                                         ((symbolp method)
                                          (symbol-name method))
                                         (t
                                          method)))
                           :post
                           :params (append `((token . ,(eslack--connection-token (eslack--connection))))
                                           params)
                           :on-success (lambda (_status object)
                                         (cond ((eq (eslack--get object 'ok) :json-false)
                                                (if on-error
                                                    (funcall on-error object)
                                                  (eslack--warning "posting to %s returned: %s"
                                                                   method
                                                                   (eslack--get object 'error))))
                                               (on-success
                                                (funcall on-success object))))))))


;;; Buttons
;;;
(defface eslack-button-face
  '((t (:inherit warning)))
  "Used for eslack buttons"
  :group 'eslack)

(define-button-type 'eslack
  'mouse-face 'highlight
  'face 'eslack-button-face
  'font-lock-face 'eslack-button-face
  'help-echo "Push with mouse-1 or RET"
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map button-map)
            (define-key map [mouse-1] 'push-button)
            map))

(define-button-type 'eslack--user-reference :supertype 'eslack)

(define-button-type 'eslack--avatar-button :supertype 'eslack
  'eslack--image-target t
  'action 'eslack--toggle-message-buttons
  'keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map button-map)
            (define-key map [mouse-1] 'push-button)
            (define-key map "v" 'eslack--debug-message-at-point)
            map))

(cl-defun eslack--button (text &rest properties &key (type 'eslack) &allow-other-keys)
  (apply #'make-text-button text nil 'type type properties))

(defun eslack--decode (text)
  (replace-regexp-in-string
   "<@\\([^>|]+\\)|?.*>"
   (lambda (match)
     (let* ((user-id (match-string 1 match))
            (probe
             (or (eslack--find user-id (eslack--users))
                 (eslack--find user-id (eslack--bots)))))
       (eslack--button (if probe
                           (format "@%s" (eslack--get probe 'name))
                         match)
                       :type 'eslack--user-reference)))
   (decode-coding-string text 'utf-8)
   'fixedcase))

(defun eslack--starred-p (message)
  (and
   (eslack--has message 'is_starred)
   (eslack--get message 'is_starred)))

(defun eslack--message-buttons (message)
  (cl-loop for (label . properties)
           in `(("[edit]" action eslack-edit-message)
                ("[delete]" action eslack-delete-message)
                ("[mark-unread]" action eslack-mark-message-unread)
                ("[add-reaction]" action eslack-add-reaction-to-message)
                ("[pin]" action eslack-pin-message)
                ,@(if (eslack--starred-p message)
                      '(("[unstar]" action eslack-unstar-message))
                    '(("[star]" action eslack-star-message))))
           collect (apply #'eslack--button label
                          'eslack--message message
                          'mouse-action (cl-getf properties 'action)
                          properties)))

(defmacro eslack--define-accessor (key)
  `(progn
     (defun ,key (json-object) (eslack--get json-object ',key))
     (gv-define-setter ,key (val json-object)
       `(eslack--put ,json-object ',',key ,val))))

(eslack--define-accessor eslack--overlay)
(eslack--define-accessor eslack--buttons-visible-p)
(eslack--define-accessor eslack--buttons-marker)
(eslack--define-accessor eslack--attachments-marker)
(eslack--define-accessor eslack--reactions-marker)

(defun eslack--toggle-message-buttons (button)
  (let ((message (get-text-property button 'eslack--message)))
    (setf (eslack--buttons-visible-p message)
          (not (eslack--buttons-visible-p message)))
    (eslack--update-message-decorations message)))

(defun eslack--prepare-message (beg end message)
  (cl-loop for key in '(attachments
                        is_starred
                        reactions
                        eslack--buttons-visible-p)
           do (eslack--put message key
                           (and
                            (eslack--has message key)
                            (eslack--get message key))))
  (setf (eslack--overlay message)           (make-overlay beg end nil t nil)
        (eslack--buttons-marker message)    (copy-marker end)
        (eslack--attachments-marker message)(copy-marker end)
        (eslack--reactions-marker message)  (copy-marker end)))

(defun eslack--message-start (message)
  (overlay-start (eslack--overlay message)))

(defun eslack--message-end (message)
  (overlay-end (eslack--overlay message)))

(defun eslack--who-summarize (user-ids)
  (cl-flet ((identify
             (id)
             (decode-coding-string
              (eslack--get (or (eslack--find id (eslack--users))
                               (eslack--find id (eslack--bots)))
                           'name)
              'utf-8)))
    (let* ((noself (cl-remove (eslack--get (eslack--self) 'id)
                              user-ids
                              :test #'string=))
           (self-p (not (eq noself user-ids)))
           (others-limit (max (if self-p 0 1)
                              (- (min 3
                                      (length noself))
                                 1)))
           (and-n-others (if (> (length user-ids) 1)
                             (- (length noself)
                                others-limit)
                           0))
           (listed
            (append (if self-p
                        '("You"))
                    (cl-loop for i from 0 below others-limit
                             for other across noself
                             collect (identify other))))
           (comma-separated
            (mapconcat #'identity
                       listed
                       ", "))
           (and-string
            (and (cl-plusp and-n-others)
                 (if (= 1 and-n-others)
                     (format " and %s" (identify (aref user-ids
                                                       (1- (length user-ids)))))
                   (format " and %s others" and-n-others)))))
      
      (list self-p
            (concat comma-separated and-string)))))

(cl-defun eslack--insert-message (user-or-bot message
                                       &key _own-p
                                       pending
                                       replaced)
  "Insert MESSAGE from USER-OR-BOT.
Destructively modifies MESSAGE and adds some `eslack'-specific
properties to it.
REPLACED is an old message to replace."
  ;; FIXME: restore-point antics could be much simplified between this
  ;; and `eslack--update-message-decorations'
  ;;
  (let* ((message-text (eslack--get message 'text))
         (start (copy-marker lui-output-marker))
         (restore-lom nil)
         (restore-point
          (and replaced
               (- (point-marker)
                  (eslack--buttons-marker replaced)))))
    (when replaced
      (let ((inhibit-read-only t)
            (old-start (eslack--message-start replaced))
            (old-end (eslack--message-end replaced)))
        (delete-region old-start old-end)
        (setq restore-lom (copy-marker lui-output-marker))
        (set-marker lui-output-marker old-start)
        (setq start (copy-marker old-start))))
    ;; `lui-insert', this changes lom for sure
    ;;
    (lui-insert (format "%s%s: %s"
                        (eslack--button "[?]"
                                        :type 'eslack--avatar-button)
                        (propertize (eslack--get user-or-bot 'name)
                                    'eslack--user user-or-bot)
                        (propertize (eslack--decode message-text)
                                    'eslack--message-text t)))
    (let ((inhibit-read-only t)
          (inhibit-point-motion-hooks t))
      (eslack--prepare-message start lui-output-marker message)
      (eslack--update-message-decorations message :restore-point restore-point)
      (add-text-properties (eslack--message-start message)
                           (eslack--message-end message)
                           `(eslack--message ,message)))

    (when pending
      (overlay-put (eslack--overlay message) 'face 'shadow))

    (when (and restore-lom
               (> restore-lom
                  lui-output-marker))
      (setq lui-output-marker restore-lom))
    ;; Finally, schedule some avatar insertion
    ;; 
    (let ((image-url
           (or (ignore-errors
                 (eslack--get user-or-bot 'profile 'image_24))
               (ignore-errors
                 (eslack--get user-or-bot 'icons 'image_36)))))
      (when image-url
        (eslack--insert-image start image-url)))))

(cl-defun eslack--update-message-decorations (message
                                              &key restore-point)
  (save-excursion
    (let ((inhibit-read-only t))
      (unless restore-point
        (setq restore-point
              (and (< (eslack--message-start message)
                      (point)
                      (eslack--message-end message))
                   (- (point-marker)
                      (eslack--buttons-marker message)))))
      (goto-char (eslack--buttons-marker message))
      ;; Reset the overlay face
      ;;
      (overlay-put (eslack--overlay message) 'face nil)
      (delete-region (eslack--buttons-marker message)
                     (eslack--message-end message))
      ;; Re-render starred status
      ;;
      (when (eslack--starred-p message)
        (overlay-put (eslack--overlay message) 'face 'hi-yellow))
      ;; Re-render buttons
      ;;
      (when (eslack--buttons-visible-p message)
        (goto-char (eslack--buttons-marker message))
        (insert
         (mapconcat #'identity
                    `(,@(eslack--message-buttons message)
                      "\n")
                    " ")))
      ;; Render attachments
      ;;
      (set-marker (eslack--attachments-marker message) (point))
      (cl-loop for attachment across (eslack--get message 'attachments)
               ;; for image-url = (eslack--get attachment 'image_url)
               for fallback = (eslack--get attachment 'fallback)
               do
               (insert (propertize
                        (format "[attachment: %s]" fallback)
                        'eslack--image-target
                        t) "\n"))
      ;; Render reactions
      ;; 
      (set-marker (eslack--reactions-marker message) (point))
      (cl-loop for reaction across (eslack--get message 'reactions)
               for (self-reacted-p who-string) = (eslack--who-summarize (eslack--get reaction 'users))
               do
               (insert (propertize
                        (format "%s reacted with %s%s"
                                who-string
                                (eslack--get reaction 'name)
                                (if self-reacted-p
                                    (format " %s"
                                            (eslack--button "[remove]"))
                                  ""))
                        'eslack--image-target
                        t) "\n"))
      ;; Overlay ends here
      ;;
      (move-overlay (eslack--overlay message)
                    (eslack--message-start message)
                    (point))
      ;; Maybe restore lom
      ;;
      (set-marker lui-output-marker (max lui-output-marker
                                         (point)))))
      ;; Maybe restore a point lost in the deleted regions
      ;; 
      (when restore-point
        (goto-char (+ (eslack--buttons-marker message)
                      restore-point))))

;;; Message actions
;;;
(defun eslack--debug-message-at-point (pos)
  (interactive (list (point)))
  (pp-display-expression (get-char-property pos 'eslack--message)
                         "*eslack message*"))

(cl-defmacro eslack--define-message-action (name (message) &optional docstring &body body)
  (declare (indent defun)
           (debug (&define name lambda-list
                           form
                           def-body)))
  `(defun ,name (&optional button)
     ,@(if (stringp docstring)
           (list docstring)
         (setq body
               (cons docstring body))
         nil)
     (interactive)
     (let* ((pos (if button (button-start button) (point)))
            (,message (get-text-property pos 'eslack--message)))
       ,@body)))


;;; Starring messages
;;;
(cl-defmethod eslack--event ((_type (eql :star-added)) _subtype message)
  "A team member has starred an item"
  (eslack--handle-star-change message))

(cl-defmethod eslack--event ((_type (eql :star-removed)) _subtype message)
  "A team member removed a star"
  (eslack--handle-star-change message))

(defun eslack--handle-star-change (frame)
  ;; Calling the arg "FRAME" here to avoid confusion
  (let* ((item (eslack--get frame 'item))
         (in-message (and
                      (eslack--has item 'message)
                      (eslack--get item 'message)))
         (cur-message (and in-message
                           (eslack--find-message (eslack--get in-message 'ts)))))
    (cond (cur-message
           (eslack--put cur-message 'is_starred
                        (and (eslack--has in-message 'is_starred)
                             (eslack--get in-message 'is_starred)))
           (eslack--update-message-decorations cur-message))
          (t
           (eslack--warning "A user has changed stars on some unsupported item %s..." item)))))

(eslack--define-message-action eslack-star-message (message)
  "Star the message at point."
  (eslack--post :stars.add
                `((channel . ,(eslack--get message 'channel))
                  (timestamp . ,(eslack--get message 'ts)))))

(eslack--define-message-action eslack-unstar-message (message)
  "Unstar the message at point."
  (eslack--post :stars.remove
                `((channel . ,(eslack--get message 'channel))
                  (timestamp . ,(eslack--get message 'ts)))))

;;; Deleting messages
;;; 
(eslack--define-message-action eslack-delete-message (message)
  "Delete the message at point"
  (eslack--post :chat.delete
                `((channel . ,(eslack--get message 'channel))
                  ;; "ts" NOT "timestamp"
                  (ts . ,(eslack--get message 'ts)))))

(cl-defmethod eslack--event ((_type (eql :message)) (_subtype (eql :message-deleted)) frame)
  (let ((channel-buffer
         (eslack--find-channel-buffer (eslack--get frame 'channel))))
    (when channel-buffer
      (with-current-buffer channel-buffer
        (let ((cur-message (eslack--find-message (eslack--get frame 'deleted_ts))))
          (cond (cur-message
                 (let ((start (eslack--message-start cur-message))
                       (end (eslack--message-end cur-message)))
                   (eslack--flash-region start
                                         end
                                         :face 'hi-pink
                                         :times 2
                                         :finally (lambda ()
                                                    (let ((inhibit-read-only t))
                                                      (delete-region start end))))))
                (t
                 (eslack--debug "Someone deleted a message that I couldn't find: %s"
                                  frame))))))))


;;; Editing messages
;;; 
(defvar eslack-edit-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'eslack-edit-message-commit)
    (define-key map (kbd "C-c C-k") 'quit-window)
    map))

(defvar eslack--editing-message nil)

(define-derived-mode eslack-edit-message-mode markdown-mode "eslack-edit"
  "Edit eslack messages")

(eslack--define-message-action eslack-edit-message (message)
  "Delete the message at point"
  (let ((cur-text (eslack--get message 'text)))
    (with-current-buffer (generate-new-buffer "*eslack edit message*")
      (eslack-edit-message-mode)
      (setq-local eslack--editing-message message)
      (insert cur-text)
      (pop-to-buffer (current-buffer))
      (eslack--message
       (concat "Do your edits, then \\[eslack-edit-message-commit] to commit"
               ", or \\[quit-window] to discard")))))

(defun eslack-edit-message-commit (message)
  "Commit to the edit of MESSAGE.
Interactively, should only be called in `eslack-edit' buffers."
  (interactive (list eslack--editing-message))
  (let ((window (selected-window)))
    (eslack--post :chat.update
                  `((channel . ,(eslack--get message 'channel))
                    ;; "ts" NOT "timestamp"
                    (ts . ,(eslack--get message 'ts))
                    (text . ,(buffer-substring-no-properties
                              (point-min) (point-max))))
                  (lambda (_object)
                    (eslack--message "edit commited")
                    (if (eq (current-buffer)
                            (window-buffer window))
                        (quit-window 'kill window))))))

(cl-defmethod eslack--event ((_type (eql :message)) (_subtype (eql :message-changed)) frame)
  (let ((channel-buffer
         (eslack--find-channel-buffer (eslack--get frame 'channel)))
        (new-message (eslack--get frame 'message)))
    (when channel-buffer
      (with-current-buffer channel-buffer
        (let ((cur-message (eslack--find-message (eslack--get new-message 'ts))))
          (cond (cur-message
                 (let ((start (eslack--message-start cur-message))
                       (end (eslack--message-end cur-message)))
                   (eslack--flash-region start
                                         end
                                         :face 'hi-green
                                         :times 2
                                         :finally (lambda ()
                                                    (eslack--insert-message
                                                     (eslack--find
                                                      (eslack--get new-message 'user)
                                                      (eslack--users))
                                                     (eslack--merge cur-message
                                                                    new-message)
                                                     :replaced cur-message)))))
                (t
                 (eslack--debug "Someone edited a message that I couldn't find: %s"
                                  frame))))))))


;;; Adding reactions
;;; 
(defun eslack--add-reaction (reactions reaction user)
  (let ((existing (eslack--find reaction reactions :key 'name)))
    (cond (existing
           (eslack--put
            existing
            'count
            (1+ (eslack--get existing 'count)))
           (eslack--put
            existing
            'users
            (vconcat
             (eslack--get existing 'users)
             (vector user))))
          (t
           (setq reactions
                 (vconcat reactions
                          (vector
                           `((count . 1)
                             (users . [,user])
                             (name . ,reaction)))))))
    reactions))

(defun eslack--remove-reaction (reactions reaction user)
  (let ((existing (cl-find-if
                   (lambda (e)
                     (and (string= (eslack--get e 'name)
                                   reaction)
                          (cl-find user (eslack--get e 'users)
                                   :test #'string=)))
                   reactions)))
    (cond (existing
           (eslack--put
            existing
            'count
            (1- (eslack--get existing 'count)))
           (eslack--put
            existing
            'users
            (cl-delete user
                       (eslack--get existing 'users)
                       :test #'string=)))
          (t
           (eslack--warning "Could not find and remove user %s's %s reaction in %s"
                            user reaction reactions)))
    reactions))

(defun eslack--handle-reaction-change (frame fn)
  (let* ((channel-buffer
          (eslack--find-channel-buffer
           (eslack--get frame 'item 'channel))))
    (cond (channel-buffer
           (with-current-buffer channel-buffer
             (let ((message (eslack--find-message (eslack--get frame 'item 'ts))))
               (cond (message
                      (eslack--put message
                                   'reactions
                                   (funcall fn
                                            (eslack--get message 'reactions)
                                            (eslack--get frame 'reaction)
                                            (eslack--get frame 'user)))
                      (eslack--flash-region (eslack--message-start message)
                                            (eslack--message-end message))
                      (eslack--update-message-decorations message))
                     (t
                      (eslack--warning "Someone added a reaction to a message I couldn't find: %s"
                                       frame))))))
          (t
           (eslack--warning "Someone added a reaction to a channel I couldn't find: %s"
                            frame)))))

(cl-defmethod eslack--event ((_type (eql :reaction-added)) _subtype frame)
  "A team member has added an emoji reaction to an item"
  (eslack--handle-reaction-change frame #'eslack--add-reaction))

(cl-defmethod eslack--event ((_type (eql :reaction-removed)) _subtype frame)
  "A team member has added an emoji reaction to an item"
  (eslack--handle-reaction-change frame #'eslack--remove-reaction))


;;; Event processing
;;;
(cl-defgeneric eslack--event (type subtype message))

(cl-defmethod eslack--event ((_type (eql :hello)) _subtype _message)
  (eslack--debug "%s says hello." (eslack--connection-name)))

(cl-defmethod eslack--event ((_type (eql :message)) (_subtype (eql nil)) message)
  (let ((room (eslack--find (eslack--get message 'channel) (eslack--rooms))))
    (eslack--with-room-buffer ((eslack--connection) room)
      (tracking-add-buffer (current-buffer))
      
      (let ((user (eslack--find (eslack--get message 'user) (eslack--users))))
        (eslack--insert-message user message)))))

(cl-defmethod eslack--event ((_type (eql :message)) (_subtype (eql :bot-message)) message)
  (let ((room (eslack--find (eslack--get message 'channel) (eslack--rooms))))
    (eslack--with-room-buffer ((eslack--connection) room)
      (let ((bot (eslack--find (eslack--get message 'bot_id) (eslack--bots))))
        (eslack--insert-message bot message)))))

(cl-defmethod eslack--event ((_type (eql :user-typing)) _subtype message)
  "A channel member is typing a message"
  (when eslack--buffer-room
    (let ((room (eslack--find (eslack--get message 'channel) (eslack--rooms)))
          (user (eslack--find (eslack--get message 'user) (eslack--users))))
      (when (eq room eslack--buffer-room)
        (eslack--message "%s is typing" (eslack--get user 'name))))))


;;; Unimplemented events
;;; 
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

(cl-defmethod eslack--event ((type (eql :reconnect-url)) _subtype message)
  "An \"experimental\" event, according to api.slack.com"
  (eslack--debug "%s is unimplemented: %s" type message))

(cl-defmethod eslack--event ((_type (eql :error)) _subtype message)
  "An error reported by the RTM API. Downgrade it to a warning"
  (eslack--warning "RTM error: %s" (eslack--get message 'error 'msg)))


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

(defvar eslack--awaiting-reply (make-hash-table)
  "A hash table integer -> sent message")

(defun eslack--send-message (text)
  (eslack--checking-connection (eslack--buffer-connection)
   (let* ((id (cl-incf eslack--next-message-id))
          (message `((text . ,text)
                     (id . ,id)
                     (channel . ,(eslack--get eslack--buffer-room 'id))
                     (type . :message))))
     (eslack--insert-message (eslack--find (eslack--get (eslack--self) 'id)
                                           (eslack--users))
                             message
                             :own-p t
                             :pending t)
     (puthash id message eslack--awaiting-reply)
     (eslack--websocket-send (eslack--connection) message)
     (goto-char (point-max)))))

(defvar eslack--last-typing-indicator-timestamp (current-time))

(defun eslack--send-typing-indicator-maybe ()
  (unless (or
           ;; silently discard if this buffer's connection is not live
           ;; anymore
           (not (eslack--connection-live-p eslack--buffer-connection))
           (< (time-to-seconds
               (time-since
                eslack--last-typing-indicator-timestamp))
              3))
    (let* ((id (cl-incf eslack--next-message-id))
           (message `((id . ,id)
                      (channel . ,(eslack--get eslack--buffer-room 'id))
                      (type . :typing))))
      (eslack--websocket-send eslack--buffer-connection message)
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

(cl-defmethod eslack--event ((_type (eql :reply-to)) _subtype reply-to-message)
  "Handle Slack's confirmation to a sent message.
The `reply-to' type doesn't really exist in the Slack API, this
particular method is hack, albeit a pacific one."
  (let* ((id (eslack--get reply-to-message 'reply_to))
         (probe (gethash id eslack--awaiting-reply)))
    (cond (probe
           (remhash id eslack--awaiting-reply)
           (overlay-put (eslack--overlay probe) 'face nil)
           (eslack--put probe 'ts (eslack--get reply-to-message 'ts)))
          (t
           (eslack--debug "Ignoring reply for unknown sent message")))))

(provide 'eslack)
;;; eslack.el ends here
