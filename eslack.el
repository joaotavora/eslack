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

;; 

;;; Code:

(require 'websocket)
(require 'url)
(require 'lui)
(require 'json)
(require 'eieio)


;;; Utils
;;;
(defun eslack--message (format-control &rest format-args)
  (message "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--error (format-control &rest format-args)
  (error "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--get-internal (object keys)
  (cl-loop for key in keys
           for a = object then res
           for res = (progn
                       (unless (listp a)
                         (eslack--error "expected %s to be an alist" a))
                       (assoc key a))
           while key
           unless res
           do (eslack--error "can't find %s in %s" key a)
           finally return res))

(defun eslack--get (object key &rest more)
  (cdr (eslack--get-internal object (cons key more))))

(gv-define-setter eslack--get (value object &rest more)
  `(setcdr (eslack--get-internal ,object (list ,@more))
           ,value))

(cl-defun eslack--find (prop seq &key (key 'id) (test #'string=))
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


;;; Connections
;;; 

;; test, out-of-date by now
;;
;; Team	User	Token	 
;; SISCOG	joaot	xoxp-7449361824-7490502034-7536719025-df3a9e

(defvar eslack--token "xoxp-7449361824-7490502034-7536719025-df3a9e")
(defvar eslack--rtm-start-https-status)
(defvar eslack--debug nil)

(defvar eslack--connections (list))
(defvar eslack--dispatching-connection nil
  "Intented to be let-bound")
(defvar eslack--buffer-connection nil
  "Intended to be buffer-local")

(defclass eslack--connection-object ()
  ((websocket  :initarg :websocket  :accessor eslack--connection-websocket)
   (state :initarg :state :accessor eslack--connection-state)))

(defun eslack--connection-name (connection)
  (eslack--get (eslack--connection-state connection) 'team 'name))

(defun eslack--prompt-for-connection-maybe ()
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
                             (let ((state (eslack--connection-state connection)))
                               (eslack--get state ',prop))))))

  (eslack--define-connection-accessors))


(defun eslack--connection ()
  (or
   eslack--dispatching-connection
   eslack--buffer-connection
   (first eslack--connections)
   (eslack--error "no usable connections")))

(defun eslack-close-all ()
  (interactive)
  (cl-loop for conn in eslack--connections
           do (websocket-close (eslack--websocket conn)))
  (setq eslack--connections nil))

(defun eslack--opened (state url ws)
  (eslack--message "WS connection established to %s!" url)
  (push (make-instance 'eslack--connection-object
                       :websocket ws
                       :state state)
        eslack--connections))

(defun eslack--closed (_state _url _ws)
  (eslack--message "right, closed!"))

(defun eslack--handle-websocket-error (&rest args)
  (eslack--message "oops something went wrong (%s)" args)
  (apply #'websocket-default-error-handler args))

(defun eslack--process (event)
  (eslack--message "a message saying %s" event))

(defun eslack--start-websockets (state)
  (let ((url (eslack--get state 'url)))
    (websocket-open url 
                    :on-open (lambda (ws) (eslack--opened state url ws))
                    :on-message (lambda (_ws f)
                                  (eslack--process (websocket-frame-payload f)))
                    :on-close (lambda (ws) (eslack--closed state url ws))
                    :on-error (lambda (&rest args)
                                (eslack--handle-websocket-error args)))))


;;; Major mode
;;;

(define-derived-mode eslack-mode lui-mode "eslack"
  "A major mode for eslack rooms"
  (lui-set-prompt "\n: "))



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
         (room (cl-find room-name rooms :key (lambda (r)
                                               (eslack--room-name r)))))
    room))

(defun eslack--buffer-name (connection room)
  (format "* eslack: %s (%s) *"
          (eslack--room-name room)
          (eslack--connection-name connection)))

(defun eslack-join-room (connection room)
  (interactive (let* ((connection (eslack--prompt-for-connection-maybe)))
                 (list connection
                       (let ((eslack--dispatching-connection connection))
                         (eslack--prompt-for-room)))))
  (let ((buffer (get-buffer-create (eslack--buffer-name connection room))))
    (with-current-buffer buffer
      (unless (eq major-mode 'eslack-mode) (eslack-mode))
      (pop-to-buffer buffer))))

(provide 'eslack)
;;; eslack.el ends here

: 
