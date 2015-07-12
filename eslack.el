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

;; test, out-of-date by now
;;
;; Team	User	Token	 
;; SISCOG	joaot	xoxp-7449361824-7490502034-7536719025-df3a9e

(defvar eslack--token "xoxp-7449361824-7490502034-7536719025-df3a9e")
(defvar eslack--rtm-start-https-status)
(defvar eslack--team-state nil
  "JSON response to /api/rtm.start")
(defvar eslack--debug nil)

(defvar eslack--connections (list))

(defclass eslack--connection ()
  ((websocket :initarg :websocket  :accessor eslack--websocket)
   (team-state :initarg :team-state :team-state eslack--websocket)))

(defun eslack--close-all ()
  (cl-loop for conn in eslack--connections
           do (websocket-close (eslack--websocket conn)))
  (setq eslack--connections nil))

(defun eslack--start-websockets ()
  (let ((url (eslack--get eslack--team-state 'url)))
    (websocket-open url 
                    :on-open (lambda (ws)
                               (eslack--message "right, it opened with %s!" ws)
                               (push (make-instance 'eslack--connection
                                                    :websocket ws
                                                    :team-state eslack--team-state)
                                     eslack--connections))
                    :on-message (lambda (_ws f)
                                  (eslack--message "a message saying %s" f))
                    :on-close (lambda (ws)
                                (eslack--message "right, closed with %s!" ws))
                    :on-error (lambda (&rest args)
                                (eslack--message "oops something went wrong (%s)" args)
                                (apply #'websocket-default-error-handler args)))))

(defun eslack-join-room (room-name)
  (interactive
   (progn
     (unless (and eslack--rooms
                  eslack--whoami)
       (eslack-init))
     (list
      (completing-read
       (eslack--format "join which room? ")
       (loop for room across eslack--rooms
             collect (eslack--get 'name room))))))

  (let ((room
         (cl-find room-name
                  eslack--rooms
                  :key #'(lambda (room) (eslack--get 'name room))
                  :test #'string=)))
    (with-current-buffer (eslack--room-buffer room-name)

      (unless (eq major-mode 'eslack-mode)
        (eslack-mode)
        (setq eslack--room-id (eslack--get 'id room))
        (eslack--setup-room)
        (eslack--join-room #'eslack--joined
                          #'eslack--teardown))

      (pop-to-buffer (current-buffer)))))

(defun eslack--message (format-control &rest format-args)
  (message "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--error (format-control &rest format-args)
  (error "%s" (apply #'format (concat "[eslack] " format-control) format-args)))

(defun eslack--get (alist key &rest more)
  (cl-loop for key in (cons key more)
           for a = alist then res
           for res = (progn
                       (unless (listp a)
                         (eslack--error "expected %s to be an alist" a))
                       (alist-get key a 'eslack--oops))
           while key
           when (eq res 'eslack--oops)
           do (eslack--error "can't find %s in %s" key a)
           finally return res))

(defun eslack ()
  (interactive)
  (url-retrieve
   (format "https://slack.com/api/rtm.start?token=%s"
           eslack--token)
   (lambda (status)
     (let ((error (plist-get status :error))
           (redirect (plist-get status :redirect)))
       (when error
         (signal (car error) (cdr error)))
       (when redirect
         (error "slack requests that you try again to %a" redirect))
       (search-forward "\n\n")
       (let ((eslack--team-state (json-read)))
         (setq eslack--debug eslack--team-state)
         (eslack--start-websockets))))))

(alist-get 'url eslack--debug)

(provide 'eslack)
;;; eslack.el ends here
