;;; ejab-connect.el --- ejab server connection

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: $Id$

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file defines variables and functions to control ejab's
;; connection to the jabber server, and send and receive XML objects.

;;; Code:

(require 'ejab-utils)
(require 'ejab-obj)

;;{{{ User Connection Variables

(defvar ejab-default-server "jabber.org"
  "Default jabber server for ejab to connect to.")

(defvar ejab-default-port 5222
  "Port on which to connect to `ejab-default-server'.")

(defvar ejab-default-username nil
  "Username to connect to `ejab-default-server' with.")

(defvar ejab-default-password nil
  "Password to register with `ejab-default-server'.")

(defvar ejab-default-resource "ejab"
  "Default resource to connect as.")

;;}}}
;;{{{ Current Connection

(defvar ejab-current-server nil
  "Current jabber server ejab is connected to.")

(defvar ejab-current-port nil
  "Current port ejab is connected to `ejab-current-server' on.")

(defvar ejab-current-username nil
  "Current username ejab is connected to `ejab-current-server' as.")

(defvar ejab-current-resource nil
  "Current resource ejab is connected to `ejab-current-server' with.")

(defun ejab-current-jid (&rest args)
  "Return current JID in user@host form."
  (and ejab-current-username
       ejab-current-server
       (format "%s@%s" ejab-current-username ejab-current-server)))

(defun ejab-current-jid-with-resource ()
  "Return current JID in user@host/resource form."
  (and ejab-current-username
       ejab-current-server
       (if ejab-current-resource
           (format "%s@%s/s" ejab-current-username
                   ejab-current-server
                   ejab-current-resource)
         (ejab-current-jid))))

;;}}}
;;{{{ Connecting and Disconnecting

(defvar ejab-before-connect-hook '()
  "Normal hook run before connecting to the server.")

(defvar ejab-connected-hook '()
  "Normal hook run upon connection to the server.
This does not mean successful login--see `ejab-authenticated-hook'.")

(defvar ejab-disconnected-hook '()
  "Normal hook run upon disconnection from server.")


(defvar ejab-connection nil
  "Connection to jabber server as a process object.")

(defvar ejab-process-buffer "*ejab*"
  "Buffer for the ejab connection process.
XML received from the server is logged here as well as processed by
the filter function.")

(defun ejab-connect (&optional server port username password resource)
  "Connect to jabber server if not already connected.
The parameters SERVER, PORT, USERNAME, PASSWORD, and RESOURCE override
the respective `ejab-default-PARAM' variables.  Interactively, prompts
for whichever of those variables is nil, unless a prefix argument is
given, in which case prompts for all of them."
  (interactive
   (list
    (or (and (not current-prefix-arg) ejab-default-server)
        (read-string (format "Server (default %s): "
                             (or ejab-default-server "jabber.org"))
                     nil nil (or ejab-default-server "jabber.org")))
    (or (and (not current-prefix-arg) ejab-default-port)
        (read-string (format "Port (default %s): "
                             (or ejab-default-port 5222))
                     nil nil (or ejab-default-port 5222)))
    (or (and (not current-prefix-arg) ejab-default-username)
        (read-string (format "Username%s: "
                             (if ejab-default-username
                                 (format " (default %s)"
                                         ejab-default-username)
                               ""))
                     nil nil ejab-default-username))
    (or (and (not current-prefix-arg) ejab-default-password)
        (read-passwd "Password: " nil ejab-default-password))
    (or (and (not current-prefix-arg) ejab-default-resource)
        (read-string (format "Resource%s: "
                             (if ejab-default-resource
                                 (format " (default %s)"
                                         ejab-default-resource)
                               ""))
                     nil nil ejab-default-resource))))
  (when ejab-connection
    (ejab-notify 2 "Connection already open; connection aborted"))
  (setq server   (or server ejab-default-server
                     (ejab-notify 3 "No server specified"))
        port     (or port ejab-default-port
                     (ejab-notify 3 "No port specified"))
        username (or username ejab-default-username
                     (ejab-notify 3 "No username specified"))
        password (or password ejab-default-password
                     (ejab-notify 3 "No password specified"))
        resource (or resource ejab-default-resource
                     (ejab-notify 3 "No resource specified")))
  (setq ejab-current-server server
        ejab-current-port port
        ejab-current-username username
        ejab-current-password password
        ejab-current-resource resource)
  (run-hooks 'ejab-before-connect-hook)
  ;; Open the connection
  (setq ejab-connection
        (open-network-stream "jabber" ejab-process-buffer
                             server port))
  (set-marker (process-mark ejab-connection)
              (save-excursion
                (with-current-buffer (process-buffer ejab-connection)
                  (erase-buffer)
                  (point-marker))))
  (set-process-filter ejab-connection #'ejab-filter)
  (process-send-string
   ejab-connection
   (concat "<?xml version='1.0'?><!DOCTYPE jabber SYSTEM \"jabber.dtd\">
<stream:stream to='" server "' xmlns='jabber:client'
xmlns:stream='http://etherx.jabber.org/streams'>"))
  (run-hooks 'ejab-connected-hook))

(defun ejab-notify-connected ()
  (ejab-notify 2 "Host %s:%s found, logging in..."
               ejab-current-server ejab-current-port))
(add-hook 'ejab-connected-hook 'ejab-notify-connected)

(defun ejab-disconnect ()
  "Disconnect from jabber server."
  (interactive)
  (if ejab-connection
      (progn
        (when (eq (process-status ejab-connection) 'open)
          (process-send-string ejab-connection "</stream:stream>")
          (delete-process ejab-connection))
        (setq ejab-connection nil
              ejab-authenticated nil
              ejab-current-session-id nil)
        (run-hooks 'ejab-disconnected-hook))
    (ejab-notify 3 "Not connected to server")))

(defun ejab-notify-disconnected ()
  (ejab-notify 2 "Disconnected from %s" ejab-current-server))
(add-hook 'ejab-disconnected-hook 'ejab-notify-disconnected)

;;}}}
;;{{{ Receive Incoming XML

(defvar ejab-receive-object-functions nil
  "Functions to process incoming XML data.
Each is called with one argument, an ejab object representing the
incoming data, and should either take appropriate action for the
object and return non-nil, or decline to handle it by returning nil.

Functions which send data to the server and expect a response should
add a function to this hook.  That function in turn should detect the
expected response, declining to handle all other incoming data, and
when it comes it should handle it however is appropriate and then
remove itself from this hook.  Here is an example of such a function,
which is expecting a response to an IQ query \(a common thing for such
a function to be doing).

\(defun ejab-receive-THING (obj)
  (ejab-with (obj)
    (when (and (eq (obj :typeof) 'iq)
               (eql (string-to-int (obj 'id)) ejab-THING-id))
      (if (string= (obj 'type) \"result\")
          (ejab-process-THING (obj :=> 'query))
        (run-hook-with-args 'ejab-error-hooks (obj :=> 'error)))
      (remove-hook 'ejab-receive-object-functions 'ejab-receive-THING)
      t)))

If you find yourself worrying about the order of the functions in this
hook, or using the third argument to `add-hook', you are going about
things in the wrong way.  At any given time there should be only *one*
function in this hook capable of handling any given received object.
If another function is handling something you want to handle, take it
up with that function--it may be configurable to decline certain
objects, or run a different hook that would suffice.  See also
`ejab-before-receive-object-hooks' and `ejab-after-receive-object-hooks'.")

(defvar ejab-before-receive-object-hooks nil
  "Hooks called before incoming XML data is processed.
Each is given one argument, an ejab object of the incoming data.")

(defvar ejab-after-receive-object-hooks nil
  "Hooks called after incoming XML data is processed.
Each is given one argument, an ejab object of the incoming data.")

(defun ejab-filter (process string)
  "Filter function for the jabber server connection."
  (cond
   ;; Sanity check
   ((not (eq process ejab-connection)))

   ;; Check for the startup string, opening the <stream:stream> tag.
   ((string-match "^<\\?xml" string)
    ;; Need to grab the session ID for later use in authentication,
    ;; but otherwise we can ignore this string.
    (setq ejab-current-session-id
          (progn (string-match "id='\\([A-Za-z0-9]+\\)'" string)
                 (match-string 1 string))))

   ;; Check for ending the connection and <stream:stream> tag.
   ((string-match "</stream:stream>" string)
    (ejab-disconnect))

   ;; Otherwise, parse for an XML tag and handle it.
   (t
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point)))
      (let (obj (pos (point)))
        ;; This function never changes the value of point except
        ;; to restore it on incomplete input.  `ejab-parse-xml'
        ;; uses point to keep track of where it is.
        (catch 'ejab-incomplete-tag
          ;; This throw is the *only* way the loop is exited.
          (while t
            (ejab-notify 0 "Parsing XML...")
            (setq obj (ejab-parse-xml))
            (ejab-notify 0 "Parsing XML...tag found")
            (ejab-notify 0 "Processing %s object..."
                         (funcall obj :typeof))
            (run-hook-with-args 'ejab-before-receive-object-hooks obj)
            (or (run-hook-with-args-until-success
                 'ejab-receive-object-functions obj)
                (ejab-notify 3 "Unhandled response: %s" string))
            (run-hook-with-args 'ejab-after-receive-object-hooks obj)
            (ejab-notify 0 "Processing %s object...done"
                         (funcall obj :typeof))
            (setq pos (point))))
        (ejab-notify 0 "Parsing XML...done")
        (goto-char pos))))))

;;}}}
;;{{{ Send Objects

(defvar ejab-before-send-object-hooks nil
  "Abnormal hook run before sending an object to the server.
Functions receive one argument: the ejab object about to be sent.")

(defvar ejab-after-send-object-hooks nil
  "Abnormal hook run after sending an object to the server.
Functions receive one argument: the ejab object just sent.")

(defun ejab-obj-send (object)
  "Send OBJECT to the server.
This is for internal use only--programs should call \(OBJECT :send)."
  (if ejab-connection
      (progn
        (run-hook-with-args 'ejab-before-send-object-hooks object)
        (process-send-string ejab-connection (funcall object :xml))
        (run-hook-with-args 'ejab-after-send-object-hooks object))
    (ejab-notify 3 "Not connected to server")))

;;}}}
;;{{{ ID Numbers

(defvar ejab-id-counter 0
  "Counter for IQ ID numbers.")

(defun ejab-next-id ()
  "Return the next sequential ID number.
ID numbers start from 0 and increase sequentially, never resetting
within an Emacs session."
  (prog1 ejab-id-counter
    (incf ejab-id-counter)))

(defun ejab-reset-id ()
  (setq ejab-id-counter 0))

(add-hook 'ejab-before-connect-hook 'ejab-reset-id)

(defvar ejab-current-session-id nil
  "The session ID of the current Jabber connection.
This is set by the server in the initial <stream:stream> tag and is
currently used only for digest authentication.")

;;}}}
;;{{{ Error Notification

(defvar ejab-error-hooks '()
  "Abnormal hook run when an error is received from the server.
Each function receives one argument, an ejab object of type `error'.
Useful attributes of that object are `type' and `:text'.")

(defvar ejab-error-types
  '(("302" . "Redirect")
    ("400" . "Bad Request")
    ("401" . "Unauthorized")
    ("402" . "Payment Required")
    ("403" . "Forbidden")
    ("404" . "Not Found")
    ("405" . "Not Allowed")
    ("406" . "Not Acceptable")
    ("407" . "Registration Required")
    ("408" . "Request Timeout")
    ("500" . "Internal Server Error")
    ("501" . "Not Implemented")
    ("502" . "Remote Server Error")
    ("503" . "Service Unavailable")
    ("504" . "Remote Server Timeout"))
  "Alist of Jabber error types and descriptions.")

(defun ejab-notify-error (err)
  "Notify the user that an error was received from the server.
This is not the same as an Ejab error and is notified with lower
priority."
  (let ((code (funcall err 'code)))
    (ejab-notify 2 "Error %s (%s): %s" code
                 (cdr (assoc code ejab-error-types))
                 (funcall err :text))))

(add-hook 'ejab-error-hooks #'ejab-notify-error)

;;}}}
;;{{{ Authentication

(defvar ejab-authenticated-hook '()
  "Normal hook run upon successful authentication with server.")

(defvar ejab-authenticated nil
  "Whether we have yet successfully authenticated with the server.")

(defvar ejab-use-digest-auth-p t
  "*Whether to use digest authentication.
If nil, plain text passwords are sent instead.")

(defvar ejab-auth-id nil)

(defun ejab-send-auth ()
  "Send authentication information to the server."
  ;; Make sure we have the correct session ID
  (unless ejab-current-session-id
    (accept-process-output ejab-connection))
  ;; Get ready for a response before sending anything
  (add-hook 'ejab-receive-object-functions #'ejab-receive-auth)
  (funcall
   (ejab-make-iq
    `(id ,(setq ejab-auth-id (ejab-next-id)) type "set")
    `(query
      ,(ejab-make-query
        '(xmlns "jabber:iq:auth")
        `((username . ,ejab-current-username)
          (resource . ,ejab-current-resource)
          ;; Use digest authentication if the user requested it, and
          ;; if SHA1 hashing is available.
          ,(if (when ejab-use-digest-auth-p
                 (ignore-errors
                   (require 'sha1))
                 (or (fboundp 'sha1)
                     (fboundp 'sha1-encode)
                     (progn
                       (ejab-notify 1 "SHA1 not found, reverting to plain text authentication.")
                       nil)))
               ;; Use digest authentication
               (cons 'digest
                     (funcall
                      (if (fboundp 'sha1) #'sha1 #'sha1-encode)
                      ;; The proper SHA1 hash is the session ID
                      ;; concatenated with the password.
                      (concat ejab-current-session-id
                              ejab-current-password)))
             ;; Use plain text authentication
             `(password . ,ejab-current-password))))))
   :send))

(add-hook 'ejab-connected-hook 'ejab-send-auth)

(defun ejab-reset-auth ()
  (setq ejab-authenticated nil))
(add-hook 'ejab-before-connect-hook 'ejab-reset-auth)

(defun ejab-receive-auth (obj)
  "Receive authentication information from server.
Return non-nil if OBJ is the desired response."
  (ejab-with (obj)
    (when (and (eq (obj :typeof) 'iq)
               (eql (string-to-int (obj 'id)) ejab-auth-id))
      (if (string= (obj 'type) "result")
          (progn
            (setq ejab-authenticated t)
            (run-hooks 'ejab-authenticated-hook))
        (setq ejab-authenticated nil)
        (run-hook-with-args 'ejab-error-hooks (obj :=> 'error))
        (let ((ejab-disconnected-hook nil)) ; Let the user see the error
          (ejab-disconnect)))
      (remove-hook 'ejab-receive-object-functions 'ejab-receive-auth)
      t)))

(defun ejab-notify-authenticated ()
  (ejab-notify 2 "Logged in as %s@%s/%s"
               ejab-current-username
               ejab-current-server
               ejab-current-resource))
(add-hook 'ejab-authenticated-hook 'ejab-notify-authenticated)

;;}}}

(provide 'ejab-connect)

;;; ejab-connect.el ends here
