;;; ejab-utils.el --- Utility functions and variables for ejab

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

;; This file contains several utility functions, macros, and variables
;; used by ejab.  Since it contains macros, it should be loaded at
;; compile-time by a top-level `require'.

;;; Code:

(require 'cl)

;;{{{ Define Error Symbols

(defmacro ejab-define-error (symbol string &optional subtype)
  "Define SYMBOL as an Ejab error with error message STRING.
If SUBTYPE is non-nil, it is used as a sub-type of error under
`ejab-errors'."
  `(progn
     (put ',symbol 'error-conditions
          '(error ejab-errors
                  ,@(if subtype (list subtype) '())
                  ,symbol))
     (put ',symbol 'error-message ,string)))

;;}}}
;;{{{ Notification Functions

(defvar ejab-notify-message-prefix "Ejab: "
  "String prepended to all Ejab notification messages.")

(defvar ejab-notification-function-list
  '(identity message message error)
  "List of functions to notify the user of Ejab events.
This list should have exactly four elements, one for each event
priority.  Each should be a function taking one argument, a string to
show the user, and displaying it \(or not) in an appropriate manner.
The first element is for priority 0, the second for 1, and so on.")

(defun ejab-toggle-debugging ()
  "Toggle the first element of `ejab-notification-function-list'.
If it is anything other than `identity', set it to that, otherwise set
it to `message'."
  (interactive)
  (if (eq (car ejab-notification-function-list) 'identity)
      (progn
        (setq ejab-notification-function-list
              (cons 'message (cdr ejab-notification-function-list)))
        (ejab-notify 1 "Debugging is now ON"))
    (progn
      (setq ejab-notification-function-list
            (cons 'identity (cdr ejab-notification-function-list)))
      (ejab-notify 1 "Debugging is now OFF"))))

(defun ejab-notify (&rest args)
  "Notify the user of some Ejab event in an appropriate way.

The first argument should be a number 0, 1, 2, or 3 indicating the
priority of the message: 0 is Debugging, 1 is FYI, 2 is Important, 3
is an Error.  The second argument should be a string, and remaining
arguments, if any, are as those to `format'.  Messages should start
with a capital letter and end without a period.  The *first* argument
may be omitted and defaults to 1.

How the user is notified depends on the priority and the current
interface configuration.  By default, priority 0 events are ignored,
priority 1 and 2 events are signalled with `message', and priority 3
events are signalled with `error', but these defaults can be changed
in `ejab-notification-function-list', which see."
  ;; Handle default priority
  (unless (numberp (car args))
    (setq args (cons 1 args)))
  (funcall (nth (car args) ejab-notification-function-list)
           (apply #'format
                  (concat ejab-notify-message-prefix (cadr args))
                  (cddr args))))

;;}}}
;;{{{ With Objects as Functions

(defmacro ejab-with (objects &rest body)
  "Evaluate BODY with OBJECTS fbound to their variable values.
This eliminates the need for `funcall' in dealing with ejab objects.
For example, \(funcall OBJ ARGS ...) can be replaced by the equivalent
\(ejab-with \(OBJ) \(OBJ ARGS ...)).  This is most useful when an
object is used many times in a function."
  `(letf ,(mapcar #'(lambda (obj)
                      `((symbol-function ',obj) ,obj))
                  objects)
     ,@body))

;   `(flet ,(mapcar #'(lambda (obj)
;                       (let ((g (gensym)))
;                         `(,obj ,g (apply ,obj ,g))))
;                   objects)
;      ,@body))

(put 'ejab-with 'lisp-indent-function 1)

(def-edebug-spec ejab-with t)

;;}}}
;;{{{ Make Caller Functions

(defun ejab-call (&rest args)
  "Return a function of one argument that calls it with ARGS.
\(ejab-call ARG ...)  ===  (lambda (x) (funcall x ARG ...)).
This can be useful for :key arguments, for example."
  (lexical-let ((args args))
    #'(lambda (x) (apply x args))))

;;}}}
;;{{{ Parsing JIDs

(defvar ejab-entity-regexp
  "\\([^:@<>'\"& \t\n\r]+@\\)?\\([a-zA-Z0-9.]+\\)\\(/.+\\)*"
  "Regular expression to match user@host/resource Jabber entities.
First subexp is user, second host, third resource.  User includes the
`@' sign and is absent for transports.")

(defun ejab-extract-jid (string)
  "Extract the JID \(user@host) from STRING.
If STRING is nil, return nil."
  (when string
    (save-match-data
      (string-match ejab-entity-regexp string)
      ;; The user part includes the @ sign
      (format "%s%s" (match-string 1 string)
              (match-string 2 string)))))

(defun ejab-extract-resource (string)
  "Extract the resource from STRING.
If STRING is nil or contains no resource, return nil."
  (when string
    (save-match-data
      (string-match ejab-entity-regexp string)
      ;; Skip the inital slash
      (and (match-string 3 string)
           (substring (match-string 3 string) 1)))))

(defun ejab-presence-jid (presence)
  "Return the JID associated with PRESENCE.
For internal use only, programs should call \(PRESENCE :jid)."
  (ejab-extract-jid (funcall presence 'from)))

(defun ejab-item-jid (item)
  "Return the JID associated with ITEM.
For internal use only, programs should call \(ITEM :jid)."
  (ejab-extract-jid (funcall item 'jid)))

(defun ejab-presence-resource (presence)
  "Return the resource associated with PRESENCE.
For internal use only, programs should call \(PRESENCE :resource)."
  (ejab-extract-resource (funcall presence 'from)))

(defun ejab-item-resource (item)
  "Return the resource associated with ITEM.
For internal use only, programs should call \(ITEM :resource)."
  (ejab-extract-resource (funcall item 'jid)))

;;}}}
;;{{{ Compare w/o Case

(defun ejab-compare-nocase (string1 string2)
  "Return non-nil if STRING1 and STRING2 are equal, ignoring case."
  (string= (downcase string1) (downcase string2)))

;;}}}
;;{{{ Fill Column

(defun ejab-adjust-fill-column ()
  "Set the fill-column to 7/8 of the window width."
  (set (make-local-variable 'fill-column)
       (round (* (window-width) .875))))

;;}}}
;;{{{ Display Buffers

(defvar ejab-previous-window-config nil
  "Previous window configuration to restore after a buffer is done.")

(defun ejab-restore-window-config ()
  "Restore previous window configuration."
  (set-window-configuration ejab-previous-window-config)
  )

(defvar ejab-frame-to-kill nil
  "Frame to kill when a buffer is done.")

(defun ejab-kill-frame ()
  (delete-frame ejab-frame-to-kill))

(defun ejab-display-buffer-by-spec (buffer spec &optional hook)
  "Display BUFFER according to SPEC.
Allowable values for SPEC, and their meanings, are:

nil     Current window \(`switch-to-buffer')
window  Other window \(`switch-to-buffer-other-window')
pop     Uses `pop-to-buffer', which see for controlling variables.
frame   New frame \(`make-frame')
LIST    New frame \(LIST passed as parameter to `make-frame')

HOOK should be a hook variable which will be run to restore the
previous configuration.  An appropriate function will be added to this
hook to restore the window configuration, delete the frame, etc.  HOOK
will be buffer-localized if it is not already, and should be run
before the buffer is killed, if it is to be killed."
  (unless (equal (current-buffer) (get-buffer buffer))
    (let ((wconfig (current-window-configuration)))
      (case spec
        ((nil)
         (switch-to-buffer buffer)
         (add-hook (make-local-hook hook) 'bury-buffer nil t))
        ((window)
         (switch-to-buffer-other-window buffer)
         (set (make-local-variable 'ejab-previous-window-config)
              wconfig)
         (add-hook (make-local-hook hook)
                   'ejab-restore-window-config nil t))
        ((pop)
         (pop-to-buffer buffer)
         (set (make-local-variable 'ejab-previous-window-config)
              wconfig)
         (add-hook (make-local-hook hook)
                   'ejab-restore-window-config nil t))
        (t
         (select-frame (make-frame (and (consp spec) spec)))
         (switch-to-buffer buffer)
         (set (make-local-variable 'ejab-frame-to-kill)
              (selected-frame))
         (add-hook (make-local-hook hook) 'ejab-kill-frame nil t)
         )))
    (run-hooks buffer-displayed-hook)))

(defvar buffer-displayed-hook nil
  "Hook run after a buffer is displayed.
This hook should always be localized with `make-local-hook'.")

;;}}}

(provide 'ejab-utils)

;;; ejab-utils.el ends here
