;;; ejab-presence.el --- ejab presence and subscription functions

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

;; This file defines variables and functions to manage ejab presences
;; and subscriptions to and from other users.

;;; Code:

(require 'cl)
(require 'ejab-utils)
(require 'ejab-obj)
(require 'ejab-roster)

;;{{{ Current Presence Info

(defvar ejab-current-available-p t
  "Non-nil if currently available and nil if unavailable.
When this variable is changed, `ejab-send-current-presence' should be
called to inform the server of the changes.")

(defvar ejab-current-presence-show nil
  "How to display the current presence.
Should be one of the strings `away', `chat', `xa', and `dnd'.
When this variable is changed, `ejab-send-current-presence' should be
called to inform the server of the changes.")

(defvar ejab-current-presence-status nil
  "A detailed description of the current presence as a string.
When this variable is changed, `ejab-send-current-presence' should be
called to inform the server of the changes.")

(defvar ejab-current-priority nil
  "The current priority as an integer.
This controls the dominant resource: if you log in multiple times with
different resources, the resource with the highest numeric priority
receives all messages not addressed to a specific resource.

When this variable is changed, `ejab-send-current-presence' should be
called to inform the server of the changes.")

(defvar ejab-default-presence-info '(t nil nil nil)
  "Default availablility and status info.
The current presence info is initialized using this variable upon each
connection to the server.  Each element of this variable can be either
a function to call \(a symbol or lambda expression), a variable to
evaluate \(a non-`fboundp' symbol), or a string or integer.  The
format is \(AVAILABLE-P SHOW STATUS PRIORITY), i.e. the resulting
value from each element is used to initialize, respectively, the
variables `ejab-current-available-p', `ejab-current-presence-show',
`ejab-current-presence-status', and `ejab-current-priority'.")

(defun ejab-initialize-presence-info ()
  "Initialize presence info from defaults and send to server."
  (multiple-value-setq (ejab-current-available-p
                        ejab-current-presence-show
                        ejab-current-presence-status
                        ejab-current-priority)
    (mapcar #'(lambda (value)
                (cond ((functionp value) (funcall value))
                      ((symbolp value) (symbol-value value))
                      (t value)))
            ejab-default-presence-info))
  (ejab-send-current-presence))

;; This is necessary not only to initialize, but to send the initial
;; "available" presence.
(add-hook 'ejab-authenticated-hook 'ejab-initialize-presence-info)

;;}}}
;;{{{ Interactive Status

(defvar ejab-default-away-message "User is currently Away"
  "Default status message when setting presence `away'.
Can be either a string, a function to call, or a variable to take the
value of.  This is also the default `xa' and `dnd' message when the
corresponding variables are nil.")

(defvar ejab-default-xa-message "User is currently Not Available"
  "Default status message when setting presence `xa'.
Can be either a string, a function to call, or a variable to take the
value of.  If nil, `ejab-default-away-message' is used.")

(defvar ejab-default-dnd-message "Please Do Not Disturb"
  "Default status message when setting presence `dnd'.
Can be either a string, a function to call, or a variable to take the
value of.  If nil, `ejab-default-away-message' is used.")

(defvar ejab-default-chat-message "Free for Chat!"
  "Default status message when setting presence `chat'.
Can be either a string, a function to call, or a variable to take the
value of.")

(defvar ejab-prompt-status-messages 't
  "Non-nil means prompt for messages when setting status.
The results of consulting `ejab-default-STATUS-message' are then
inserted in the minibuffer as the default return string.  Nil means to
just use the defaults without prompting.")

(defun ejab-get-status-message (status default)
  "Get the appropriate STATUS message with default DEFAULT."
  (cond ((functionp default)
         (setq default (funcall default)))
        ((symbolp default)
         (setq default (symbol-value default))))
  (if ejab-prompt-status-messages
      (read-string (format "%s message: " status) default)
    default))

(dolist (status '("away" "xa" "dnd" "chat"))
  (fset (intern (format "ejab-toggle-presence-%s" status))
        `(lambda (&optional arg)
           ,(format "Toggle `%s' status, or set/unset with positive/negative ARG.
Specifically, if ARG is positive or `ejab-current-presence-show' is
nil, set it to `%s' and set `ejab-current-presence-status' to
`ejab-default-%s-message' \(see `ejab-prompt-status-messages'),
otherwise set `ejab-current-presence-show' to nil and clear status."
                    status status status)
           (interactive "P")
           (if (if arg
                   (> (prefix-numeric-value arg) 0)
                 (null ejab-current-presence-show))
               (setq ejab-current-presence-show ,status
                     ejab-current-presence-status
                     (ejab-get-status-message
                      ,(capitalize status)
                      ,(intern (format "ejab-default-%s-message"
                                       status))))
             (setq ejab-current-presence-show nil
                   ejab-current-presence-status nil))
           (ejab-send-current-presence)))
  (fset (intern (format "ejab-set-presence-%s" status))
        `(lambda ()
           ,(format "Set current status to `%s'.
Specifically, set `ejab-current-presence-show' to `%s' and set
`ejab-current-presence-status' to `ejab-default-%s-message' \(see
`ejab-prompt-status-messages')."
                    status status status)
           (interactive)
           (setq ejab-current-presence-show ,status
                 ejab-current-presence-status
                 (ejab-get-status-message
                  ,(capitalize status)
                  ,(intern (format "ejab-default-%s-message"
                                   status))))
           (ejab-send-current-presence)))
  )

(defun ejab-clear-presence ()
  "Set current status to normal available.
Clears `ejab-current-presence-show' and `ejab-current-presence-status'."
  (interactive)
  (setq ejab-current-presence-show nil
        ejab-current-presence-status nil)
  (ejab-send-current-presence))

(defun ejab-set-priority (priority)
  "Set current priority to PRIORITY, an integer."
  (interactive "nPriority: ")
  (setq ejab-current-priority priority)
  (ejab-send-current-presence))

;;}}}

;;{{{ Send Presence Tags

(defun ejab-send-current-presence ()
  "Send current presence info to the server.
Looks at by `ejab-current-available-p', `ejab-current-presence-show',
`ejab-current-presence-status', and `ejab-current-priority'."
  (funcall
   (ejab-make-presence
    `(type ,(if ejab-current-available-p "available" "unavailable"))
    (list 'show ejab-current-presence-show
          'status ejab-current-presence-status
          'priority (and ejab-current-priority
                         (int-to-string ejab-current-priority))))
   :send))

;;}}}
;;{{{ Request Subscriptions

(defun ejab-request-subscription (&optional user)
  "Request subscription to the presence of a Jabber USER.
You will be notified if the request is accepted.  USER may be either a
jid \(a string `USER@HOST') or an ejab `item' object.  Interactively,
prompts to add the user to the roster as well."
  (interactive "sSubscribe to user: ")
  (let ((jid (if (stringp user) user (funcall user 'jid))))
    (funcall (ejab-make-presence `(to ,jid type "subscribe")) :send)
    (and (interactive-p)
         (y-or-n-p "Add user to the roster as well?")
         (ejab-interactive-add-roster-item jid))))

;; Acceptance is handled in `ejab-receive-presence'.  We do this
;; globally, rather than temporarily when subscriptions are requested,
;; because a response is not guaranteed, more than one subscription
;; request can be in progress at once, and can span several sessions.

;;}}}
;;{{{ Request Unsubscriptions

(defun ejab-request-unsubscription (user)
  "Unsubscribe to the presence of a USER.
You will be notified when the unsubscription is complete.  USER may be
either a jid \(a string `USER@HOST') or an ejab `item' object.
Interactively, prompts to remove the user from the roster as well."
  (interactive "sUnsubscribe to user: ")
  (let ((jid (if (stringp user) user (funcall user 'jid))))
    (funcall (ejab-make-presence `(to ,jid type "unsubscribe")) :send)
    (and (interactive-p)
         (y-or-n-p "Remove user from the roster as well?")
         (ejab-remove-roster-item jid))))

;; Confirmation is handled in `ejab-receive-presence'.

;;}}}

;;{{{ Receive Presence Tags

(defun ejab-receive-presence (obj)
  "Receive a presence tag and take appropriate action.
There are several different types of presence tags, meaning very
different things--see the JPG."
  (ejab-with (obj)
    (when (eq (obj :typeof) 'presence)
      ;; `case' uses `eql', and identical strings may not be `eql'.
      (case (intern (or (obj 'type) "available"))
        ((available unavailable)
         ;; Update the roster
         (ejab-roster-update-presence obj))
        (subscribe
         ;; Shall we accept this subscription request?
         (when (funcall ejab-subscription-requested-function obj)
           (ejab-accept-subscription-request obj)))
        (unsubscribe
         ;; Unsubscribe presences are FYI only (see JPG)
         (ejab-notify 1 "User %s has unsubscribed from your presence"
                      (obj 'from)))
        (subscribed
         ;; A subscription request of ours has been accepted.
         (ejab-notify 2 "User %s has accepted your subscription request"
                      (obj 'from)))
        (unsubscribed
         ;; We have successfully unsubscribed from someone.
         (ejab-notify 2 "Successfully unsubscribed from %s's presence"
                      (obj 'from)))
        (t
         ;; The other presence type, `probe', is server-only.
         (ejab-notify 3 "Presence tag with unknown type %s"
                      (obj 'type))))
      t)))

(add-hook 'ejab-receive-object-functions 'ejab-receive-presence)

;;}}}
;;{{{ Receive Subscription Requests

(defun ejab-default-subscription-requested-function (presence)
  "Query the user in a basic way about a subscription request.
Return non-nil if the request is accepted, nil otherwise."
  (y-or-n-p (format "Accept subscription requsest from %s? "
                    (funcall presence 'from))))

(defvar ejab-subscription-requested-function
  #'ejab-default-subscription-requested-function
  "Function called to decide whether to accept subscription requests.

When a subscription request is received, the value of this variable is
called as a function with one argument: an ejab OBJECT representing
the presence tag requesting subscription.  All this function really
needs to know is that the JID of the user requesting subscription can
be accessed via \(funcall OBJECT 'from).

If this function returns non-nil, then the subscription request is
immediately accepted.  However, in some cases it may be desirable for
this function to return and yet for the acceptance decision to be
postponed.  In this case, this function should return nil and store
its argument somewhere where the code that eventually makes the
acceptance decision \(or gets that decision from the user) can access
it.  If the request is to be denied, no action is necessary.  If it is
to be accepted, the code that determines this should call
\(ejab-accept-subscription-request OBJECT), where OBJECT is the
argument that `ejab-subscription-requested-function' was originally
called with.  Authors of this type of code should remember to provide
for multiple subscription requests being in progress at once.")

(defun ejab-accept-subscription-request (presence)
  "Accept the subscription request PRESENCE."
  (funcall (ejab-make-presence
            `((to . ,(obj 'from))
              (type . "subscribed")))
   :send))

;; Subscription requests are handled by `ejab-receive-presence', which
;; calls `ejab-subscription-requested-function' and then invokes
;; `ejab-accept-subscription-request' if it returns true.

;;}}}

(provide 'ejab-presence)

;;; ejab-presence.el ends here
