;;; ejab-msg.el --- ejab message handling functions

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

;; This file defines variables and functions to handle messages in
;; ejab.  It sets up hooks and defines a simple mode for displaying
;; messages.  The standard ways to compose messages are defined in
;; `ejab-draft.el', and ways to display messages in `ejab-msg-disp.el',
;; but this file is easy to configure with replacements.

;;; Code:

(require 'cl)
(require 'ejab-utils)

;;{{{ Message Recipients

(defun ejab-msg-parse-recipients (string)
  "Return a list of plain JIDs that STRING specifies sending to.
STRING should be comma-delimited and allows the following formats:

- Simple text without commas <user@host/resource>
- \"Anything but a double quote\" <user@host/resource>
- user@host/resource (No right parentheses or double quotes)

If STRING is nil, return nil."
  (when string
    ;; First kill the quoted and parenthesized text.
    (while (string-match "\"[^\"]*\"" string)
      (setq string (replace-match "" nil nil string)))
    (while (string-match "([^)]*)" string)
      (setq string (replace-match "" nil nil string)))
    ;; Now split and remove remaining text entry-by-entry
    (mapcar #'(lambda (entry)
                (if (string-match "<\\([^>]+\\)>" entry)
                    (match-string 1 entry)
                  (and (string-match "\\s-*\\(\\S-+\\)\\s-*" entry)
                       (match-string 1 entry))))
            (split-string string "[,]"))))

(defun ejab-message-recipients (msg)
  "Return a list of all JIDs that MSG is/was sent to.
Understands jabber:x:envelope and calls `ejab-msg-parse-recipients' on
both `to' and `cc' fields.
For internal use only, programs should call \(MSG :recipients)."
  (remove-duplicates
   (let ((envelope (find "jabber:x:envelope" (funcall msg :==> 'x)
                         :test #'string= :key (ejab-call 'xmlns))))
     (if envelope
         (append (ejab-msg-parse-recipients (funcall envelope 'to))
                 (ejab-msg-parse-recipients (funcall envelope 'cc)))
       (ejab-msg-parse-recipients (funcall msg 'to))))
   :test #'ejab-compare-nocase))

;;}}}

;;{{{ Receive Messages

(defvar ejab-receive-message-hooks '()
  "Abnormal hook run when a message is received.
Functions in this hook receive one argument: the received message as
an ejab `message' object, whose primary interesting attributes include
`from', `type', `subject', and `body'.  This hook is not for primary
message handling, see `ejab-default-receive-message-function' and
`ejab-receive-message-functions' for that.")

(defvar ejab-receive-message-functions '()
  "Abnormal hook run to handle received messages.
Functions in this hook receive one argument: the received message as
an ejab `message' object, whose primary interesting attributes
include `from', `type', `subject', and `body'.  They should return
non-nil if they have handled the message in an appropriate way and nil
otherwise.

Functions in this hook should never handle all messages.  They are
primarily for filtering certain messages and similar goals.  Generic
messages should be handled by `ejab-default-receive-message-function',
which is called if no function in this hook returns non-nil.")

(defvar ejab-default-receive-message-function 'ejab-enqueue-message
  "Function to handle most received messages.
If no function in `ejab-receive-message-functions' handles a given
message, then the value of this variable is called with one argument:
the received message as an ejab `message' object, whose primary
interesting attributes include `from', `type', `subject', and `body'.")

(defun ejab-receive-message (obj)
  "Receive a message object.
Return non-nil if OBJ is a message object and was handled."
  (when (eq (funcall obj :typeof) 'message)
    (run-hook-with-args 'ejab-receive-message-hooks obj)
    (or (run-hook-with-args-until-success
         'ejab-receive-message-functions obj)
        (funcall ejab-default-receive-message-function obj))
    t))

(add-hook 'ejab-receive-object-functions 'ejab-receive-message)

;;}}}
;;{{{ Send Messages

;; No special functions are required to send messages; simply make a
;; message with `ejab-make-message' and then call its :send method.
;; See `ejab-draft.el', however.

(defun ejab-new-message (&optional headers)
  "Begin composing a new Jabber message.
Any plugin providing a way to create a new message should bind it to
this command and then customize `ejab-new-message-function', so that
the user can override it without rebinding keys.  HEADERS is as in
`ejab-draft-new-message'."
  (interactive)
  (funcall ejab-new-message-function headers))

(defvar ejab-new-message-function 'ejab-compose-message
  "Function called to begin composing a new message.
It will probably want to call `ejab-draft-new-message' and display the
returned buffer somehow.  It receives one argument HEADERS as is given
to `ejab-draft-new-message'.")

(defvar ejab-before-send-message-hooks nil
  "Abnormal hook run before sending a message.
Functions receive one argument, the ejab `message' object about to be
sent, and can modify it if they so desire.")

(defun ejab-before-send-message (obj)
  "Run `ejab-before-send-message-hooks' on messages."
  (when (eq (funcall obj :typeof) 'message)
    (run-hook-with-args 'ejab-before-send-message-hooks obj)))

(add-hook 'ejab-before-send-object-hooks 'ejab-before-send-message)

;;}}}
;;{{{ Send Replies

(defvar ejab-current-message nil
  "The most current received message, mostly for replying purposes.
The default value of this variable should be the most recent message
that was displayed, but in message display buffers it should have a
buffer-local value of the message displayed in that buffer.  This is
normally handled by `ejab-message-new-display'.")

(defvar ejab-reply-original-message nil
  "The ejab message object being replied to in a given buffer, if any.")
(make-variable-buffer-local 'ejab-reply-original-message)

(defun ejab-new-reply (msg)
  "Begin composing a reply to a Jabber message.
Any plugin allowing the user to create a new message should call this
command and then customize `ejab-new-reply-function'.  However, an
intermediary command may be invoked first which knows how to get the
correct message object to reply to.  When called interactively, MSG is
set to `ejab-current-message', which is often best."
  (interactive (list ejab-current-message))
  (if msg
      (funcall ejab-new-reply-function msg)
    (ejab-notify 3 "No message to reply to")))

(defvar ejab-new-reply-function 'ejab-compose-reply
  "Function to begin composing a new reply.
Called with one argument, the message object to reply to.  It will
probably want to call `ejab-draft-new-reply' and then display the
buffer somehow.")

;;}}}
;;{{{ Get Reply Headers

(defun ejab-reply-get-to (msg)
  "Return the address to which replies to MSG should go.
If MSG uses the `jabber:x:envelope' extension, this is its `replyto'
field, or its `from' field if there is none.  If not, or if neither
exist, MSG's `from' attribute is used."
  (let ((envelope (find "jabber:x:envelope" (funcall msg :==> 'x)
                        :test #'string= :key (ejab-call 'xmlns))))
    (if envelope
        (or (funcall envelope 'replyto)
            (funcall envelope 'from)
            (funcall msg 'from))
      (funcall msg 'from))))

(defvar ejab-reply-to-all nil
  "If non-nil, replies go by default to all recipients.
A prefix argument passed to \\[ejab-new-reply] inverts this value.")

(defun ejab-reply-get-cc (msg)
  "Return the addresses that should be `cc'ed in a reply to MSG.
If `ejab-reply-to-all' XOR `current-prefix-arg', replies to all other
recipients, else just to the sender."
  (if ;; This is an XOR
      (if ejab-reply-to-all
          (not current-prefix-arg)
        current-prefix-arg)
      ;; Reply to all other recipients
      (remove*
       (format "%s@%s" ejab-current-username ejab-current-server)
       (funcall msg :recipients)
       :key #'ejab-extract-jid)
    ;; Reply to sender only
    nil))

(defvar ejab-reply-subject-prefix "Re: ")
(defvar ejab-reply-subject-prefix-regexp "^re: *")

(defun ejab-reply-get-subject (msg)
  "Return an appropriate subject for replying to MSG.
If MSG's subject matches `ejab-reply-subject-prefix-regexp', it is
returned as-is, otherwise `ejab-reply-subject-prefix' is prepended.
If MSG has no subject, return nil."
  (when (funcall msg 'subject)
    (let ((case-fold-search t))
      (if (string-match ejab-reply-subject-prefix-regexp
                        (funcall msg 'subject))
          (funcall msg 'subject)
        (concat ejab-reply-subject-prefix (funcall msg 'subject))))))

(defun ejab-reply-get-thread (msg)
  (funcall msg 'thread))

;;}}}

;;{{{ Message Font Lock

;; Here we define font-lock which is used for both displaying incoming
;; messages and composing new messages.  Thus `ejab-draft.el' requires
;; this file.  Add Bcc.

;; We have cases folded.
(defvar ejab-msg-font-lock-keywords
  '((ejab-msg-match-to-header
     (1 'ejab-msg-to-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-sentto-header
     (1 'ejab-msg-to-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-cc-header
     (1 'ejab-msg-cc-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-bcc-header
     (1 'ejab-msg-bcc-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-from-header
     (1 'ejab-msg-from-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-sender-header
     (1 'ejab-msg-from-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-replyto-header
     (1 'ejab-msg-replyto-header-face)
     (2 'ejab-msg-jid-face))
    (ejab-msg-match-subject-header
     (1 'ejab-msg-subject-header-face)
     (2 'ejab-msg-subject-face))
    (ejab-msg-match-thread-header
     (1 'ejab-msg-thread-header-face)
     (2 'ejab-msg-thread-face))
    (ejab-msg-match-boundary
      (0 'ejab-msg-boundary-face))
    ("^ *\\(\\w*\\)\\(>\\) *\\(.*\\)"
     (1 'ejab-msg-citation-face)
     (2 'ejab-msg-citation-mark-face)
     (3 'ejab-msg-cited-text-face))))

(dolist (header '("to" "sentto" "from" "sender"
                  "cc" "bcc" "replyto" "subject" "thread"))
  (fset (intern (format "ejab-msg-match-%s-header" header))
        `(lambda (bound)
           (and (save-excursion
                  (ejab-msg-match-boundary (point-max)))
                (re-search-forward
                 ,(format "^\\(%s:\\) *\\(.*\\)" header)
                 (min bound (match-beginning 0))
                 t)))))

(defun ejab-msg-match-boundary (bound)
  (and (save-excursion
         (goto-char (point-min))
         (re-search-forward "^-+" bound t))
       (<= (point) (match-beginning 0))
       (goto-char (match-end 0))))

(defface ejab-msg-to-header-face
  '((t (:bold t :foreground "DarkGreen")))
  "")
(defface ejab-msg-cc-header-face
  '((t (:bold t :foreground "Orchid")))
  "")
(defface ejab-msg-bcc-header-face
  '((t (:bold t :foreground "Cyan")))
  "")
(defface ejab-msg-from-header-face
  '((t (:bold t :foreground "Black")))
  "")
(defface ejab-msg-replyto-header-face
  '((t (:bold t :foreground "Coral")))
  "")
(defface ejab-msg-jid-face
  '((t (:underline t)))
  "")
(defface ejab-msg-subject-header-face
  '((t (:bold t :foreground "Brown")))
  "")
(defface ejab-msg-subject-face
  '((t (:bold t)))
  "")
(defface ejab-msg-thread-header-face
  '((t (:bold t :foreground "Orange")))
  "")
(defface ejab-msg-thread-face
  '((t (:foreground "Grey")))
  "")
(defface ejab-msg-boundary-face
  '((t (:bold t :foreground "Red")))
  "")
(defface ejab-msg-citation-face
  '((t (:foreground "Violet")))
  "")
(defface ejab-msg-citation-mark-face
  '((t (:foreground "DarkGreen")))
  "")
(defface ejab-msg-cited-text-face
  '((t (:foreground "Blue")))
  "")

;;}}}

(provide 'ejab-msg)

;;; ejab-msg.el ends here
