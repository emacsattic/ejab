;;; ejab-msg-disp.el --- Display incoming ejab messages

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

;; This file defines a major mode and associated functions to display
;; incoming Jabber messages.  Alternate ways to display messages are
;; easy to plug into `ejab-msg.el'.

;;; Code:

(require 'ejab-msg)
(require 'ejab-presence)

;;{{{ Message Display Mode

(defun ejab-message-mode ()
  "Major mode for displaying received Jabber messages.
The text is read-only and the following commands are available.

\\{ejab-message-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'ejab-message-mode
        mode-name "JMessage")
  (use-local-map ejab-message-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(ejab-msg-font-lock-keywords t t nil nil))
  (setq buffer-read-only t)
  (run-hooks 'ejab-message-mode-hook)
  )

;; Don't inherit mode in new buffers
(put 'ejab-message-mode 'mode-class 'special)

(defvar ejab-message-mode-map (make-keymap))
(suppress-keymap ejab-message-mode-map)

(define-key ejab-message-mode-map "r" 'ejab-new-reply)
(define-key ejab-message-mode-map "q" 'ejab-message-quit)
(define-key ejab-message-mode-map " " 'scroll-up)
(define-key ejab-message-mode-map [backspace] 'scroll-down)

;;}}}
;;{{{ New Message Display

(defun ejab-message-new-display (msg)
  "Display MSG in a new Ejab Message Mode buffer.
Display headers from the message object and any `jabber:x:envelope'
tag.  The envelope overrides the message object, but an overriden
`from' field becomes a `sender' field and an overridden `to' field
becomes a `sentto' field.  The returned buffer is not selected."
  (let ((env (find "jabber:x:envelope" (funcall msg :==> 'x)
                   :key (ejab-call 'xmlns) :test #'string=)))
    (ejab-with (msg env)
      (save-excursion
        (let ((from (or (and env (env 'from)) (msg 'from)))
              (to (or (and env (env 'to)) (msg 'to))))
          (set-buffer (generate-new-buffer "*JMessage*"))
          (when from
            ;; Insert `from' header
            (insert (format "From: %s\n" from))
            ;; Insert `sender' header if appropriate
            (and env (env 'from) (msg 'from)
                 (not (ejab-compare-nocase (env 'from) (msg 'from)))
                 (insert (format "Sender: %s\n" (msg 'from)))))
          (when to
            ;; Insert `to' header
            (insert (format "To: %s\n" to))
            ;; Insert `sentto' header if appropriate
            (and env (env 'to) (msg 'to)
                 (not (ejab-compare-nocase (env 'to) (msg 'to)))
                 (insert (format "Sent-To: %s\n" (msg 'to))))))
        (when env
          (dolist (header '(replyto forwardedby cc))
            (when (env header)
              (insert (format "%s: %s\n"
                              (capitalize (symbol-name header))
                              (env header))))))
        (when (msg 'subject)
          (insert (format "Subject: %s\n" (msg 'subject))))
        (insert "----\n")
        (insert (msg 'body))
        (ejab-message-mode)
        ;; See `ejab-current-message' for why we do this.
        (setq-default ejab-current-message msg)
        (set (make-local-variable 'ejab-current-message) msg)
        (goto-char (point-min))
        (current-buffer)))))

;;}}}
;;{{{ Quit Message Display

(defvar ejab-message-display-finished-hook nil
  "Normal hook run when a message display should be removed.
This hook should be used to restore the previous window state, kill a
popup frame, etc.  In such cases it is probably best localized with
`make-local-hook', and set by the function which initially rearranged
the windows, created the popup frame, etc.")

(defun ejab-message-quit ()
  "Stop displaying the current message."
  (interactive)
  (kill-buffer
   (prog1 (current-buffer)
          (run-hooks 'ejab-message-display-finished-hook))))

;;}}}

;;{{{ Display Incoming Messages

(defvar ejab-incoming-message-display-spec 'pop
  "How to display incoming message buffers.
See `ejab-display-buffer-by-spec' for allowable values.")

(defun ejab-display-message (msg)
  "Display an incoming message in `ejab-message-mode'.
See `ejab-incoming-message-display' for how the buffer is initally
displayed."
  (ejab-display-buffer-by-spec (ejab-message-new-display msg)
                               ejab-incoming-message-display-spec
                               'ejab-message-display-finished-hook))

;;}}}
;;{{{ Incoming Message Queue

(defvar ejab-message-queue nil
  "Queue of incoming messages.
Each element is a `message' object waiting to be displayed.  The front
of the list is the front of the queue.  New messages should be added
to the end with `ejab-queue-message', and displayed messages removed
with `ejab-pop-next-queued-message' or `ejab-delete-queued-message'.")

(defvar ejab-message-enqueued-hooks '()
  "Abnormal hook run when a message is enqueued.
Functions receieve one argument, the enqueued message.")

(defvar ejab-message-dequeued-hooks '()
  "Abnormal hook run when a message is dequeued \(popped or deleted).
Functions receieve one argument, the dequeued message.")

(defun ejab-enqueue-message (msg)
  "Add MSG to the message queue, `ejab-message-queue'."
  (if ejab-message-queue
      (setcdr (last ejab-message-queue) (list msg))
    (setq ejab-message-queue (list msg)))
  (run-hook-with-args 'ejab-message-enqueued-hooks msg))

(defun ejab-pop-next-queued-message ()
  "Pop and return the next message in `ejab-message-queue'."
  (let ((msg (pop ejab-message-queue)))
    (run-hook-with-args 'ejab-message-dequeued-hooks msg)
    msg))

(defun ejab-delete-queued-message (msg)
  "Delete MSG from the message queue.
If MSG was not in the message queue, return nil, otherwise non-nil."
  (let ((new-queue (delete msg ejab-message-queue)))
    (unless (equal new-queue ejab-message-queue)
      (setq ejab-message-queue new-queue)
      (run-hook-with-args 'ejab-message-dequeued-hooks msg)
      t)))

(defun ejab-display-next-message ()
  "Display the next message in the message queue."
  (interactive)
  (if ejab-message-queue
      (ejab-display-message (ejab-pop-next-queued-message))
    (ejab-notify 3 "Empty message queue")))

(defun ejab-notify-new-message (msg)
  "Notify the user of a newly received message."
  (ejab-notify 2 "New message from %s%s"
               ;; Display by name if possible
               (let ((item (ejab-get-item (funcall msg 'from))))
                 (or (and item
                          (funcall item 'name))
                     (funcall msg 'from)))
               ;; Display the subject if any
               (if (funcall msg 'subject)
                   (format ": `%s'" (funcall msg 'subject))
                 "")))

(add-hook 'ejab-receive-message-hooks 'ejab-notify-new-message)

;;}}}

(provide 'ejab-msg-disp)

;;; ejab-msg-disp.el ends here
