;;; ejab-draft.el --- ejab draft mode for composing messages

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

;; This file defines a major mode and associated functions for
;; composing Jabber messages.  Use of this mode is not necessary to
;; send messages--all that is required is some way to input the
;; message and then (funcall (ejab-make-message ...) :send)--but this
;; provides a convenient and familiar interface.

;;; Code:

(require 'cl)
(require 'ejab-obj)
(require 'ejab-msg)

;; BBDB integration?

;;{{{ Major Mode

;; This function does not add any text to the buffer; the various
;; compose-message and reply-to-message functions do that.

(define-derived-mode ejab-draft-mode text-mode "JDraft"
  "Major mode for composing Jabber messages.
Based on `text-mode' so all text editing commands are available.
Additional commands include:\\<ejab-draft-mode-map>

\\[ejab-draft-send]	Send the draft being composed.
\\[ejab-draft-cancel]	Cancel the draft being composed.

An Ejab Draft Mode buffer must contain a header section, very similar
to that often seen in various email composition buffers.  Available
headers include `to', `from', `cc', `bcc', `replyto', `thread', and
`subject'.  The `from' and `replyto' headers only affect the
`jabber:x:envelope' tag that Ejab automatically includes and will be
invisible to clients not supporting that extension.  The `to' and `cc'
headers are copied to the `jabber:x:envelope' tag, and copies of the
message is sent to all addresses in both, as well as addresses in the
`bcc' field.  Such addresses should be comma-delimited and can have
attached commentary \(see `ejab-draft-parse-recipients').  Only the
`to' header is mandatory."
  (set (make-local-variable 'font-lock-defaults)
       '(ejab-msg-font-lock-keywords t t nil nil))
  (set (make-local-variable 'paragraph-start)
        "^-+$\\|[ 	\n\f]")
  (set (make-local-variable 'paragraph-separate)
       "^-+$\\|[ 	\f]*$")
  (add-hook (make-local-hook 'post-command-hook)
            'ejab-adjust-fill-column nil 'local)
  )

(define-key ejab-draft-mode-map "\C-c\C-c" 'ejab-draft-send)
(define-key ejab-draft-mode-map "\C-c\C-q" 'ejab-draft-cancel)

(defvar ejab-draft-goto-header-map (make-sparse-keymap))
(define-key ejab-draft-mode-map "\C-c\C-f" ejab-draft-goto-header-map)
(mapcar* #'(lambda (key header)
             (define-key ejab-draft-goto-header-map key
               (intern (format "ejab-draft-goto-%s-header" header))))
         '("\C-t" "\C-f" "\C-c" "\C-b"  "\C-r"    "\C-s"    "\C-d")
         '( "to"  "from"  "cc"  "bcc"  "replyto" "subject" "thread"))

(define-key ejab-draft-mode-map "\C-c\C-t" 'ejab-draft-goto-text)

;;}}}
;;{{{ Move to Headers/Text

(dolist (header '("to" "from" "cc" "bcc" "replyto" "subject" "thread"))
  (fset (intern (format "ejab-draft-goto-%s-header" header))
        `(lambda ()
           ,(format "Go to the %s header, creating it if absent." header)
           (interactive)
           (goto-char (point-min))
           (cond
            ((re-search-forward ,(format "^%s: .*" header) nil t)
             (while (looking-at "\n ")
               (forward-line 1)
               (end-of-line)))
            ((re-search-forward "^-+$" nil t)
             (beginning-of-line)
             (insert ,(format "%s: \n" header))
             (backward-char))
            (t (ejab-notify
                3 ,(format "No %s header or header boundary found"
                           header)))))))

(defun ejab-draft-goto-text ()
  "Go to the start of the body text of the message."
  (interactive)
  (if (re-search-forward "^-+$" nil t)
      (forward-char 1)
    (ejab-notify 3 "No header boundary found")))

;;}}}
;;{{{ Send Message

(defvar ejab-draft-finished-hook nil
  "Hook run after a draft is killed \(usually sent or canceled).
The draft can be sent without killing the buffer or running this hook,
however.  This hook should be used to restore the previous window
state, kill a popup frame, etc.  In such cases it is probably best
localized with `make-local-hook', and set by the function which
initially rearranged the windows, created the popup frame, etc.")

(defun ejab-draft-get-header (header)
  "Return the value of HEADER in the current draft message, or nil."
  (let* ((case-fold-search t)
         (value (and (goto-char (point-min))
                     (re-search-forward
                      (format "^%s: *\\(.+\\)" header) nil t)
                     (match-string 1))))
    (when value
      ;; Look for continuation lines.
      (forward-line 1)
      (beginning-of-line)
      (while (looking-at " +\\(.+\\)")
        (setq value (concat value (match-string 1)))
        (forward-line 1)))
    value))

(defun ejab-draft-send (preserve)
  "Send the current Ejab Draft Mode buffer as a Jabber message.
If PRESERVE is non-nil \(prefix arg interactively), do not kill the
draft buffer or restore previous configuration after sending."
  (interactive "P")
  (let ((case-fold-search t) body)
    (destructuring-bind (to from cc bcc replyto subject thread)
        (mapcar #'ejab-draft-get-header
                '(to from cc bcc replyto subject thread))
      (setq body (and (goto-char (point-min))
                      (re-search-forward "^-+\n" nil t)
                      (buffer-substring (point) (point-max))))
      (cond
       ((or (not to) (string= to ""))
        (ejab-notify 3 "No `to' header supplied"))
       ((not body)
        (ejab-notify 3 "No header boundary found"))
       (t
        ;; Send to each recipient specified in `to' or `cc' fields.
        (dolist (recipient (append (ejab-msg-parse-recipients to)
                                   (ejab-msg-parse-recipients cc)
                                   (ejab-msg-parse-recipients bcc)))
          (funcall
           (ejab-make-message
            `(to ,recipient type "normal")
            `(body ,body subject ,subject thread ,thread
                   x ,(ejab-make-x
                       '(xmlns "jabber:x:envelope")
                       `(to ,to from ,from cc ,cc replyto ,replyto))))
           :send))
        (unless preserve
          (kill-buffer
           (prog1 (current-buffer)
                  (run-hooks 'ejab-draft-finished-hook))))
        (ejab-notify 1 "Draft message sent"))))))

;;}}}
;;{{{ Cancel Message

(defun ejab-draft-cancel ()
  "Cancel the current draft Jabber message being composed."
  (interactive)
  (when (or (not (interactive-p))
            (y-or-n-p "Cancel draft message? "))
    (kill-buffer
     (prog1 (current-buffer)
            (run-hooks 'ejab-draft-finished-hook)))
    (ejab-notify 1 "Draft message canceled")))

;;}}}

;;{{{ Create New Message

(defvar ejab-draft-header-defaults
  '((to . "")
    (from . ejab-current-jid)
    (subject . nil)
;;;    (thread . (lambda () (format "%d" (abs (random)))))
    (thread . nil)
    (cc . nil)
    (replyto . nil))
  "Default values for header fields in new messages.
Each element looks like \(HEADER . METHOD) where HEADER is a symbol
representing a header field \(`to', `from', `cc', `replyto', `subject',
or `thread') and METHOD is one of:

* nil, meaning no such field should be inserted.
* a string, the value to insert.  \"\" makes an empty field.
* the symbol `prompt', meaning to prompt the user for a value.
* a function, which should return one of the above values.

Note that `ejab-draft-new-message' leaves point at the first empty
header field, or the body if there is none.

This variable is only used for new messages.  For replies to other
users' messages, see `ejab-draft-reply-header-defaults'.")

(defun ejab-draft-new-message (&optional headers)
  "Create a new message buffer in Ejab Draft Mode.
If HEADERS is not supplied, header fields are filled in according to
`ejab-draft-header-defaults', otherwise elements of HEADERS override
the default headers.  Do not select the buffer but merely return it."
  (save-excursion
    (set-buffer (generate-new-buffer "*JDraft*"))
    (let ((endmark (make-marker))
          header method)
      (dolist (pair ejab-draft-header-defaults)
        ;; Get the overriding headers if given
        (when (assq (car pair) headers)
          (setq pair (assq (car pair) headers)))
        (setq header (car pair)
              method (cdr pair))
        (while (functionp method)
          (setq method (funcall method)))
        (cond
         ((not method))
         ((stringp method)
          (insert (format "%s: %s\n"
                          (capitalize (symbol-name header))
                          method))
          (and (string= method "")
               (not (marker-position endmark))
               (set-marker endmark (1- (point)))))
         ((eq method 'prompt)
          (insert
           (format "%s: %s\n" (capitalize (symbol-name header))
                   (read-string
                    (format "%s: " (capitalize (symbol-name header)))))))
         (t (ejab-notify 3 "Invalid header method: %s" method))))
      (insert "----\n")
      (when (marker-position endmark)
        (goto-char endmark)))
    (ejab-draft-mode)
    (current-buffer)))

;;}}}
;;{{{ Create New Reply

(defvar ejab-draft-reply-header-defaults
  '((to . ejab-reply-get-to)
    (from . ejab-current-jid)
    (subject . ejab-reply-get-subject)
    (thread . ejab-reply-get-thread)
    (cc . ejab-reply-get-cc)
    (replyto . nil))
  "Default values for header fields in new messages.
Each element looks like \(HEADER . METHOD) where HEADER is a symbol
representing a header field \(`to', `from', `cc', `replyto', `subject',
or `thread') and METHOD is one of:

* nil, meaning no such field should be inserted.
* a string, the value to insert.  \"\" makes an empty field.
* the symbol `prompt', meaning to prompt the user for a value.
* a function, which should return one of the above values when applied
  to the ejab message object being replied to.

Note that `ejab-draft-new-reply' leaves point at the first empty
header field, or the body if there is none.")

(defun ejab-draft-new-reply (msg)
  "Create a new buffer in Ejab Draft Mode to reply to MSG.
Consult the variable `ejab-draft-reply-header-defaults' to set initial
header fields.  Do not select the buffer, just return it."
  (save-excursion
    (set-buffer (generate-new-buffer "*JDraft*"))
    (let ((endmark (make-marker))
          header method)
      (dolist (pair ejab-draft-reply-header-defaults)
        (setq header (car pair)
              method (cdr pair))
        (while (functionp method)
          (setq method (funcall method msg)))
        (cond
         ((not method))
         ((stringp method)
          (insert (format "%s: %s\n"
                          (capitalize (symbol-name header))
                          method))
          (and (string= method "")
               (not (marker-position endmark))
               (set-marker endmark (1- (point)))))
         ((eq method 'prompt)
          (insert
           (format "%s: %s\n"
                   (capitalize (symbol-name header))
                   (read-string
                    (format "%s: " (capitalize (symbol-name header)))))))
         (t (ejab-notify 3 "Invalid header method: %s" method))))
      (insert "----\n")
      (when (marker-position endmark)
        (goto-char endmark)))
    (ejab-draft-mode)
    (setq ejab-reply-original-message msg)
    (current-buffer)))

;;}}}
;;{{{ Citing Original Messages

(defvar ejab-draft-cite-headers '(from subject)
  "List of headers to copy when citing a replied message.")

(defvar ejab-draft-use-mail-citation-hook nil
  "Whether `ejab-draft-yank-and-cite' should use `mail-citation-hook'.
Normally, this is nil, since Jabber messages are not email and there
is usually very little need for the functionality of packages such as
SuperCite.  In this case, `ejab-draft-cite-hook' is used instead.")

(defvar ejab-draft-cite-hook 'ejab-draft-cite-original
  "Hook run to cite replied messages.
This hook is run if `ejab-draft-use-mail-citation-hook' is nil, or if
is non-nil and `mail-citation-hook' is nil.")

(defun ejab-draft-yank-and-cite ()
  "When replying to a message, cite the original message.
Copies its body, with a few headers \(see `ejab-draft-cite-headers'),
then runs `ejab-draft-cite-hook', or `mail-citation-hook' if
`ejab-draft-use-mail-citation-hook' is non-nil."
  (interactive)
  (unless ejab-reply-original-message
    (ejab-notify 3 "No original message to cite"))
  (let ((beg (point)))
    (dolist (header ejab-draft-cite-headers)
      (insert (format "%s: %s\n"
                      (cdr (assq header ejab-draft-header-display))
                      (funcall ejab-reply-original-message header))))
    (insert "\n" (funcall ejab-reply-original-message 'body))
    (push-mark (point))
    (goto-char beg))
  (cond
   ;; Use `mail-citation-hook' if so instructed and it is non-nil.
   ((and ejab-draft-use-mail-citation-hook
         (boundp 'mail-citation-hook)
         mail-citation-hook)
    (run-hooks 'mail-citation-hook))
   ;; Use our own hook, if it is non-nil.
   (ejab-draft-cite-hook
    (run-hooks 'ejab-draft-cite-hook))
   ;; Otherwise, provide a good default.
   (t
    (ejab-draft-cite-original))))

;; Allow customizing this more?
(defun ejab-draft-cite-original ()
  "Cite an already copied replied-to message simply and sensibly.
Just change the header to a simple attribution and insert `> ' in
front of every body text line."
  ;; Kill header lines, leaving an attribution
  (while (looking-at "\\(.+\\): \\(.*\\)")
    (when (string= (downcase (match-string 1)) "to")
      (insert (format "%s wrote:\n" (match-string 2))))
    (delete-region (point) (save-excursion (forward-line) (point))))
  ;; Kill blank lines
  (while (looking-at "^ *$")
    (delete-region (point) (save-excursion (forward-line) (point))))
  ;; Cite the rest of it
  (while (< (point) (mark))
    (insert "> ")
    (forward-line 1)
    (beginning-of-line)))

;;}}}

;;{{{ Display New Messages

(defvar ejab-compose-message-display-spec 'pop
  "How to display new message buffers created by \\[ejab-compose-message].
For allowable values, see `ejab-display-buffer-by-spec'.")

(defun ejab-compose-message (&optional headers)
  "Create and display a new message composition buffer.
See `ejab-draft-header-defaults' for how the header fields are
initially filled in, and `ejab-compose-message-display' for how the
buffer is initally displayed.  The buffer is in `ejab-draft-mode'."
  (ejab-display-buffer-by-spec (ejab-draft-new-message headers)
                               ejab-compose-message-display-spec
                               'ejab-draft-finished-hook))

;;}}}
;;{{{ Display New Replies

(defvar ejab-compose-reply-display-spec 'pop
  "How to display new reply buffers created by \\[ejab-compose-reply].
See `ejab-display-buffer-by-spec' for allowable values.")

(defun ejab-compose-reply (msg)
  "Create and display a new reply \(to MSG) composition buffer.
See `ejab-draft-header-defaults' for how the header fields are
initially filled in, and `ejab-compose-reply-display' for how the
buffer is initally displayed.  The buffer is in `ejab-draft-mode'."
  (ejab-display-buffer-by-spec (ejab-draft-new-reply msg)
                               ejab-compose-reply-display-spec
                               'ejab-draft-finished-hook))

;;}}}

(provide 'ejab-draft)

;;; ejab-draft.el ends here
