;;; ejab-misc.el --- key and menu bindings and mode line for ejab

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

;; This file defines key sequences and menu bindings for EJab.  It
;; also defines the mode line format.

;;; Code:

(require 'cl)

;;{{{ Keymap

;; First, define the keymap
(defvar ejab-keymap (make-sparse-keymap))
(mapc #'(lambda (pair)
          (apply #'define-key ejab-keymap pair))
      `(("m" ejab-new-message)
        ("r" ejab-new-reply)

        ("c" ejab-connect)
        ("d" ejab-disconnect)

        (" " ejab-display-next-message)
        ("n" ejab-display-next-message)

        ("y" ejab-set-priority)
        ("a" ejab-toggle-presence-away)
        ("x" ejab-toggle-presence-xa)
        ("D" ejab-toggle-presence-dnd)
        ("C" ejab-toggle-presence-chat)

        ("t" ejab-toggle-hide-offline)

        ("l" ejab-display-roster)
        ("s" ejab-request-subscription)
        ("e" ejab-edit-roster-item)
        ("u" ejab-request-unsubscription)

        ("h" ejab-describe-object)
        ("\C-t" ejab-toggle-debugging)
        ))

;; Now, we decide where to put it.
(defvar ejab-global-map-prefix "\C-c!"
  "Prefix for Ejab global keymap.
If nil, ejab commands are not bound to any global key sequences.
Beware of reserved key sequences when changing this variable: the only
key sequences reserved for users are `C-c LETTER' and function keys
F5-F9, although many other key sequences have no definition in vanilla
Emacs.  This variable's value should be set before ejab is loaded, or
else call `ejab-rebind-global-map' after setting it.")

(defun ejab-rebind-global-map ()
  (when ejab-global-map-prefix
    (define-key global-map ejab-global-map-prefix ejab-keymap)))

(ejab-rebind-global-map)

;;}}}
;;{{{ Menus

(defvar ejab-global-menu-map (make-sparse-keymap)
  "Keymap to display Ejab global menu.")

(easy-menu-define
 ejab-menu ejab-global-menu-map
 "The Ejab Menu, both global and in ejab frames."
 '("EJab"
   ("Send"
    ["Connect" ejab-connect (not ejab-connection)]
    ["Disconnect" ejab-disconnect ejab-connection]
    ["Agents" xxx nil]
    "---"
    ["New Message" ejab-new-message t]
    ["Reply" ejab-new-reply ejab-current-message]
    ["Display Message" ejab-display-next-message ejab-message-queue]
    )
   ("Roster"
    ["Display Roster" ejab-display-roster ejab-connection]
    ["Add User" ejab-request-subscription ejab-connection]
    ["Edit User" ejab-edit-roster-item ejab-connection]
    ["Remove User" ejab-request-unsubscription ejab-connection]
    "---"
    ["Hide Offline Users" ejab-toggle-hide-offline
     :style toggle :selected (ejab-hide-offline-p)]
    ["Hide DND Users" ejab-toggle-hide-dnd
     :style toggle :selected (ejab-hide-dnd-p)]
    ["Hide N/A Users" ejab-toggle-hide-xa
     :style toggle :selected (ejab-hide-xa-p)]
    ["Hide Away Users" ejab-toggle-hide-away
     :style toggle :selected (ejab-hide-away-p)]
    )
   ("Status"
    ["Available" ejab-clear-presence :style radio
     :selected (not ejab-current-presence-show)
     :active ejab-connection]
    ["Away" ejab-set-presence-away :style radio
     :selected (string= ejab-current-presence-show "away")
     :active ejab-connection]
    ["Extended Away" ejab-set-presence-xa :style radio
     :selected (string= ejab-current-presence-show "xa")
     :active ejab-connection]
    ["DND" ejab-set-presence-dnd :style radio
     :selected (string= ejab-current-presence-show "dnd")
     :active ejab-connection]
    ["Free for Chat" ejab-set-presence-chat :style radio
     :selected (string= ejab-current-presence-show "chat")
     :active ejab-connection]
    )
   ))

(defvar ejab-display-global-menu t
  "Non-nil means display the EJab global menu bar.")
(add-to-list 'minor-mode-map-alist
             (cons 'ejab-display-global-menu ejab-global-menu-map))

(defun ejab-take-over-menu-bar (&optional keymap)
  "Take over the menu bar for ejab in KEYMAP.
If nil, KEYMAP defaults to the value of `current-local-map'.
`ejab-display-global-menu' should also be set to nil in buffers that
use such a keymap."
  (or keymap (setq keymap (current-local-map)))
  (dolist (menu '(buffer files options tools edit search mule))
    (define-key keymap (vector 'menu-bar menu) 'undefined))
  (define-key keymap [menu-bar] ejab-menu))

;;}}}
;;{{{ Mode Line

;; User variables
(defvar ejab-mode-line-string "J:"
  "String to display in the mode line when EJab is loaded.
This is followed by the current username, if any, the current status,
if not `available', and the number of queued messages, if any.")

(defvar ejab-mode-line-disconnected-string "--")
(defvar ejab-mode-line-unauthenticated-string "??")

(defvar ejab-mode-line-format
  '("" ejab-mode-line-string
    (ejab-authenticated ejab-current-username
                        (ejab-connection
                         ejab-mode-line-unauthenticated-string
                         ejab-mode-line-disconnected-string))
    (ejab-current-presence-show ("{" ejab-current-presence-show "}"))
    (ejab-queue-count ("[" ejab-queue-count "]"))))

;; Set up the global-mode-string
(cond
 ;; Don't do it twice
 ((and (consp global-mode-string)
       (memq 'ejab-mode-line-format global-mode-string)))
 (global-mode-string
  (setq global-mode-string
        (list "" 'ejab-mode-line-format " " global-mode-string)))
 (t
  (setq global-mode-string (list "" 'ejab-mode-line-format " "))))

(defvar ejab-queue-count nil
  "Number of currently queued messages, an integer, or nil if none.")
(defun ejab-increment-queue-count (msg)
  (setq ejab-queue-count
        (if ejab-queue-count
            (int-to-string (1+ (string-to-int ejab-queue-count)))
          "1")))
(defun ejab-decrement-queue-count (msg)
  (setq ejab-queue-count
        (if (or (not ejab-queue-count)
                (string= ejab-queue-count "1"))
            nil
          (int-to-string (1- (string-to-int ejab-queue-count))))))
(add-hook 'ejab-message-enqueued-hooks 'ejab-increment-queue-count)
(add-hook 'ejab-message-dequeued-hooks 'ejab-decrement-queue-count)

;;}}}

(provide 'ejab-misc)

;;; ejab-misc.el ends here
