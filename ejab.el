;;; ejab.el --- Jabber client for Emacs

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Package: ejab
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Keywords: comm
;; Version: 0.1.0-dev

;; Revision: $Id$

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

;; This file and those that accompany it define a Jabber client for
;; Emacs called Ejab.  Ejab is highly configurable and receptive to
;; plugins for additional functionality.

;; This file loads all the other core files, and defines a "style"
;; facility, to allow users to customize many aspects of EJab at once.
;; It also runs `ejab-load-hook' if this is the first load.

;;; Code:

(require 'cl)
(require 'ejab-utils)
(require 'ejab-misc)
(require 'ejab-obj)
(require 'ejab-xml)
(require 'ejab-connect)
(require 'ejab-msg)
(require 'ejab-roster)
(require 'ejab-presence)
(require 'ejab-draft)
(require 'ejab-msg-disp)
(require 'ejab-rost-disp)

;;{{{ Display Styles

(defvar ejab-styles-alist
  '(("standard"
     (ejab-roster-display-spec pop)
     (ejab-incoming-message-display-spec nil)
     (ejab-compose-message-display-spec pop)
     (ejab-compose-reply-display-spec pop)
     (ejab-default-receive-message-function ejab-enqueue-message))
    ("popup"
     (ejab-roster-display-spec
      ((width . 20)
       (height . 16)
       ))
     (ejab-incoming-message-display-spec
      ((width . 50)
       (height . 20)
       ))
     (ejab-compose-message-display-spec
      ((width . 50)
       (height . 20)
       ))
     (ejab-compose-reply-display-spec pop)
     (ejab-default-receive-message-function ejab-display-message)))
  "Alist of display styles.
Each element looks like \(STYLE BINDING ...) where STYLE is a a string
naming the style and each BINDING is of one of the forms:

1. \(VARIABLE VALUE).  When STYLE is enabled, VARIABLE is set to VALUE.
2. \(call ON OFF).  When STYLE is enabled, the function ON is called
   with no arguments, and when it is disabled, OFF is called.
3. \(hook HOOK FUNCTION).  When STYLE is enabled, FUNCTION is added to
   HOOK with `add-hook', and when disabled, removed with `remove-hook'.

Style switching is done with `ejab-use-style', which reverts the style
to `standard' in between each change.  The `standard' style should
thus define suitable defaults for all VARIABLEs that any style sets.")

;; This defaults to `standard' because all the default values of the
;; `defvar's correspond to that.
(defvar ejab-current-style "standard"
  "The current style setting.")

(defun ejab-use-style (style)
  "Use the EJab display style STYLE.
See `ejab-styles-alist' for a list of all available styles."
  (interactive
   (list
    (completing-read "Style (default standard): " ejab-styles-alist
                     nil t nil nil "standard")))
  (ejab-disable-style ejab-current-style)
  (unless (or (string= ejab-current-style "standard")
              (string= style "standard"))
    (ejab-enable-style "standard")
    (ejab-disable-style "standard"))
  (ejab-enable-style style))

(defun ejab-enable-style (style)
  (dolist (spec (cdr (assoc style ejab-styles-alist)))
    (case (first spec)
      ((call) (funcall (second spec)))
      ((hook) (add-hook (second spec) (third spec)))
      (t (set (first spec) (second spec))))))

(defun ejab-disable-style (style)
  (dolist (spec (cdr (assoc style ejab-styles-alist)))
    (case (first spec)
      ((call) (funcall (third spec)))
      ((hook) (remove-hook (second spec) (third spec))))))

;;}}}
;;{{{ Run Load Hook

(defvar ejab-load-hook ()
  "Normal hook run after EJab is first loaded.")

(unless (featurep 'ejab)
  (run-hooks 'ejab-load-hook))

;;}}}

(provide 'ejab)

;;; ejab.el ends here
