;;; ejab-rost-disp.el --- Display the ejab roster

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
;; the Jabber roster.

;;; Code:

(require 'cl)
(require 'ejab-utils)
(require 'ejab-roster)

;;{{{ Roster Mode

(defun ejab-roster-mode ()
  "Major mode to display the Jabber roster.
The following key bindings are available:
\\{ejab-roster-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'ejab-roster-mode
        mode-name "JRoster")
  (use-local-map ejab-roster-mode-map)
  (set (make-local-variable 'font-lock-defaults)
        '(ejab-roster-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'ejab-display-global-menu) nil)
  (set (make-local-variable 'mode-line-format)
       ejab-mode-line-format)
  (set (make-local-variable 'ejab-mode-line-disconnected-string)
       "Not Connected")
  (set (make-local-variable 'ejab-mode-line-string)
       "EJab: ")
  (ejab-take-over-menu-bar)
  (setq buffer-read-only t)
  (run-hooks 'ejab-roster-mode-hook)
  )

;; Don't inherit mode in new buffers
(put 'ejab-roster-mode 'mode-class 'special)

(defvar ejab-roster-mode-map (make-sparse-keymap))
(set-keymap-parent ejab-roster-mode-map ejab-keymap)

(define-key ejab-roster-mode-map "q" 'ejab-roster-display-quit)
(define-key ejab-roster-mode-map "l" 'ejab-roster-display-refresh)
(define-key ejab-roster-mode-map [mouse-1]
  'ejab-roster-display-compose-message)

;;}}}
;;{{{ Font Lock

(defvar ejab-roster-font-lock-keywords
  '(("\\(<.>\\|*\\) \\(.+ (Offline)\\)$" 2 'ejab-roster-offline-face)
    ("\\(<.>\\|*\\) \\(.+ (Away)\\)$" 2 'ejab-roster-away-face)
    ("\\(<.>\\|*\\) \\(.+ (N/A)\\)$" 2 'ejab-roster-xa-face)
    ("\\(<.>\\|*\\) \\(.+ (Chat)\\)$" 2 'ejab-roster-chat-face)
    ("\\(<.>\\|*\\) \\(.+ (DND)\\)$" 2 'ejab-roster-dnd-face)
    ("\\(<.>\\|*\\) \\(.+\\)$" 2 'ejab-roster-online-face)))

(defface ejab-roster-offline-face
  '((t (:foreground "Grey")))
  "")
(defface ejab-roster-online-face
  '((t (:foreground "Blue")))
  "")
(defface ejab-roster-away-face
  '((t (:foreground "DarkGreen")))
  "")
(defface ejab-roster-xa-face
  '((t (:foreground "SteelBlue")))
  "")
(defface ejab-roster-chat-face
  '((t (:foreground "Magenta")))
  "")
(defface ejab-roster-dnd-face
  '((t (:foreground "Brown")))
  "")

;;}}}
;;{{{ Display Roster

(defvar ejab-roster-display-spec nil
  "How to display the roster buffer.
See `ejab-display-buffer-by-spec' for allowable values.")

(defun ejab-display-roster ()
  "Display the current roster according to `ejab-roster-display-spec'.
Create and populate the roster display buffer if it does not exist."
  (interactive)
  (ejab-notify 0 "Displaying roster")
  (ejab-display-buffer-by-spec (or (get-buffer "*JRoster*")
                                   (ejab-roster-display-refresh))
                               ejab-roster-display-spec
                               'ejab-roster-display-finished-hook))

(add-hook 'ejab-roster-received-hook 'ejab-display-roster t)

(defvar ejab-roster-display-finished-hook '()
  "Hook run after the user is finished with the roster buffer.
This hook should be used to restore the previous window state, kill a
popup frame, etc.  In such cases it is probably best localized with
`make-local-hook', and set by the function which initially rearranged
the windows, created the popup frame, etc.")

(defun ejab-roster-display-quit ()
  "Stop displaying the roster.
This does not kill the roster buffer, though."
  (interactive)
  (run-hooks 'ejab-roster-display-finished-hook))

(defun ejab-kill-roster-display ()
  "Kill the roster display buffer.
This is normally done upon disconnection."
  (when (get-buffer "*JRoster*")
    (set-buffer (get-buffer "*JRoster*"))
    (ejab-roster-display-quit)
    (kill-buffer (get-buffer "*JRoster*"))))

(add-hook 'ejab-disconnected-hook 'ejab-kill-roster-display)
(add-hook 'ejab-before-connect-hook 'ejab-kill-roster-display)

;;}}}
;;{{{ Refresh Display

(defun ejab-roster-display-refresh ()
  "Create/update and return the roster display buffer.
There is only ever one roster display buffer, although it may be
displayed in different windows and/or frames.  This function creates
this buffer if it does not exist, clears and repopulates it, then
returns it.  It does not select it.  If its `buffer-invisibility-spec'
is not already set, it sets it to an appropriate value."
  (interactive)
  (ejab-notify 1 "Building roster display...")
  (save-current-buffer
    (set-buffer (get-buffer-create "*JRoster*"))
    ;; Don't turn on the mode twice, because that clobbers extra local
    ;; variables, like invisibility spec and saved window config.
    (unless (eq major-mode 'ejab-roster-mode)
      (ejab-roster-mode))
    (let ((ejab-invis-spec nil)
          (buffer-read-only nil)
          (tree (ejab-roster->group-tree)))
      (erase-buffer)
      (mapc #'delete-overlay (overlays-in (point-min) (point-max)))
      (ejab-roster-display-insert-tree tree)
      ;; If the invis spec hasn't been set yet, set it now.
      (when (eq buffer-invisibility-spec t)
        (setq buffer-invisibility-spec
              (append ejab-invis-spec ejab-default-hide-status))))
    (ejab-notify 1 "Building roster display...done")
    (current-buffer)))

;;}}}

;;{{{ Format Roster as Group Tree

(defun ejab-roster->group-tree ()
  "Return the current roster as a tree of groups.
A GROUP has the format \(NAME ELEMENT ...), where NAME is a string
identifying the group and each ELEMENT is either \(USER . STATUS) as
in `ejab-current-roster', or a subgroup GROUP.  \(Subgroups are
specified in the XML as `group/subgroup/subsubgroup/...', but this is
user-transparent.)  A group tree is a list of GROUPs."
  (let ((tree (list "Root")) current)
    (dolist (userlist ejab-current-roster)
      (dolist (group (or (funcall (car userlist) :==> 'group)
                         ;; If the user is in no groups, this has the
                         ;; effect of inserting him/her at top level.
                         '("")))
        (setq current tree)
        ;; Traverse the tree downwards until we find the group.
        (dolist (subgroup (split-string
                           (if (stringp group)
                               group
                             (funcall group :text))
                           "/"))
          (setq current
                ;; If the group exists, save it and go on from here.
                (or (assoc subgroup (cdr current))
                    ;; Otherwise, we have to create it first.
                    (progn
                      (setcdr current
                              (cons (list subgroup) (cdr current)))
                      (cadr current)))))
        ;; We now have the group where the item will go, so add it.
        (setcdr current (cons userlist (cdr current)))))
    ;; Skip the dummy group label
    (cdr tree)))

;;}}}
;;{{{ Insert Group Trees

(defun ejab-roster-display-insert-group (group nesting invis incrs prefix)
  "Insert a tree layout to display the roster group TREE.
NESTING should be an integer count of how many groups are above this
one, INVIS a list of symbols to add to the invisibility spec of all
inserted text, INCRS a list of functions to call to increment the
online counts of all containing groups, and PREFIX a string to be
added to all new invisibility spec symbols."
  (let ((begin (point))
        button-loc
        (button-map (make-sparse-keymap))
        count-loc
        incrementer
        ;; `buffer-invisibility-spec' is compared with `eq', so
        ;; anything in there should be a symbol.
        (groupsym (intern (concat prefix (car group)))))
    ;; Insert the group label
    (insert-char ?\  (* 4 nesting))
    (setq button-loc (point))
    (insert (if (and (listp buffer-invisibility-spec)
                     (memq groupsym buffer-invisibility-spec))
                "[+] "
              "[-] ")
            (car group) " (")
    (setq count-loc (point))
    (insert "0)\n")
    ;; Make the online count incrementing function
    (setq incrementer (ejab-roster-display-make-incrementer count-loc))
    ;; Make the [+] into a button
    (put-text-property button-loc (+ button-loc 3)
                       'mouse-face 'highlight)
    (define-key button-map [mouse-2]
      (ejab-roster-display-make-toggle-command groupsym (1+ button-loc)))
    (put-text-property button-loc (+ button-loc 3)
                       'local-map button-map)
    ;; Put the invisibility spec on.
    (put-text-property begin (point) 'invisible invis)
    ;; Create the group marker.
    (overlay-put (make-overlay begin (point))
                 'ejab-group groupsym)
    ;; Finally, insert the elements of the group.  We have to be
    ;; careful never to overwrite an existing `invisible' property.
    (ejab-roster-display-insert-tree (cdr group)
                                     (1+ nesting)
                                     (cons groupsym invis)
                                     (cons incrementer incrs)
                                     (concat prefix (car group) "/"))))

(defun ejab-roster-display-insert-item (item nesting invis incrs prefix)
  "Insert a tree layout to display the roster group ITEM.
NESTING should be an integer count of how many groups are above this
one, INVIS a list of symbols to add to the invisibility spec of all
inserted text, INCRS a list of functions to call to increment the
online counts of all containing groups, and PREFIX a string to be
added to all new invisibility spec symbols."
  (declare (special ejab-invis-spec))
  (let ((begin (point))
        (presence (ejab-get-presence (car item)))
        button-loc
        (button-map (make-sparse-keymap))
        status-loc
        ;; `buffer-invisibility-spec' is compared with `eq', so
        ;; anything in there should be a symbol.
        (usersym
         (intern (concat prefix (funcall (car item) 'jid)))))
    ;; Insert the user label
    (insert-char ?\  (* 4 nesting))
    (setq button-loc (point))
    (insert (if (or (not (listp buffer-invisibility-spec))
                    (memq usersym buffer-invisibility-spec))
                "<+> "
              "<-> ")
            (or (funcall (car item) 'name)
                (funcall (car item) 'jid)))
    (setq status-loc (point))
    (insert (ejab-roster-display-presence-string presence)
            "\n")
    ;; Hide the resources by default
    (add-to-list 'ejab-invis-spec usersym)
    ;; Make the <+> into a button
    (put-text-property button-loc (+ button-loc 3)
                       'mouse-face 'highlight)
    (define-key button-map [mouse-2]
      (ejab-roster-display-make-toggle-command usersym (1+ button-loc)))
    (put-text-property button-loc (+ button-loc 3)
                       'local-map button-map)
    ;; Add the invisibility spec
    (put-text-property begin (point) 'invisible
                       (cons (ejab-roster-display-presence-invis presence)
                             invis))
    ;; Increment the group counts if necessary
    (unless (or (not presence)
                (string= (funcall presence 'type) "unavailable"))
      (mapcar #'funcall incrs))
    ;; Insert the user's resources.
    (dolist (resource-presence (cdr item))
      (ejab-roster-display-insert-resource
       resource-presence
       (1+ nesting)
       (list* (ejab-roster-display-presence-invis presence) usersym invis)))
    ;; Add the user marker and saved info
    (let ((ovl (make-overlay begin (point))))
      (overlay-put ovl 'ejab-user
                   (intern (funcall (car element) 'jid)))
      (overlay-put ovl 'ejab-sub-invis
                   (list* (ejab-roster-display-presence-invis presence)
                          usersym invis))
      (overlay-put ovl 'ejab-incrs incrs)
      (overlay-put ovl 'ejab-nesting nesting)
      (overlay-put ovl 'ejab-change-status
                   (ejab-roster-display-make-status-changer
                    status-loc)))))

(defun ejab-roster-display-insert-resource (presence nesting invis)
  "Insert a line to display the resource of PRESENCE.
NESTING should be an integer count of how many groups and items are
above this one, and INVIS a list of symbols to add to the invisibility
spec of all inserted text."
  (let ((begin (point))
        status-loc)
    (insert-char ?\  (* 4 nesting))
    (insert "* "
            (or (funcall presence :resource) "(default)"))
    (setq status-loc (point))
    (insert (ejab-roster-display-presence-string presence)
            "\n")
    (put-text-property begin (point) 'invisible invis)
    (let ((ovl (make-overlay begin (point))))
      (overlay-put ovl 'ejab-resource (funcall presence :resource))
      (overlay-put ovl 'ejab-change-status
                   (ejab-roster-display-make-status-changer
                    status-loc)))))

(defun ejab-roster-display-insert-tree
  (tree &optional nesting invis incrs prefix)
  "Insert a tree layout to display TREE at nesting level NESTING.
NESTING should be an integer count of how many groups are above this
one, INVIS a list of symbols to add to the invisibility spec of all
inserted text, INCRS a list of functions to call to increment the
online counts of all containing groups, and PREFIX a string to be
added to all new invisibility spec symbols."
  (or nesting (setq nesting 0))
  (dolist (element tree)
    (if (stringp (car element))
        ;; Element is a group
        (ejab-roster-display-insert-group element nesting invis incrs prefix)
      ;; Element is a user
      (ejab-roster-display-insert-item element nesting invis incrs prefix))))

;;}}}
;;{{{ Add, Delete, Update;-COM-
;-COM-
;-COM-(defun ejab-roster-display-delete-item (item)
;-COM-  "Delete the entr(ies) for ITEM from the roster display buffer.
;-COM-Does nothing if the roster display buffer does not exist."
;-COM-  (when (get-buffer "*JRoster*")
;-COM-    (save-current-buffer
;-COM-      (set-buffer (get-buffer "*JRoster*"))
;-COM-      (let (pos)
;-COM-        (while (setq pos (text-property-any
;-COM-                          (point-min) (point-max) 'ejab-user
;-COM-                          (intern (funcall item 'jid))))
;-COM-          (delete-region pos (next-single-property-change
;-COM-                              pos 'ejab-user nil (point-max))))))))
;-COM-
;-COM-(add-hook 'ejab-before-roster-item-removed-hooks
;-COM-          'ejab-roster-display-delete-item)
;-COM-
;-COM-(defun ejab-roster-display-add-item (item)
;-COM-  "Add entr(ies) for ITEM to the roster display buffer.
;-COM-Does nothing if the roster display buffer does not exist."
;-COM-  (when (get-buffer "*JRoster*")
;-COM-    (save-current-buffer
;-COM-      (set-buffer (get-buffer "*JRoster*"))
;-COM-      ;; Add the user to the correct groups, creating groups if
;-COM-      ;; necessary.
;-COM-      )))
;-COM-
;-COM-(add-hook 'ejab-roster-item-added-hooks 'ejab-roster-display-add-item)
;-COM-
;-COM-(defun ejab-roster-display-update-item (old new)
;-COM-  (ejab-roster-display-delete-item old)
;-COM-  (ejab-roster-display-add-item new))
;-COM-
;-COM-(add-hook 'ejab-before-roster-item-updated-hooks
;-COM-          'ejab-roster-display-update-item)
;-COM-
;;}}}
;;{{{ Add, Delete, Update (Rebuild)

;; Adding to existing groups is too complicated for now, and the
;; roster doesn't generally change much within a given session, so we
;; just rebuild the whole thing every time.
(defun ejab-rebuild-roster-display (&rest args)
  (when (get-buffer "*JRoster*")
    (ejab-roster-display-refresh)))

(add-hook 'ejab-roster-item-added-hooks
          'ejab-rebuild-roster-display)
(add-hook 'ejab-after-roster-item-removed-hooks
          'ejab-rebuild-roster-display)
(add-hook 'ejab-after-roster-item-updated-hooks
          'ejab-rebuild-roster-display)

;;}}}
;;{{{ Update For Presence

(defun ejab-roster-display-update-presence (user presence)
  "Update the displayed PRESENCE for USER."
  (when (get-buffer "*JRoster*")
    (save-current-buffer
      (set-buffer (get-buffer "*JRoster*"))
      (let ((buffer-read-only nil)
            begin
            (usersym (intern (funcall user 'jid))))
        ;; Find each occurrence of the user in the group hierarchy.
        (dolist (ovl (remove-if-not
                      #'(lambda (ovl)
                          (eq (overlay-get ovl 'ejab-user) usersym))
                      (overlays-in (point-min) (point-max))))
          (let ((old-invis (overlay-get ovl 'ejab-sub-invis))
                (new-pres (ejab-get-presence user)))
            (let ((new (ejab-roster-display-presence-invis new-pres))
                  (prev (car (intersection
                              old-invis
                              '(offline online away xa dnd chat))))
                  (rest (set-difference
                         old-invis
                         '(offline online away xa dnd chat))))
              ;; If the user's main presence changed, then:
              (unless (eq prev new)
                ;; Increment or decrement the counters if necessary
                (and (eq prev 'offline)
                     (not (eq new 'offline))
                     (mapc #'funcall
                           (overlay-get ovl 'ejab-incrs)))
                (and (not (eq prev 'offline))
                     (eq new 'offline)
                     (mapc (ejab-call -1)
                           (overlay-get ovl 'ejab-incrs)))
                ;; Update the displayed string
                (funcall (overlay-get ovl 'ejab-change-status)
                         new-pres))
              ;; Always update the specific resource line.
              (let ((resource-ovl
                     (find-if
                      #'(lambda (ovl)
                          (string= (overlay-get ovl 'ejab-resource)
                                   (funcall presence :resource)))
                      (overlays-in (overlay-start ovl)
                                   (overlay-end ovl)))))
                (if resource-ovl
                    (funcall (overlay-get resource-ovl
                                          'ejab-change-status)
                             presence)
                  ;; Insert a new resource line
                  (goto-char (overlay-start ovl))
                  (forward-line 1)
                  (beginning-of-line)
                  (ejab-roster-display-insert-resource
                   presence
                   (1+ (overlay-get ovl 'ejab-nesting))
                   (cons new rest))
                  ;; Make sure the overlay covers the new line.
                  (move-overlay ovl (overlay-start ovl)
                                (max (overlay-end ovl) (point)))))
              ;; Update the invisibility specs
              (goto-char (overlay-start ovl))
              (forward-line 1)
              (beginning-of-line)
              (put-text-property
               (overlay-start ovl) (point) 'invisible
               (cons new
                     (set-difference
                      (get-text-property (overlay-start ovl)
                                         'invisible)
                      '(offline online away xa dnd chat))))
              (put-text-property (point) (overlay-end ovl)
                                 'invisible (cons new rest))
              )))))))

(add-hook 'ejab-after-presence-change-hooks
          'ejab-roster-display-update-presence)

;;}}}

;;{{{ Make Helper Functions

(defun ejab-roster-display-make-toggle-command (symbol location)
  "Return a function object to expand SYMBOL on a button at LOCATION.
When invoked \(usually as the binding of a mouse button press), this
command will toggle the character at LOCATION \(`+' and `-') and the
visibility of text with invisibility spec SYMBOL."
  (lexical-let ((sym symbol)
                (loc (set-marker (make-marker) location)))
    #'(lambda (event)
        (interactive "e")
        (save-excursion
          (let ((buffer-read-only nil))
            (goto-char loc)
            (if (eql (char-after) ?\+)
                (progn
                  (insert-char ?\- 1 t)
                  (remove-from-invisibility-spec sym))
              (insert-char ?\+ 1 t)
              (add-to-invisibility-spec sym))
            (delete-char 1))))))

(defun ejab-roster-display-make-incrementer (location)
  "Return a function to increment/decrement the number at LOCATION.
The function takes one argument and adds it to this number, defaulting
to +1."
  (lexical-let ((loc (copy-marker location nil)))
    #'(lambda (&optional incr)
        (save-excursion
          (let* ((buffer-read-only nil)
                 (end (copy-marker loc))
                 (count (read end)))
            (delete-region loc end)
            (goto-char loc)
            (insert (int-to-string (+ count (or incr 1)))))))))

(defun ejab-roster-display-make-status-changer (location)
  "Return a function to change the displayed status at LOCATION.
The function takes one argument, a presence object, and changes the
displayed status at LOCATION to the status of that presence."
  (lexical-let ((loc (copy-marker location nil)))
    #'(lambda (presence)
        (save-excursion
          (goto-char loc)
          (let* ((buffer-read-only nil)
                 (invis (get-text-property (point) 'invisible)))
            (delete-region loc (progn (end-of-line) (point)))
            (insert
             (ejab-roster-display-presence-string presence))
            (put-text-property loc (point) 'invisible invis))))))

;;}}}
;;{{{ Display Presence

(defun ejab-roster-display-presence-string (presence)
  "Return a string to display after a user or resource for PRESENCE."
  ;; No received presence at all means the user is offline.
  (if (or (not presence)
          (string= (funcall presence 'type) "unavailable"))
      " (Offline)"
    (cdr (assoc (funcall presence 'show)
                `(("away" . " (Away)")
                  ("xa" . " (N/A)")
                  ("chat" . " (Chat)")
                  ("dnd" . " (DND)")
                  (nil . ""))))))

(defun ejab-roster-display-presence-invis (presence)
  "Return the appropriate `invisible' property for PRESENCE.
This is `offline' if PRESENCE is nil or unavailable, the `show' of
PRESENCE otherwise, or `online' if it has none."
  (if (or (not presence)
          (string= (funcall presence 'type)
                   "unavailable"))
      ;; `buffer-invisibility-spec' is compared with `eq', so anything
      ;; in there should be a symbol.
      'offline
    (intern (or (funcall presence 'show) "online"))))

;;}}}
;;{{{ Status Hiding

(defvar ejab-default-hide-status '(offline)
  "List of status symbols to hide by default in roster display.")

(dolist (status '(offline online away xa dnd chat))
  (fset (intern (format "ejab-hide-%s-p" status))
        `(lambda ()
           (if (buffer-live-p (get-buffer "*JRoster*"))
               (save-excursion
                 (set-buffer (get-buffer "*JRoster*"))
                 (memq ',status buffer-invisibility-spec))
             (memq ',status ejab-default-hide-status))))
  (fset (intern (format "ejab-toggle-hide-%s" status))
        `(lambda ()
           (interactive)
           (if (buffer-live-p (get-buffer "*JRoster*"))
               (save-excursion
                 (set-buffer (get-buffer "*JRoster*"))
                 (if (memq ',status buffer-invisibility-spec)
                     (remove-from-invisibility-spec ',status)
                   (add-to-invisibility-spec ',status)))
             (if (memq ',status ejab-default-hide-status)
                 (setq ejab-default-hide-status
                       (remove ',status ejab-default-hide-status))
               (add-to-list 'ejab-default-hide-status ',status))))))

;;}}}

;;{{{ Mouse Bindings

(defun ejab-roster-display-user-at-click (event &optional noresource)
  "Return the jid of the user under the mouse at EVENT.
Unless NORESOURCE, include any resource under mouse.  If no user is
under the mouse, return nil."
  (let ((user
         (some #'(lambda (ovl)
                   (overlay-get ovl 'ejab-user))
               (overlays-at (posn-point (event-start event)))))
        (resource
         (some #'(lambda (ovl)
                   (overlay-get ovl 'ejab-resource))
               (overlays-at (posn-point (event-start event))))))
    (when user
      (if (and resource (not noresource))
          (format "%s/%s" user resource)
        (symbol-name user)))))

(defun ejab-roster-display-compose-message (event)
  "Begin composing a message to the user clicked on."
  (interactive "e")
  (when (> (event-click-count event) 1)
    (let ((user (ejab-roster-display-user-at-click event)))
      (when user
        (ejab-new-message `((to . ,user)))))))

;;}}}

(provide 'ejab-rost-disp)

;;; ejab-rost-disp.el ends here
