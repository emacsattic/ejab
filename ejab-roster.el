;;; ejab-roster.el --- Ejab roster storage and handling

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

;; This file defines functions and variables to store and manipulate
;; the Jabber roster.  The roster is displayed by `ejab-rost-disp.el'

;;; Code:

(require 'cl)
(require 'ejab-utils)
(require 'ejab-obj)

;;{{{ Matching Items and Presences

(defun ejab-presence=item-p (one two)
  "Return non-nil if a presence is an instance of an item.
The arguments can be passed in either order.  If the item has no
resource, then the jids must match and the presence can have any
resource, but if the item has a resource, it must match as well."
  (let (presence item)
    (if (eq (funcall one :typeof) 'presence)
        (setq presence one item two)
      (setq presence two item one))
    (if (funcall item :resource)
        (ejab-compare-nocase (funcall item 'jid)
                             (funcall presence 'from))
      (and (ejab-compare-nocase (funcall item 'jid)
                                (funcall presence 'from))
           (string= (funcall item :resource)
                    (funcall presence :resource))))))

;;}}}
;;{{{ Roster Storage and Access

(defvar ejab-current-roster nil
  "Store the current roster of contacts and status.

Each element of this list looks like \(ITEM . STATUS), where ITEM is
an ejab `item' object, as from a roster query, and STATUS is a list of
ejab `presence' objects such as would be received from the server to
specify the user's status.  Each element of STATUS is the presence for
a unique resource of the user.  If STATUS is nil, the user is
unavailable.

This alist is best accessed via `ejab-get-presence' and modified via
`ejab-roster-update-item' and `ejab-roster-update-presence', since
they handle all the difficult matching and searching and run the
correct updating hooks.")

(defun ejab-get-item (user)
  "Get the roster item for the USER, a jid maybe with a resource."
  (car (assoc* (ejab-extract-jid user)
               ejab-current-roster
               :key (ejab-call 'jid)
               :test #'ejab-compare-nocase)))

(defun ejab-get-presence (user &optional resource)
  "Get the presence for a Jabber USER on the roster.
USER may be either a jid \(a string `USER@HOST') or an ejab `item'
object.  RESOURCE may be either a string, specifying the resource to
get the presence for, nil, meaning to get the presence for the
highest-priority resourced, or t, meaning to get a list of presences
for all resources.  If RESOURCE is the empty string \"\", it means to
get the presence unassociated with any resource.  If USER is not on
the roster, has no resource RESOURCE, or RESOURCE is nil and no
presence at all has been received for USER, return nil."
  (let ((pair (assoc* (if (stringp user) user (funcall user 'jid))
                      ejab-current-roster
                      :key (ejab-call 'jid)
                      :test #'ejab-compare-nocase)))
    (cond
     ;; This is probably technically unnecessary, but good practice.
     ((not pair) nil)
     ;; Strings are looked up in the list, but resource-less presences
     ;; are stored as nil, not "".
     ((stringp resource)
      (car (member* (if (string= resource "") nil resource)
                    (cdr pair)
                    :key (ejab-call :resource)
                    :test #'equal)))
     ;; Return the whole list.
     ((eq resource t) (cdr pair))
     ;; Get the highest priority.
     ((not resource)
      ;; Not sure exactly how this should work.  Is 0 the right
      ;; default priority?
      (car (sort* (copy-list (cdr pair)) #'>
                  :key #'(lambda (presence)
                           (string-to-int
                            (or (funcall presence 'priority) "0"))))))
     (t (ejab-notify 3 "Invalid resource: %s" resource)))))

;;}}}
;;{{{ Roster Updating

(defvar ejab-roster-item-added-hooks nil
  "Abnormal hook run after a roster item is added.
Functions in this hook are passed one argument, the USER just added to
the roster, as an ejab `item' object whose primary useful attributes
are `jid' and `name'.  This user has no presence yet \(i.e. he or she
is unavailable).  Note that each user must be added to the local
roster each session as the server informs us of our roster settings.")

(defvar ejab-before-roster-item-removed-hooks nil
  "Abnormal hook run before a roster item is removed.
Functions in this hook are passed one argument, the USER about to be
removed from the roster, as an ejab `item' object whose primary useful
attributes are `jid' and `name'.  The user's current presence can be
obtained from `ejab-get-presence', which see.")

(defvar ejab-after-roster-item-removed-hooks nil
  "Abnormal hook run after a roster item is removed.
Functions in this hook are passed one argument, the USER just removed
from the roster, as an ejab `item' object whose primary useful
attributes are `jid' and `name'.")

(defvar ejab-before-roster-item-updated-hooks nil
  "Abnormal hook run before a roster item is removed.
Functions in this hook are passed two arguments:
1. The USER about to be updated on the roster, as an ejab `item'
   object whose primary useful attributes are `jid' and `name'.  The
   user's current presence can be obtained from `ejab-get-presence',
   which see.
2. The NEW ejab `item' object about to replace this user on the
   roster.  The `jid' should be the same, but other attributes may
   change.")

(defvar ejab-after-roster-item-updated-hooks nil
  "Abnormal hook run after a roster item is removed.
Functions in this hook are passed one arguments, the new ejab `item'
object that just replaced this user on the roster.")

(defvar ejab-before-presence-change-hooks nil
  "Abnormal hook run before a user's presence changes.
Functions in this hook are passed two arguments:
1. The USER whose presence changed, an ejab `item' object, whose
   primary useful attributes are `jid' and `name'.  Information about
   the current presences of the user \(before this change) can be
   obtained from `ejab-get-presence', which see.
2. The NEW presence, an ejab `presence' object, whose primary useful
   attributes are `type', `show', `status', and `:resource'.

This hook exists so that you can access the previous presence of the
user.  In general, `ejab-after-presence-change-hooks' is easier to
use, because `ejab-get-presence' works on the new presence then.")

(defvar ejab-after-presence-change-hooks nil
  "Abnormal hook run after a user's presence changes.
Functions in this hook are passed two arguments:
1. The USER whose presence changed, an ejab `item' object, whose
   primary useful attributes are `jid' and `name'.  Information about
   all new presences of the user \(after the change) can be obtained
   from `ejab-get-presence', which see.
2. The NEW presence, an ejab `presence' object, whose primary useful
   attributes are `type', `show', `status', and `:resource'.")

(defun ejab-roster-update-item (item)
  "Add, update, or remove ITEM on the current roster.
If ITEM has subscription type `remove', it is removed if it exists,
otherwise it is updated if it exists or added if it doesn't.  Items
are matched with any resources they may have, so the same jid with
different resources can appear twice on the roster.  This is mostly
for transports.  This function sends nothing to the server, it only
updates the local copy."
  (let ((pair (assoc* (funcall item 'jid)
                      ejab-current-roster
                      :key (ejab-call 'jid)
                      :test #'ejab-compare-nocase)))
    (if pair
        (if (string= (funcall item 'subscription) "remove")
            ;; Remove item
            (progn
              (run-hook-with-args
               'ejab-before-roster-item-removed-hooks item)
              (setq ejab-current-roster
                    (remove* (funcall item 'jid) ejab-current-roster
                             :key #'(lambda (pair)
                                      (funcall (car pair) 'jid))
                             :test #'ejab-compare-nocase))
              (run-hook-with-args
               'ejab-after-roster-item-removed-hooks item))
          ;; Update item
          (run-hook-with-args 'ejab-before-roster-item-updated-hooks
                              (car pair) item)
          (setcar pair item)
          (run-hook-with-args 'ejab-after-roster-item-updated-hooks
                              item))
      ;; Add item
      (add-to-list 'ejab-current-roster (cons item nil))
      (run-hook-with-args 'ejab-roster-item-added-hooks item))))

(defun ejab-roster-update-presence (presence)
  "Add or update the PRESENCE for a user's resource in the roster.
If the user is not in the roster, add a skeletal item.  This function
sends nothing to the server, it only updates the local copy."
  (let ((userlist (assoc* presence ejab-current-roster
                          :test #'ejab-presence=item-p)))
    (unless userlist
      ;; Create skeletal user
      (setq userlist
            (list (ejab-make-item `(jid ,(funcall presence :jid)))))
      (add-to-list 'ejab-current-roster userlist))
    (run-hook-with-args 'ejab-before-presence-change-hooks
                        (car userlist) presence)
    ;; Remove any previous presence for the resource, and add the new
    ;; presence, at the beginning so it will have precedence in the
    ;; absence of `priority' attributes.  This also works if :resource
    ;; is nil (no resource).
    (setcdr userlist
            (cons presence
                  (delete* (funcall presence :resource)
                           (cdr userlist)
                           :key (ejab-call :resource)
                           :test #'equal)))
    (run-hook-with-args 'ejab-after-presence-change-hooks
                        (car userlist) presence)))

;;}}}
;;{{{ Clear Roster

(defun ejab-clear-roster ()
  "Clear the roster.
This is normally done upon disconnection from the server."
  (setq ejab-current-roster nil))

(add-hook 'ejab-disconnected-hook 'ejab-clear-roster)
(add-hook 'ejab-before-connect-hook 'ejab-clear-roster)

;;}}}

;;{{{ Interactive Commands

(defun ejab-interactive-read-item (prompt)
  "Interactively read an item in the current roster with PROMPT.
Provides completion on both jids and nicknames.  Returns the
corresponding item in the current roster."
  (let ((user (completing-read
               prompt
               (mapcan #'(lambda (userlist)
                           (list (list (funcall (car userlist) 'jid))
                                 (list (funcall (car userlist) 'name))))
                       ejab-current-roster))))
    (or (find user (mapcar #'car ejab-current-roster)
              :key (ejab-call 'jid) :test #'ejab-compare-nocase)
        (find user (mapcar #'car ejab-current-roster)
              :key (ejab-call 'name) :test #'string=))))

(defun ejab-interactive-add-roster-item (user)
  "Add USER to the roster, querying for nickname and groups.
If query is not desired, use `ejab-set-roster-item'.  Note that this
function itself is *not* an interactive command, it is called from
other such commands."
  (ejab-set-roster-item
   user (read-string "Nickname for user: ")
   (loop for group = (read-string
                      "Add to Group (return when done): ")
         until (string= group "")
         collect group)))

(defun ejab-interactive-edit-roster-item (user)
  "Set the name and groups for USER, querying interactively.
If query is not desired, use `ejab-set-roster-item'.  Note that this
function itself is *not* an interactive command, it is called from
other such commands.  USER should be an ejab `item' object."
  (ejab-set-roster-item
   user
   (read-string "Nickname: " (funcall user 'name))
   (append (loop for group in (funcall user :==> 'group)
                 unless (y-or-n-p (format "Remove from group `%s'? "
                                          (funcall group :text)))
                 collect (funcall group :text))
           (loop for group = (read-string
                              "Add to Group (return when done): ")
                 until (string= group "")
                 collect group))))

(defun ejab-edit-roster-item (user)
  "Edit the roster item USER, setting name and/or groups."
  (interactive (list (ejab-interactive-read-item "Edit User: ")))
  (ejab-interactive-edit-roster-item user))

;;}}}

;;{{{ Set Roster Items

(defvar ejab-set-roster-item-ids-alist ()
  "Alist of pending roster sets.
Each element is \(ID JID NAME ADDP) where ID was the id of the <iq>
sent, JID the jid added/updated, NAME the nickname, if any, and ADDP
whether the set was an add \(otherwise it was an update).")

(defun ejab-set-roster-item (user &optional name groups)
  "Add or update a USER in the roster.
Sends a set request to the server, which should respond with a roster
push, at which point the user will be added or updated.  USER may be
either a jid \(string) or an ejab `item' object.  In the former case,
second and third arguments may be specified to give the user a
nickname and one or more groups, while in the latter case the `item'
object may have a `name' attribute and/or one or more `group'
contents.  Note that adding users to the roster is orthogonal to
subscribing to presence, although often one does both together."
  (let ((id (ejab-next-id)))
    (if (stringp user)
        (setq groups (mapcar #'(lambda (group)
                                 (cons 'group group))
                             groups))
      (setq groups (mapcar #'(lambda (group)
                               (cons 'group group))
                           (funcall user :==> 'group))
            name (funcall user 'name)
            user (funcall user 'jid)))
    (add-to-list 'ejab-set-roster-item-ids-alist
                 (list id user name (not (ejab-get-item user))))
    (funcall
     (ejab-make-iq
      `(type "set" id ,id)
      `(query ,(ejab-make-query
                `(xmlns "jabber:iq:roster")
                `(item ,(ejab-make-item
                         `(jid ,user name ,name)
                         groups)))))
     :send)
    (add-hook 'ejab-receive-object-functions 'ejab-receive-roster-set)))

;; The response to a roster set is apparently both an empty "result"
;; query and a roster push.  The latter is handled by
;; `ejab-receive-roster' and the former right here.

(defun ejab-receive-roster-set (obj)
  (ejab-with (obj)
    (when (eq (obj :typeof) 'iq)
      (let ((pair (assoc (string-to-int (obj 'id))
                         ejab-set-roster-item-ids-alist)))
        (when pair
          ;; Handle both results and errors
          (if (string= (obj 'type) "result")
              ;; Only notification is necessary, since the server also
              ;; does a roster push to update our local copy.
              (ejab-notify 2 "%s <%s> successfully %s roster"
                           (third pair) (second pair)
                           (if (fourth pair) "added to" "updated in"))
            (run-hook-with-args 'ejab-error-hooks (obj :=> 'error)))
          ;; Error or not, we can stop waiting for this response
          (setq ejab-set-roster-item-ids-alist
                (remove* pair ejab-set-roster-item-ids-alist
                         :test #'equal))
          ;; If we aren't waiting for any more roster removes, no need
          ;; to check for such responses.
          (unless ejab-set-roster-item-ids-alist
            (remove-hook 'ejab-receive-object-functions
                         'ejab-receive-roster-set))
          ;; Handled object.
          t)))))

;;}}}
;;{{{ Remove Roster Items

(defvar ejab-remove-roster-item-ids-alist ())

(defun ejab-remove-roster-item (user)
  "Remove a USER from the roster.
Sends a remove request to the server, which should respond with a
roster push, at which point the user will be removed.  USER may be
either a jid \(string) or an ejab `item' object."
  (let ((id (ejab-next-id))
        (jid (if (stringp user) user (funcall user 'jid))))
    (add-to-list 'ejab-remove-roster-item-ids-alist (cons id jid))
    (funcall
     (ejab-make-iq
      `(type "set" id ,id)
      `(query ,(ejab-make-query
                `(xmlns "jabber:iq:roster")
                `(item ,(ejab-make-item
                         `(jid ,jid subscription "remove"))))))
     :send))
  (add-hook 'ejab-receive-object-functions 'ejab-receive-roster-remove))

;; The response to a roster remove is apparently both an empty
;; "result" query and a roster push.  The latter is handled by
;; `ejab-receive-roster' and the former right here.

(defun ejab-receive-roster-remove (obj)
  (ejab-with (obj)
    (when (eq (obj :typeof) 'iq)
      (let ((pair (assoc (string-to-int (obj 'id))
                         ejab-remove-roster-item-ids-alist)))
        (when pair
          ;; Handle both results and errors
          (if (string= (obj 'type) "result")
              ;; Only notification is necessary, since the server also
              ;; does a roster push to update our local copy.
              (ejab-notify 2 "%s successfully removed from roster"
                           (cdr pair))
            (run-hook-with-args 'ejab-error-hooks (obj :=> 'error)))
          ;; Error or not, we can stop waiting for this response
          (setq ejab-remove-roster-item-ids-alist
                (remove* pair ejab-remove-roster-item-ids-alist
                         :test #'equal))
          ;; If we aren't waiting for any more roster removes, no need
          ;; to check for such responses.
          (unless ejab-remove-roster-item-ids-alist
            (remove-hook 'ejab-receive-object-functions
                         'ejab-receive-roster-remove))
          ;; Handled object.
          t)))))

;;}}}
;;{{{ Request Roster from Server

(defvar ejab-roster-received-hook '()
  "Normal hook run when a requested roster list is received.
This is not run on roster pushes, only when an entire requested roster
is receieved.  This normally only happens upon connection.")

(defun ejab-notify-roster-received ()
  "Notify the user that the roster has been received."
  (ejab-notify 1 "Roster received"))

(add-hook 'ejab-roster-received-hook 'ejab-notify-roster-received)

(defvar ejab-roster-request-id nil)

(defun ejab-request-roster ()
  "Request roster from the server.
Reset local copy and request the entire roster from the server.  When
the requested roster arrives, `ejab-roster-received-hook' is run."
  ;; Reset the roster
  (setq ejab-current-roster nil)
  ;; Put ourselves on the roster
  (funcall
   (ejab-make-iq
    `(type "get" id ,(setq ejab-roster-request-id (ejab-next-id)))
    `(query ,(ejab-make-query
              '(xmlns "jabber:iq:roster")
              '())))
   :send))

(add-hook 'ejab-authenticated-hook #'ejab-request-roster)

;;}}}
;;{{{ Receive Roster Items

(defun ejab-receive-roster (obj)
  "Receive roster information from the server.
Return non-nil if OBJ is roster information."
  (when (and (eq (funcall obj :typeof) 'iq)
             (funcall obj :=> 'query)
             (string= (funcall obj :-> 'query 'xmlns)
                      "jabber:iq:roster"))
    (ejab-notify 0 "Receiving roster information")
    (if (member (funcall obj 'type) '("set" "result"))
        (progn
          (ejab-notify 0 "Updating roster item(s)...")
          (mapc #'ejab-roster-update-item
                (funcall obj :-> 'query :==> 'item))
          (ejab-notify 0 "Updating roster item(s)...done")
          (when (and (funcall obj 'id)
                     (eql (string-to-int (funcall obj 'id))
                          ejab-roster-request-id))
            (setq ejab-roster-request-id nil)
            (run-hooks 'ejab-roster-received-hook)))
      (run-hook-with-args 'ejab-error-hooks (funcall obj :=> 'error)))
    t))

;; We do this globally because the server may (I believe) send us
;; roster pushes due to the actions of a different resource, so not
;; all received rosters are in response to a query or set.
(add-hook 'ejab-receive-object-functions #'ejab-receive-roster)

;;}}}

(provide 'ejab-roster)

;;; ejab-roster.el ends here
