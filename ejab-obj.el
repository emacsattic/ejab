;;; ejab-obj.el --- ejab objects to represent XML tags

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

;; This file defines functions for making and manipulating ejab
;; objects, which represent XML tags.  Since it defines macros
;; (`ejab-defobj' and friends), other files which use these macros
;; should `require' this file at top-level for byte-compilation.  It
;; is not necessary to do so if a file only uses the maker functions,
;; although it should still be loaded (or autoloaded) at run-time.

;; Ejab objects represent XML tags relatively faithfully, with two
;; major exceptions:

;; 1. XML tags can contain other tags that are only allowed once,
;;    while if an ejab object can contain another, it can contain any
;;    number of it.  This is generally not a problem.

;; 2. XML namespaces specify syntax for all *contained* tags as well
;;    as the tag with the xmlns='...' attribute, while
;;    `ejab-defobj-by-namespace' only conditions the primary object.
;;    E.g. <item> in jabber:iq:roster and <item> in jabber:x:roster
;;    are distinct and may have different syntax (but don't), while as
;;    ejab objects they are the same type and have the same syntax.
;;    This has not yet been a problem.

;;; Code:

(require 'cl)
(require 'ejab-utils)

;;{{{ Auxiliary Functions

(defun ejab-maker-name (name &optional suffix)
  "Return the name of the function that makes objects of tyme NAME.
Normally this is `ejab-make-NAME', but if SUFFIX is non-nil, then it
is `ejab-make-NAME/SUFFIX'--this is used for namespaces."
  (intern (if suffix
              (format "ejab-make-%s/%s" name suffix)
            (format "ejab-make-%s" name))))

(ejab-define-error ejab-improper-object-type
                   "Improper object type")

(defun ejab-ensure-object (obj name)
  "Make OBJ into an object of type NAME if it isn't already.
If NAME is not an object type, return OBJ."
  (let ((maker (ejab-maker-name name)))
    (cond ((not maker) obj)     ; Not a valid object type
          ((not obj) nil)
          ((ejab-object-p obj)
           (if (eq name (funcall obj :typeof))
               obj
             (signal 'ejab-improper-object-type
                     (list (funcall obj :typeof)))))
          ((stringp obj)
           (funcall maker obj))
          (t (apply maker obj)))))

(defun ejab-get (key somelist)
  "Get from either an alist or a plist."
  (if (consp (car somelist))
      ;; alist
      (cdr (assq key somelist))
    ;; plist
    (plist-get somelist key)))

(defun ejab-get-all (key somelist)
  "Get *all* entries matching KEY from SOMELIST."
  (if (consp (car somelist))
      ;; alist
      (mapcar #'cdr
              (remove* key somelist :test-not #'eq :key #'car))
    ;; plist
    (loop for (thekey value) on somelist by #'cddr
          if (eq thekey key) collect value)))

(defun ejab-keyword-p (symbol)
  (= ?: (aref (symbol-name symbol) 0)))

;; Maybe improve this at some point
(defalias 'ejab-object-p 'functionp)

;;}}}
;;{{{ Generic Object Behavior

(ejab-define-error ejab-invalid-contained-object
                   "Invalid contained object"
                   ejab-object-access-error)
(ejab-define-error ejab-ambiguous-contained-object
                   "Multiply contained object accessed singly"
                   ejab-object-access-error)
(ejab-define-error ejab-unavailable-contained-object
                   "No such object is contained"
                   ejab-object-access-error)
(ejab-define-error ejab-invalid-method "Invalid method call"
                   ejab-object-access-error)
(ejab-define-error ejab-invalid-attribute "Invalid attribute access"
                   ejab-object-access-error)

(defun ejab-generic-object
  (name text attribs contents methods this attr args)
  "Implement the behavior of all ejab objects.
Ejab objects themselves are lexical closures which simply call this
function, passing it everything they were closed with, and the
arguments they are called with."
  (if (ejab-keyword-p attr)
      ;; Calling a method
      (case attr
        (:=>
         ;; Get/set a singly contained tag.
         (let ((pair (assq (car args) contents)))
           (cond
            ((not pair)
             (signal 'ejab-invalid-contained-object (list (car args))))
            ((cddr pair)
             (signal 'ejab-ambiguous-contained-object (list (car args))))
            ((cdr args)
             ;; Set value
             (setcar (cdr pair)
                     (ejab-ensure-object (cadr args) (car args))))
            (t
             ;; Get value
             (cadr pair)))
           ))
        (:->
         ;; Call a singly contained tag with arguments
         (apply (or (funcall this :=> (car args))
                    (signal 'ejab-unavailable-contained-object
                            (list (car args))))
                (cdr args)))
        (:==>
         ;; Get a list of a (multiply) contained tag
         (let ((pair (assq (car args) contents)))
           (if pair
               (cdr pair)
             (signal 'ejab-invalid-contained-object (list (car args))))))
        (:typeof name)
        (:attribs attribs)
        (:contents contents)
        (:methods methods)
        (:xml (require 'ejab-xml) (ejab-obj->xml this))
        (:send (require 'ejab-connect) (ejab-obj-send this))
        (:text (if args
                   (setq text (car args))
                 text))
        (t
         ;; Look for a custom method
         (let ((pair (assq attr methods)))
           (if pair
               (funcall (cdr pair) this)
             (signal 'ejab-invalid-method (list attr))))))
    ;; Get/set an attribute
    (let ((pair (assq attr attribs)))
      (if pair
          (if args
              (setcdr pair (car args))
            (cdr pair))
        ;; If no such attribute, look for a contained tag and
        ;; return its :text, if any.
        (condition-case var
            (apply this :-> attr :text args)
          ;; If such a contained tag is possible, but none is
          ;; currently contained, return nil.
          (ejab-unavailable-contained-object
           nil)
          ;; If there are too many such tags, propagate the error.
          (ejab-ambiguous-contained-object
           (signal 'ejab-ambiguous-contained-object (cdr data)))
          ;; If no such contained tag is possible, then the
          ;; attribute access is invalid.
          (ejab-invalid-contained-object
           (signal 'ejab-invalid-attribute (list attr))))))))

;;}}}
;;{{{ Define Object Types

(defmacro ejab-defobj
  (name &optional attribs contents pcdata methods docs suffix)
  "Define an ejab object type.
NAME should be the name of the XML tag, such as `presence', as a
lisp symbol.  ATTRIBS are the allowed XML attributes, and CONTENTS are
the XML tags allowed inside, both as symbols.  If PCDATA is
non-nil, then the object can contain text as well as the XML tags
in CONTENTS \(if any).  DOCS, if supplied, is inserted into the
otherwise automatically generated docstring of the maker function
\(see below).  SUFFIX, a string or symbol, is used internally to
distinguish between namespaces--see `ejab-defobj-by-namespace'.

METHODS, if present, should be an alist associating custom method
names \(see below), colons included, to functions implementing them.
Such functions should take one argument: the object the method was
invoked on.  At present it is not possible to redefine the built-in
methods.

This macro defines a function `ejab-make-NAME-SUFFIX' which takes up
to three arguments and returns a NAME object.  If PCDATA is non-nil,
the first argument, if present and non-nil, should be the text
contained.  The second \(first if PCDATA is nil) argument should be an
alist or plist of attributes \(name and value) and the third \(second)
an alist or plist of contained objects \(name and value).  Contained
objects can appear more than once in this list.  Rather than an ejab
object, the values in the last argument can also be a string or a list
of arguments to make the value with.

If PCDATA is non-nil and the first argument is not a string, it is
considered the attributes, the second argument is the contents, and
the third attribute, if any, is the text.  If PCDATA is nil, a third
\(ignored) argument may be supplied, and if the first argument is a
string, it is considered the ignored argument.  This basically means
that however you call it will probably work.

Ejab objects are lisp function objects, their properties accessible by
calling them with arguments.  The examples here assume a surrounding
\(ejab-with \(OBJ) ...) -- see `ejab-with'.

\(OBJ FIELD) returns the FIELD attribute of OBJ, while \(OBJ FIELD
VALUE) sets it to VALUE and returns VALUE.  \(OBJ :=> TAG) returns the
contained tag TAG as an ejab object, and \(OBJ :=> TAG VALUE) sets it
to VALUE, which can be either an ejab object of appropriate type or a
creation-value as above.  To remove an attribute or contained tag,
set it to nil.

In general, \(OBJ :METHOD ARGS ...) calls the method METHOD, possibly
with arguments ARGS.  Common methods include `xml' to build an XML
representation as a string, `send' to send to the Jabber server,
`typeof' to get the type of object, and `text' to get and set the
contained text \(if PCDATA).  Others are `attribs' and `contents',
which return the actual alists of attributes and contained tags
respectively, and some objects will have other methods, defined with
the METHODS parameter.  The method `methods' returns the alist of all
other methods.

A useful shortcut is the `->' method which calls a contained object
with arguments.  Thus \(OBJ :-> TAG ARGS ...) is equivalent to
\(funcall \(OBJ :=> TAG) ARGS ...).  In addition, if OBJ can have no
attribute named TAG, then calling \(OBJ TAG [ARG]) is equivalent to
\(OBJ :-> TAG :text [ARG]).

The `=>' and `->' methods and shortcuts only work for objects where
only a single instance is contained.  Multiply contained tags must be
accessed differently.  \(OBJ :==> TAG) returns a list of all TAGs in
OBJ, which can be mapped over or searched.  \(Often useful is `find'
with keyword argmuents `:key' \(see `ejab-call') and `:test' \(often
`string=')).  Currently this list cannot be modified after creation,
but its elements can be changed using their own methods.  When TAG is
singly contained in OBJ, `==>' is a one-element list."
  (let ((maker (ejab-maker-name name suffix)))
    `(progn
       (defun ,maker
         ,(if pcdata
              '(&optional text attribs contents)
            '(&optional attribs contents notused))
         ,(format "Make an ejab `%s' object%s.
Allowed Attributes: %s
Allowed Contents:   %s
Can%s Contain PCDATA
Special Methods:    %s
%s
This function was generated by `ejab-defobj', which see for further
information."
                  name
                  (if suffix
                      (format " in the %s namespace" suffix)
                    "")
                  (or attribs "None")
                  (if contents
                      (mapconcat #'(lambda (name)
                                     (format "%s\t-> `%s'" name
                                             (ejab-maker-name name)))
                                 contents
                                 "\n                    ")
                    "None")
                  (if pcdata "" "not")
                  (if methods
                      (mapconcat #'(lambda (pair)
                                     (format "%s\t-> `%s'"
                                             (car pair) (cdr pair)))
                                 methods
                                 "\n                    ")
                    "None")
                  (if docs
                      (format "\nDescription:        %s\n" docs)
                    "")
                   )
         ;; Let callers be lazy
         ,@(if pcdata
               `((unless (stringp text)
                   (psetq contents attribs
                          attribs text
                          text contents)))
             `((when (stringp attribs)
                 (psetq attribs contents
                        contents notused))))
         ;; Save all the data where we can access it later.
         (lexical-let
             ((name ',name)
              (text ,(if pcdata '(format "%s" text) nil))
              ;; After this, attribs and contents are alists with
              ;; values for all allowed items, whether nil or not.
              ;; Nil means there is no such attribute or object, but
              ;; there could be.
              (attribs
               (mapcar
                #'(lambda (attr)
                    (cons attr (ejab-get attr attribs)))
                ',attribs))
              (contents
               (mapcar
                #'(lambda (tag)
                    (cons tag
                          (remove nil
                                  (mapcar
                                   #'(lambda (obj)
                                       (ejab-ensure-object obj tag))
                                   (ejab-get-all tag contents)))))
                ',contents))
              (methods (copy-tree ',methods)))
           ;; Make a function to do the accessing.
           (labels
               ((this (attr &rest args)
                      (ejab-generic-object
                       name text attribs contents methods #'this
                       attr args)))
             #'this)))
       ,@(when suffix
           ;; Add the suffix to the list of namespaces for this
           ;; type of object.
           `((put ',(ejab-maker-name name)
                  'ejab-namespaces
                  (cons ',suffix
                        (get ',(ejab-maker-name name)
                             'ejab-namespaces)))
             ;; If we're not called from `ejab-defobj-by-namespace',
             ;; then call it to recalculate the docstring of the
             ;; generic maker function.
             (when (fboundp ',(ejab-maker-name name))
               ;; This actually does no harm if called recursively,
               ;; but being unnecessary we try to avoid it.
               (ejab-defobj-by-namespace ,name))
             ))
       )))

(put 'ejab-defobj 'lisp-indent-function 1)

(defmacro ejab-defobj-textonly (names)
  "Define a number of objects as text only.
This means no attributes, no contents, no extra methods, only PCDATA.
Each element of NAMES should be either a symbol NAME or a cons cell
\(NAME . DOCS)."
  `(progn
     ,@(mapcar #'(lambda (name)
                   (if (consp name)
                       `(ejab-defobj ,(car name) () () t ()
                                     ,(cdr name))
                     `(ejab-defobj ,name () () t)))
               names)))

(defmacro ejab-defobj-empty (names)
  "Define a number of objects as empty.
This means no attributes, no contents, no extra methods, no PCDATA.
Such elements are meaningful only by their presence versus absence.
Each element of NAMES should be either a symbol NAME or a cons cell
\(NAME . DOCS)."
  `(progn
     ,@(mapcar #'(lambda (name)
                   (if (consp name)
                       `(ejab-defobj ,(car name) () () nil ()
                                     ,(cdr name))
                     `(ejab-defobj ,name)))
               names)))

;;}}}
;;{{{ Namespace-Dependence

(defmacro ejab-defobj-by-namespace (name &rest namespaces)
  "Define an object type whose characteristics depend on a namespace.
NAME should be the name of the XML tag, such as `query', and each
NAMESPACE should be a list \(NS ATTRIBS CONTENTS PCDATA METHODS DOCS)
specifying the behavior of NAME in namespace NS \(a symbol or string).
ATTRIBS will automatically include `xmlns'.

As with `ejab-defobj', objects are created with `ejab-make-NAME'.  The
difference between namespace-dependent and -independent objects is
intended to be transparent.  The namespace must be specified when
creating an object, however \(with the attribute `xmlns'), and cannot
be changed once the object is created.  See `ejab-defobj' for how to
manipulate objects.

Once an object has been defined as namespace-dependent with this
function, new namespaces may be created by manually passing the SUFFIX
argument to `ejab-defobj'."
  `(progn
     ;; First make the individual namespace objects
     ,@(mapcar #'(lambda (ns)
                   `(ejab-defobj ,name
                      ,(if (memq 'xmlns (second ns))
                           (second ns)
                         (cons 'xmlns (second ns)))
                      ,(third ns) ; contents
                      ,(fourth ns) ; pcdata
                      ,(fifth ns) ; methods
                      ,(sixth ns) ; docs
                      ,(first ns) ; namespace (suffix)
                      ))
               namespaces)
     ;; Now make the general object that calls them all.
     (defun ,(ejab-maker-name name) (&rest args)
       ,(format "Make an ejab `%s' object.
Objects of this type are namespace-dependent.  The following
namespaces are defined for `%s' objects:

%s

This function was generated by `ejab-defobj-by-namespace', which see
for further information."
                name name
                (mapconcat #'(lambda (ns)
                               (format "%s\t-> `%s'" ns
                                       (ejab-maker-name name ns)))
                           (remove-duplicates
                            (append (mapcar #'car namespaces)
                                    (get (ejab-maker-name name)
                                         'ejab-namespaces)))
                           "\n"))
       (apply (ejab-maker-name
               ',name
               (or (ejab-get 'xmlns
                             ;; Skip text, if any.
                             (if (stringp (car args))
                                 (cadr args)
                               (car args)))
                   (ejab-notify 3 "xmlns attribute required for %s"
                                ',name)))
              args))))

(put 'ejab-defobj-by-namespace 'lisp-indent-function 1)

;;}}}
;;{{{ Describe Objects

(defun ejab-describe-object (object)
  "Display the documentation of the ejab object OBJECT.
This is actually the the function documentation of `ejab-make-OBJECT'."
  (interactive
   (list
    (completing-read "Describe Ejab Object: " obarray
                     #'(lambda (sym)
                         (fboundp (ejab-maker-name sym))))))
  (describe-function (ejab-maker-name object)))

;;}}}
;;{{{ Standard Jabber Objects

;; <message> elements
(ejab-defobj message
  (to from id type)
  (body subject thread error x)
  nil
  ((:recipients . ejab-message-recipients)
   )
  "A message from one user to another.")
(ejab-defobj-textonly (body subject thread))

;; <presence> elements
(ejab-defobj presence
  (to from type)
  (status priority show x)
  nil ; no text
  ((:jid . ejab-presence-jid)
   (:resource . ejab-presence-resource)
   )
  "Information, requests, or status of online presence."
  )
(ejab-defobj-textonly (status priority show))

;; <iq> elements
(ejab-defobj iq
  (to from id type)
  (query error vCard))

;; At some point we will want to implement this.
(ejab-defobj vCard ())

;; <error> elements
(ejab-defobj error (type) () t)

;; <query xmlns="..."> elements
(ejab-defobj-by-namespace query
  (jabber:iq:autoupdate () (release beta dev))
  (jabber:iq:agent () (agent))
  (jabber:iq:agents () (agent))
  (jabber:iq:auth () (username password digest resource))
  (jabber:iq:filter () (rule))
  (jabber:iq:oob () (url desc))
  (jabber:iq:private () ())
  (jabber:iq:register ()
                      (instructions username nick password name first
                                    last email address city state zip
                                    phone url date misc text key
                                    registered remove))
  (jabber:iq:roster () (item))
  (jabber:iq:search () ())
  (jabber:iq:time () (utc tz display))
  (jabber:iq:version () (name version os))
  )

;; jabber:iq:autoupdate
(ejab-defobj release (priority) (ver url desc))
(ejab-defobj beta (priority) (ver url desc))
(ejab-defobj dev (priority) (ver url desc))
(ejab-defobj-textonly (ver url desc))

;; jabber:iq:agent(s)
(ejab-defobj agent
  (jid)
  (name description transport service register search groupchat url
        agents))                ; This sometimes appears.  Why?
(ejab-defobj-textonly
 (name description transport service register search groupchat url
       agents))

;; jabber:iq:auth
(ejab-defobj-textonly (username password digest resource))

;; jabber:iq:filter
(ejab-defobj rule
  (name)
  ;; These are for matching
  (unavailable from resource subject body show
               ;; and these are for actions
               settype forward reply offline continue))
;; from is defined in jabber:x:envelope
;; subject and body are defined for message
;; show is defined for presence
(ejab-defobj-textonly (settype forward reply))
(ejab-defobj-empty (unavailable offline continue))

;; jabber:iq:oob
;;; These were already defined for autoupdate, although technically
;;; they are separate here, being in a different namespace.
;;;(ejab-defobj-textonly (url desc))

;; jabber:iq:private
;;; A query of this namespace can contain any valid XML to be stored
;;; on the server and later retrieved.  See the JPG for details.  We
;;; don't need to worry about it until we want to.

;; jabber:iq:register
(ejab-defobj-textonly 
 (instructions username nick password name first last email
               address city state zip phone url date misc text key
               registered remove))

;; jabber:iq:roster
(ejab-defobj item
  (jid name subscription ask)
  (group)
  nil
  ((:jid . ejab-item-jid)
   (:resource . ejab-item-resource)
   ))
(ejab-defobj-textonly (group))

;; jabber:iq:search
;;; This one is too complicated and server-dependent for now.  See the
;;; JPG for details on how it works.

;; jabber:iq:time
(ejab-defobj-textonly (utc tz display))

;; jabber:iq:version
(ejab-defobj-textonly (name version os))

;; <x xmlns="..."> elements
(ejab-defobj-by-namespace x
  (jabber:x:autoupdate
   () () t)
  (jabber:x:delay
   (stamp from) () t)
  (jabber:x:oob
   ()
   (url desc))
  (jabber:x:roster
   ()
   (item))
  ;; The dtd for this one is called jabber:x:ident
  (jabber:x:envelope
   ()
   (to from cc forwardedby replyto))
  )

;; jabber:x:envelope
(ejab-defobj to (jid) () t)
(ejab-defobj from (jid) () t)
(ejab-defobj cc (jid) () t)
(ejab-defobj forwardedby (jid) () t)
(ejab-defobj replyto (jid) () t)

;;}}}

(provide 'ejab-obj)

;;; ejab-obj.el ends here
