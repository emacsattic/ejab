;;; ejab-xml.el --- Parse and generate XML for ejab

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

;; This file defines functions that parse XML into ejab objects and
;; generate XML from ejab objects.

;;; Code:

(require 'cl)
(require 'ejab-obj)

;;{{{ XML Escaping

(defvar ejab-xml-escapes-alist
  '(("&" . "amp")
    ("'" . "apos"))
  "Alist of entities that are escaped in XML.
Each element is \(ENTITY . NAME).")

(defun ejab-escape-xml (text)
  "Escape entites in `ejab-xml-escapes-alist' in TEXT."
  (when text
    (let ((pos 0)
          (chars (concat "["
                         (regexp-quote
                          (mapconcat #'car ejab-xml-escapes-alist ""))
                         "]")))
      (while (string-match chars text pos)
        (setq text (replace-match
                    (concat "&" (cdr (assoc (match-string 0 text)
                                            ejab-xml-escapes-alist))
                            ";")
                    t t text)
              pos (match-end 0)))
      text)))

(defun ejab-unescape-xml (text)
  "Unescape entites in `ejab-xml-escapes-alist' in TEXT."
  (when text
    (let ((pos 0)
          (entities (concat "&"
                            (regexp-opt
                             (mapcar #'cdr ejab-xml-escapes-alist) t)
                            ";")))
      (while (string-match entities text pos)
        (setq text (replace-match
                    (car (rassoc (match-string 1 text)
                                 ejab-xml-escapes-alist))
                    t t text)
              pos (match-end 0)))
      text)))

;;}}}
;;{{{ Convert to XML

(defun ejab-obj->xml (object)
  "Return an XML representation of OBJECT.
Only for internal use--programs should call \(OBJECT :xml)."
  (ejab-with (object)
    (concat "<" (symbol-name (object :typeof))
            (mapconcat #'(lambda (pair)
                           (if (cdr pair)
                               (format " %s='%s'"
                                       (car pair) (cdr pair))
                             ""))
                       (object :attribs) "")
            ;; Check for empty tag (just for aesthetics)
            (if (or (object :text)
                    (some #'cdr (object :contents)))
                (concat ">"
                        (mapconcat #'(lambda (pair)
                                       (if (cdr pair)
                                           (mapconcat
                                            #'(lambda (tag)
                                                (funcall tag :xml))
                                            (cdr pair) "")
                                         ""))
                                   (object :contents) "")
                        (or (ejab-escape-xml (object :text)) "")
                        "</" (symbol-name (object :typeof)) ">")
              "/>"))))

;;}}}
;;{{{ Convert from XML

(defun ejab-parse-xml ()
  "Parse XML in the current buffer.
Return an ejab object as long as there is a complete XML tag in the
buffer to be parsed, otherwise throw `ejab-incomplete-tag', meaning
wait until more text is added.  In this case, the caller should
restore the value of point before the call \(oh, for a coroutine...).

Note that this function *never* returns nil.  It either parses and
returns a complete ejab object, or throws `ejab-incomplete-tag'."
  (let (tag endtag maker attr attribs contents (text ""))
    ;; First the tag name
    (or (looking-at "\\s-*<\\(\\w+\\)")
        (throw 'ejab-incomplete-tag t))
    (setq tag (match-string 1)
          endtag (format "\\s-*</%s>" tag)
          maker (ejab-maker-name tag))
    (goto-char (match-end 0))
    ;; Now the attributes
    (while (looking-at "\\s-+\\(\\w+\\)=['\"]")
      (setq attr (intern (match-string 1)))
      ;; The 1 is intentional, so that the next search can still match
      ;; empty attribute values.
      (goto-char (match-end 1))
      (or (looking-at "='\\([^']*\\)'")
          (looking-at "=\"\\([^\"]*\\)\"")
          (throw 'ejab-incomplete-tag t))
      (setq attribs (cons (cons attr (match-string 1)) attribs))
      (goto-char (match-end 0)))
    ;; The start tag must end, possibly being empty.
    (or (looking-at "\\s-*\\(/\\)?>")
        (throw 'ejab-incomplete-tag t))
    (goto-char (match-end 0))
    (if (match-string 1)
        ;; We have an empty tag, so make and return it.
        (funcall maker attribs)
      ;; Process contents recursively
      (while (not (looking-at endtag))
        ;; Process any text
        (when (looking-at "\\s-*\\([^<]+\\)\\s-*<")
          (setq text (concat text (match-string 1)))
          ;; The 1 is so we can still see the `<'.
          (goto-char (match-end 1)))
        ;; Process contained tag if any.
        (unless (looking-at endtag)
          (let ((obj (ejab-parse-xml)))
            (setq contents
                  (cons (cons (funcall obj :typeof) obj) contents)))))
      ;; Tag is now over, so make and return it.
      (goto-char (match-end 0))
      (funcall maker attribs contents (ejab-unescape-xml text)))))

;;}}}

(provide 'ejab-xml)

;;; ejab-xml.el ends here
