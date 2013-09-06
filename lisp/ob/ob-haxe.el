(require 'ob-ref);;; ob-template.el --- org-babel functions for template evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it
;; is a template for use in adding new language support to Org-babel.
;; Good first steps are to copy this file to a file named by the
;; language you are adding, and then use `query-replace' to replace
;; all strings of "template" in this file with the name of your new
;; language.
;;
;; If you have questions as to any of the portions of the file defined
;; below please look to existing language support for guidance.
;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at http://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.

;;; Requirements:

;; This relies on `haxe-mode' being available. `haxe-mode' itself
;; needs the following (available from MELPA):
;; - `auto-complete'
;; - `ede'
;; - `eieio'
;; - `ehelp'
;; - `cc-bytecomp'
;; - `cc-mode'
;; - `cc-fonts'
;; - `cc-langs'
;; - `cl'
;; - `compile'
;; - `flymake'
;; - `nxml-mode'
;; - `xml'
;; - `thingatpt'
;; - `speedbar' (optional)
;; Libraries I've written (available from Googlecode):
;; - `i++' <http://code.google.com/p/i-iterate/>
;; - `cl-format' <http://code.google.com/p/formatting-el/>
;; as well as HaXe compiler version 3.X. Some optional features may
;; use Python or Shell. So far tested only on Linux.

;;; Code:

(require 'ob)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))
(require 'haxe-mode)

(defvar org-babel-haxe-compiler "haxe"
  "Path to Haxe compiler.")

(defvar org-babel-haxe-backends
  '(("node" . "node")
    ("flash" . "flashplayer"))
  "Interpreters that can execute generated code.")

(defvar org-babel-haxe-args '("--connect")
  "Arguments used to compile Haxe code.")

(defvar org-babel-haxe-template
  "package %s; class %s{static function main(){%s}}"
  "The template to be used when we have incomplete code.")

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("haxe" . "hx"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:haxe '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:haxe' function below.
(defun org-babel-expand-body:haxe (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars
         (cl-remove-if-not
          #'consp
          (second (or processed-params
                      (org-babel-process-params params))))))
    (message "vars: %s" vars)
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "var %s = %S;"
                (car pair) (org-babel-haxe-var-to-haxe (cdr pair))))
      vars "\n") "\n" body "\n")))

(defun org-babel-haxe-backend-args (backend classname packagename)
  (cond
   ((string= backend "node")
    (cl-format "-js %s.js -main %[%s%:%]"
               classname
               (if (zerop (length packagename)) 2 1) packagename))
   (t (error "Backend %s is not supported." backend))))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.

(defun org-babel-haxe-strip-comments (code)
  ;; TODO: comments inside strings
  (++
    (with (linecommentp blockcommentp ca cb))
    (for a from 0 below (1- (length code)))
    (for b from 1)
    (setq ca (aref code a) cb (aref code b))
    (cond
     ((and (not blockcommentp) (not linecommentp)
           (char-equal ca ?\/) (char-equal cb ?\/))
      (setf linecommentp t))
     ((and (not blockcommentp) (not linecommentp)
           (char-equal ca ?\/) (char-equal cb ?*))
      (setf blockcommentp t))
     ((and blockcommentp (char-equal ca ?*) (char-equal cb ?\/))
      (setf blockcommentp nil))
     ((char-equal cb ?\n) (setf linecommentp nil)))
    (message "ca: %s a: %d, b: %d result: %s" ca a b (reverse result))
    (unless (or blockcommentp linecommentp)
      (collect ca into result))
    (finally
     (return
      (progn
        (unless linecommentp (push cb result))
        (coerce (nreverse result) 'string))))))

(defun org-babel-haxe-find-module-name (body)
  (let ((nocomment (org-babel-haxe-strip-comments body))
        case-fold-search)
    (string-match "class\\s-*\\([[:alnum:]_]+\\)" nocomment)
    (or (match-string 1 nocomment) "Template")))

(defun org-babel-execute:haxe (body params)
  "Execute a block of Haxe code with org-babel.  This function is
called by `org-babel-execute-src-block'"
  (message "executing Haxe source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         ;; (session (org-babel-haxe-initiate-session (first processed-params)))
         ;; variables assigned for use in the block
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         (cmpflag (or (cdr (assoc :cmpflag params)) ""))
         (cmdline (or (cdr (assoc :cmdline params)) ""))
         (backend (or (cdr (assoc :backend params)) "node"))
         (classname (or (cdr (assoc :classname params))
                        (org-babel-haxe-find-module-name body)))
         (packagename (file-name-directory classname))
         (src-file (concat classname ".hx"))
         ;; expand the body with `org-babel-expand-body:haxe'
         (full-body (org-babel-expand-body:haxe
                     body params processed-params)))
    (haxe-start-waiting-server)
    (message "src-file created in: %s" (expand-file-name src-file))
    (with-temp-file src-file (insert full-body))
    (org-babel-eval
     (cl-format
      "%{%s%^ %}"
      `(,org-babel-haxe-compiler
        ,@org-babel-haxe-args
        ,(format "%s:%d" haxe-host-default haxe-port-default)
        ,(org-babel-haxe-backend-args backend classname packagename)
        ,cmpflag, src-file)) "")
    (unless (or (not packagename) (file-exists-p packagename))
      (make-directory packagename 'parents))
    (org-babel-eval (concat backend " " cmdline " " classname) "")))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:haxe (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-haxe-var-to-haxe (var)
  "Convert an elisp var into a string of haxe source code
specifying a var of the same value."
  (if (listp var)
      (cl-format "[%{%/org-babel-js-var-to-js/%^, %}]" var)
    (format "%S" var)))

(defun org-babel-haxe-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-haxe-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (haxe-start-waiting-server)))

(provide 'ob-haxe)
;;; ob-haxe.el ends here
