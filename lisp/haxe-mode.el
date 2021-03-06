;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) 2006-2007 Jens Peter Secher
;; https://github.com/pdorrell/emacs-site-lisp/blob/master/haxe-mode.el
;; Copyright (C) Ritchie Turner (blackdog@cloudshift.cl)
;; https://github.com/cloudshift/hx-emacs/blob/master/hxc-complete.el
;; Copyright (C) Oleg Sivokon (olegsivokon@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ------------------------------------------------------------------------

;; This is my (Oleg) modification and extension of the original haxe-mode
;; written by Jens Peter Secher, it also borrows from the auto-completion
;; extension written by Ritchie Turner. The original commentary follows.
;; This mode introduces dependency to auto-complete library, one can be
;; found here: http://cx4a.org/software/auto-complete/

;; This is haxe-mode, an Emacs major mode for the haXe programming
;; language (http://haxe.org).

;; haxe-mode is built on top of the excellent cc-mode, inspired by the
;; guide http://cc-mode.sourceforge.net/derived-mode-ex.el.

;; haxe-mode is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.1.1 - Fixed typedef indentation.
;;            Fixed lexical analysis so that type names can contain digits.
;;    0.2.0 - Base on java-mode instead of c++-mode.
;;            Added compile-error parser for the haXe compiler output.
;;            Loads of improvements.
;;    0.2.1 - Fix buffer-local comment-start-skip problem.
;;    0.2.2 - Recognize keyword override.
;;    0.3.0 - Switched to GPLv3 license because that is what cc-mode is using.
;;    0.3.1 - Fix compile problem with emacs23.
;;

;;; Usage:
;;
;; Include something like this in your .emacs:
;; (require 'haxe-mode)
;; (defconst my-haxe-style
;;   '("java" (c-offsets-alist . ((case-label . +)
;;                                (arglist-intro . +)
;;                                (arglist-cont-nonempty . 0)
;;                                (arglist-close . 0)
;;                                (cpp-macro . 0))))
;;   "My haXe Programming Style")
;; (add-hook 'haxe-mode-hook
;;   (function (lambda () (c-add-style "haxe" my-haxe-style t))))
;; (add-hook 'haxe-mode-hook
;;           (function
;;            (lambda ()
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode t)
;;              (setq fill-column 80)
;;              (local-set-key [(return)] 'newline-and-indent))))


;;; Code:

(eval-when-compile (require 'cl))
(require 'cc-bytecomp)
(require 'cc-mode)
(require 'cc-fonts)
;; (cc-require-when-compile 'cc-langs)
(require 'cc-langs)

(require 'compile)
;; ------------------- my change -------------------------------------
(require 'flymake)
(require 'xml)
(require 'ehelp)
(require 'speedbar nil t)
(require 'haxe-utils)
(require 'haxe-help)
(require 'haxe-project)
(require 'haxe-completion)
(require 'haxe-log)
(require 'haxe-compiler-mode)
(require 'custom/create-project)
(require 'ede/haxe)
(require 'ede/haxe-speedbar)
;; ------------------- my change -------------------------------------

;; The language constants are needed when compiling.
;; (eval-when-compile
;;   (let ((load-path
;;          (if (and (boundp 'byte-compile-dest-file)
;;                   (stringp byte-compile-dest-file))
;;              (cons (file-name-directory byte-compile-dest-file) load-path)
;;            load-path)))
;;     (load "cc-mode" nil t)
;;     (load "cc-fonts" nil t)
;;     (load "cc-langs" nil t)
;;     (load "cc-bytecomp" nil t)))

(eval-and-compile
  ;; Tell the language constant system about haXe and base it on Java.
  (c-add-language 'haxe-mode 'java-mode))

;;; Lexer-level syntax (identifiers, tokens etc).

;; No other operators in identifiers.
(c-lang-defconst c-after-id-concat-ops
  haxe nil)

;; Conditional compilation and metadata prefices.
(c-lang-defconst c-opt-cpp-prefix
  haxe "\\s *#")

;; ---------------------------------------------------- < my change >

(c-lang-defconst c-opt-cpp-macro-define
  haxe nil)

(c-lang-defconst c-opt-cpp-macro-define-start
  haxe nil)

(c-lang-defconst c-opt-cpp-macro-define-id
  haxe nil)

(c-lang-defconst c-symbol-start
  haxe (concat "[" c-alpha "_@]:?"))

;; --------------------------------------------------- < end my change >

;; No strings in conditional compilation.
(c-lang-defconst c-cpp-message-directives
  haxe nil)

;; No file name in angle brackets or quotes in conditional compilation.
(c-lang-defconst c-cpp-include-directives
  haxe nil)

;; No macro definition in conditional compilation.
(c-lang-defconst c-opt-cpp-macro-define
  haxe nil)

;; Conditional compilation directives followed by expressions.
(c-lang-defconst c-cpp-expr-directives
  haxe '("if" "else"))

;; No functions in conditional compilation.
(c-lang-defconst c-cpp-expr-functions
  haxe nil)

;; haXe operators.
(c-lang-defconst c-operators
  haxe `(
         ;; Preprocessor.
         (prefix "#")
         ;; Standard operators.
         ,@(c-lang-const c-identifier-ops)
         ;; Generics.
         (postfix-if-paren "<" ">")
         ;; Postfix.
         (left-assoc "." "->")
         (postfix "++" "--" "[" "]" "(" ")")
         ;; Unary.
         (prefix "++" "--" "+" "-" "!" "~" "new")
         ;; Multiplicative.
         (left-assoc "*" "/" "%")
         ;; Additive.
         (left-assoc "+" "-")
         ;; Shift.
         (left-assoc "<<" ">>" ">>>")
         ;; Relational.
         (left-assoc "<" ">" "<=" ">=")
         ;; Iteration.
         (left-assoc "...")
         ;; Equality.
         (left-assoc "==" "!=" "===" "!==")
         ;; Bitwise and.
         (left-assoc "&")
         ;; Bitwise exclusive or.
         (left-assoc "^")
         ;; Bitwise or.
         (left-assoc "|")
         ;; Logical and.
         (left-assoc "&&")
         ;; Logical or.
         (left-assoc "||")
         ;; Assignment.
         (right-assoc ,@(c-lang-const c-assignment-operators))
         ;; Exception.
         (prefix "throw")
         ;; Sequence.
         (left-assoc ",")))

;; No overloading.
(c-lang-defconst c-overloadable-operators
  haxe nil)
(c-lang-defconst c-opt-op-identitier-prefix
  haxe nil)

;;; Keywords.

;; I will treat types uniformly below since they all start with capital
;; letters.
(c-lang-defconst c-primitive-type-kwds
  haxe nil)

;; TODO: check double occurrence of enum.
;; Type-introduction is straight forward in haXe.
(c-lang-defconst c-class-decl-kwds
  haxe '( "class" "interface" "enum" "typedef" ))

;; Recognises enum constants.
;; TODO: find a way to also recognise parameterised constants.
(c-lang-defconst c-brace-list-decl-kwds
  haxe '( "enum" ))

;; Keywords introducing declarations where the identifier follows directly
;; after the keyword, without any type.
(c-lang-defconst c-typeless-decl-kwds
  haxe (append '( "function" "var" )
               (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds)))
  
;; Definition modifiers.
(c-lang-defconst c-modifier-kwds
  haxe '( "private" "public" "static" "override" "inline"))
(c-lang-defconst c-other-decl-kwds
  haxe nil)

;; Namespaces.
(c-lang-defconst c-ref-list-kwds
 haxe '( "import" "package" "using"))

;; Statement keywords followed directly by a substatement.
(c-lang-defconst c-block-stmt-1-kwds
  haxe '( "do" "else" "try" ))

;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  haxe '( "for" "if" "switch" "while" "catch" ))

;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  haxe '( "break" "continue" "return" "default" "new" ))

;; No ';' inside 'for'.
(c-lang-defconst c-paren-stmt-kwds
  haxe nil)

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  haxe '( "false" "true" "null" ))

;; Keywords for expressions.
(c-lang-defconst c-primary-expr-kwds
  haxe '( "this" "super" ))

(c-lang-defconst c-decl-hangon-kwds
  haxe '( "in" ))

;; No other labels.
(c-lang-defconst c-before-label-kwds
  haxe nil)

;; No classes inside expressions.
(c-lang-defconst c-inexpr-class-kwds
  haxe nil)

;; No brace lists inside expressions.
(c-lang-defconst c-inexpr-brace-list-kwds
  haxe nil)

;; All identifiers starting with a capital letter are types.
(c-lang-defconst c-cpp-matchers
  haxe (append
        (c-lang-const c-cpp-matchers c)
        '(("\\<\\([A-Z][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face))
        '(("\\<\\(@:?[A-Za-z]+\\)\\>" 1 c-annotation-face))))

;; Generic types.
(c-lang-defconst c-recognize-<>-arglists
  haxe t)

;; Fontification degrees.
(defconst haxe-font-lock-keywords-1 (c-lang-const c-matchers-1 haxe)
  "Minimal highlighting for haxe mode.")

(defconst haxe-font-lock-keywords-2 (c-lang-const c-matchers-2 haxe)
  "Fast normal highlighting for haxe mode.")

(defconst haxe-font-lock-keywords-3 (c-lang-const c-matchers-3 haxe)
  "Accurate normal highlighting for haxe mode.")

(defvar haxe-font-lock-keywords haxe-font-lock-keywords-3
  "Default expressions to highlight in haxe mode.")

(defvar haxe-mode-syntax-table nil
  "Syntax table used in HaXe mode buffers.")
(or haxe-mode-syntax-table
    (setq haxe-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table haxe))))

(defvar haxe-mode-abbrev-table nil
  "Abbreviation table used in haxe mode buffers.")
(c-define-abbrev-table 'haxe-mode-abbrev-table
  ;; Keywords that, if they occur first on a line, might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar haxe-mode-map ()
  "Keymap used in haxe mode buffers.")
(unless haxe-mode-map
  (setq haxe-mode-map (c-make-inherited-keymap)))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(make-variable-buffer-local 'compile-command)

;; Tell compilation-mode how to parse error messages.  You need to set
;; compilation-error-screen-columns to nil to get the right
;; interpretation of tabs.
(add-to-list 'compilation-error-regexp-alist
             '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
               1 2 3))

;; ------------------- My edits --------------------------------------------

(defadvice c-forward-annotation
  (around haxe-forward-annotation ())
  "Overrides `c-forward-annotation' to be able to use @:\w+ syntax as well
as the Java original syntax."
  (and (looking-at "@")
       (progn (forward-char) t)
       (if (looking-at ":")
           (progn
             (message "looked at :")
             (forward-char)
             (c-forward-type))
         (c-forward-type))
       (progn
         (message "and stepped word")
         (c-forward-syntactic-ws) t)
       (if (looking-at "(")
           (c-go-list-forward)
         t)))

(defadvice c-beginning-of-macro
  (around haxe-beginning-of-macro (&optional lim))
  "This completely shuts down `cpp-macro' syntax because it clashes
with HaXe macro metadata." nil)

(c-lang-defconst c-complex-decl-matchers
  t `(c-font-lock-complex-decl-prepare
      ,@(if (c-major-mode-is 'objc-mode)
            `(,(c-make-font-lock-search-function
                (c-make-keywords-re t
                  (delete "@class"
                          (append (c-lang-const c-protection-kwds)
                                  (c-lang-const c-other-decl-kwds)
                                  nil)))
                '((c-put-char-property (1- (match-end 1))
                                       'c-type 'c-decl-end)))
              c-font-lock-objc-methods))
      c-font-lock-declarations
      c-font-lock-enclosing-decls
      ,@(when (c-lang-const c-recognize-<>-arglists)
          `(c-font-lock-<>-arglists))
      ,(let ((re (c-make-keywords-re nil
                   (c-lang-const c-primitive-type-kwds))))
         (if (c-major-mode-is 'pike-mode)
             `(,(concat "\\(\\=.?\\|[^>]\\|[^-]>\\)"
                        "\\<\\(" re "\\)\\>")
               2 font-lock-type-face)
           `(,(concat "\\<\\(" re "\\)\\>")
             1 'font-lock-type-face)))
      ,@(when (c-lang-const c-type-prefix-kwds)
          `((,(byte-compile
               `(lambda (limit)
                  (c-fontify-types-and-refs
                      ((c-promote-possible-types t)
                       (parse-sexp-lookup-properties
                        (eval-when-compile
                          (boundp 'parse-sexp-lookup-properties))))
                    (save-restriction
                      (narrow-to-region (point) limit)
                      (while (re-search-forward
                              ,(concat "\\<\\("
                                       (c-make-keywords-re nil
                                         (c-lang-const c-type-prefix-kwds))
                                       "\\)\\>")
                              limit t)
                        (unless (c-skip-comments-and-strings limit)
                          (c-forward-syntactic-ws)
                          ;; My change was to add a colon to the expresion
                          ;; the rest is identical to the original definition in c-mode.
                          ;; I've removed all other comments to make the code shorter.
                          (when (or (looking-at c-prefix-spec-kwds-re)
                                    (and (c-major-mode-is 'java-mode)
                                         (looking-at "@?:[A-Za-z0-9]+")))
                            (c-forward-keyword-clause 1))
                          ,(if (c-major-mode-is 'c++-mode)
                               `(when (and (c-forward-type)
                                           (eq (char-after) ?=))
                                  (forward-char)
                                  (c-forward-syntactic-ws)
                                  (c-forward-type))
                             `(c-forward-type)))))))))))
      ,@(when (c-major-mode-is 'c++-mode)
          `(("\\<new\\>"
             (c-font-lock-c++-new))))))

(c-lang-defconst c-basic-matchers-after
  t `(,@(when (c-lang-const c-brace-id-list-kwds)
          `(c-font-lock-enum-tail
            (,(c-make-font-lock-search-function
               (concat
                "\\<\\("
                (c-make-keywords-re nil (c-lang-const c-brace-id-list-kwds))
                "\\)\\>"
                "[^\]\[{}();,/#=]*"
                "{")
               '((c-font-lock-declarators limit t nil)
                 (save-match-data
                   (goto-char (match-end 0))
                   (c-put-char-property (1- (point)) 'c-type
                                        'c-decl-id-start)
                   (c-forward-syntactic-ws))
                 (goto-char (match-end 0)))))))

      ,@(when (c-lang-const c-before-label-kwds)
          `((eval
             . ,(let* ((c-before-label-re
                        (c-make-keywords-re nil
                          (c-lang-const c-before-label-kwds))))
                  `(list
                    ,(concat "\\<\\(" c-before-label-re "\\)\\>"
                             "\\s *"
                             "\\("	; identifier-offset
                             (c-lang-const c-symbol-key)
                             "\\)")
                    (list ,(+ (regexp-opt-depth c-before-label-re) 2)
                          c-label-face-name nil t))))))
      ,@(when (or (c-lang-const c-type-list-kwds)
                  (c-lang-const c-ref-list-kwds)
                  (c-lang-const c-colon-type-list-kwds))
          `((,(c-make-font-lock-BO-decl-search-function
               (concat "\\<\\("
                       (c-make-keywords-re nil
                         (append (c-lang-const c-type-list-kwds)
                                 (c-lang-const c-ref-list-kwds)
                                 (c-lang-const c-colon-type-list-kwds)))
                       "\\)\\>")
               '((c-fontify-types-and-refs ((c-promote-possible-types t))
                   (c-forward-keyword-clause 1)
                   (if (> (point) limit) (goto-char limit))))))))

      ,@(when (c-lang-const c-paren-type-kwds)
          `((,(c-make-font-lock-search-function
               (concat "\\<\\("
                       (c-make-keywords-re nil
                         (c-lang-const c-paren-type-kwds))
                       "\\)\\>")
               '((c-fontify-types-and-refs ((c-promote-possible-types t))
                   (c-forward-keyword-clause 1)
                   (if (> (point) limit) (goto-char limit))))))))
      ;; This is my change on top of the original c-mode, the rest is idenatical
      ;; to the original. Removed other comments for shortness.
      ,@(when (c-major-mode-is 'java-mode)
          `((eval . (list "\\<\\(@:?[a-zA-Z0-9]+\\)\\>" 1 c-annotation-face))))))

;; TODO: find the way to reuse the original, this function is huge, don't want
;; to duplicate it here.
(defadvice c-forward-decl-or-cast-1
  (around haxe-forward-decl-or-cast-1
          (preceding-token-end context last-cast-end))
  "See the original documentation in the `c-forward-decl-or-cast-1'"
  (let ((start-pos (point))
        at-type
        type-start
        id-start
        backup-at-type backup-type-start backup-id-start
        at-type-decl
        at-typedef
        maybe-typeless
        backup-at-type-decl backup-maybe-typeless
        at-decl-or-cast
        backup-if-not-cast
        cast-end
        (save-rec-type-ids c-record-type-identifiers)
        (save-rec-ref-ids c-record-ref-identifiers))

    (while (c-forward-annotation)
      (c-forward-syntactic-ws))
    (while
        (let* ((start (point)) kwd-sym kwd-clause-end found-type)
          (when (or (looking-at c-prefix-spec-kwds-re)
                    (and (c-major-mode-is 'java-mode)
                         ;; my change: added colon to the expression
                         (looking-at "@:?[A-Za-z0-9]+")))
            (if (looking-at c-typedef-key)
                (setq at-typedef t))
            (setq kwd-sym (c-keyword-sym (match-string 1)))
            (save-excursion
              (c-forward-keyword-clause 1)
              (setq kwd-clause-end (point))))
          (when (setq found-type (c-forward-type t)) ; brace-block-too
            (when at-type
              (setq at-decl-or-cast 'ids)
              (when (eq at-type 'found)
                (save-excursion
                  (goto-char type-start)
                  (let ((c-promote-possible-types t))
                    (c-forward-type)))))
            (setq backup-at-type at-type
                  backup-type-start type-start
                  backup-id-start id-start
                  at-type found-type
                  type-start start
                  id-start (point)
                  backup-at-type-decl nil
                  backup-maybe-typeless nil))
          (if kwd-sym
              (progn
                (if (c-keyword-member kwd-sym 'c-decl-hangon-kwds)
                    (progn
                      (setq at-decl-or-cast t)
                      (if at-type
                          (setq id-start kwd-clause-end)
                        (setq start-pos kwd-clause-end))
                      (goto-char kwd-clause-end))
                  (setq backup-at-type nil
                        start-pos kwd-clause-end)
                  (if found-type
                      (progn
                        (when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
                          (setq backup-at-type-decl t))
                        (when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
                          (setq backup-maybe-typeless t)))
                    (when (c-keyword-member kwd-sym 'c-typedef-decl-kwds)
                      (setq at-type-decl t))
                    (when (c-keyword-member kwd-sym 'c-typeless-decl-kwds)
                      (setq maybe-typeless t))
                    (setq at-decl-or-cast t)
                    (goto-char kwd-clause-end))))
            (and found-type (not (eq found-type t))))))
    (cond
     ((eq at-type t)
      (while (looking-at c-decl-hangon-key)
        (c-forward-keyword-clause 1))
      (setq id-start (point)))
     ((eq at-type 'prefix)
      (setq at-type t))
     ((not at-type)
      (setq id-start start-pos))
     ((and (eq at-type 'maybe)
           (c-major-mode-is 'c++-mode))
      (save-excursion
        (let (name end-2 end-1)
          (goto-char id-start)
          (c-backward-syntactic-ws)
          (setq end-2 (point))
          (when (and
                 (c-simple-skip-symbol-backward)
                 (progn
                   (setq name
                         (buffer-substring-no-properties (point) end-2))
                   (< (skip-chars-backward ":~ \t\n\r\v\f") 0))
                 (progn
                   (setq end-1 (point))
                   (c-simple-skip-symbol-backward))
                 (>= (point) type-start)
                 (equal (buffer-substring-no-properties (point) end-1)
                        name))
            (goto-char type-start)
            (setq at-type nil
                  backup-at-type nil
                  id-start type-start))))))
    (let ((start (point)) (paren-depth 0) pos
          got-prefix
          got-parens
          got-identifier
          got-suffix
          got-prefix-before-parens
          got-suffix-after-parens
          at-decl-end
          identifier-type identifier-start
          c-parse-and-markup-<>-arglists)
      (goto-char id-start)
      (while (and (looking-at c-type-decl-prefix-key)
                  (if (and (c-major-mode-is 'c++-mode)
                           (match-beginning 3))
                      (when (setq got-identifier (c-forward-name))
                        (if (looking-at "\\(::\\)")
                            (progn (setq got-identifier nil) t)
                          nil))
                    t))
        (if (eq (char-after) ?\()
            (progn
              (setq paren-depth (1+ paren-depth))
              (forward-char))
          (unless got-prefix-before-parens
            (setq got-prefix-before-parens (= paren-depth 0)))
          (setq got-prefix t)
          (goto-char (match-end 1)))
        (c-forward-syntactic-ws))
      (setq got-parens (> paren-depth 0))
      (or got-identifier
          (and (looking-at c-identifier-start)
               (setq got-identifier (c-forward-name))))
      (while (if (looking-at c-type-decl-suffix-key)
                 (if (eq (char-after) ?\))
                     (when (> paren-depth 0)
                       (setq paren-depth (1- paren-depth))
                       (forward-char)
                       t)
                   (when (if (save-match-data (looking-at "\\s\("))
                             (c-safe (c-forward-sexp 1) t)
                           (goto-char (match-end 1))
                           t)
                     (when (and (not got-suffix-after-parens)
                                (= paren-depth 0))
                       (setq got-suffix-after-parens (match-beginning 0)))
                     (setq got-suffix t)))
               (when (and (= paren-depth 1)
                          (not got-prefix-before-parens)
                          (not (eq at-type t))
                          (or backup-at-type
                              maybe-typeless
                              backup-maybe-typeless
                              (when c-recognize-typeless-decls
                                (not context)))
                          (setq pos (c-up-list-forward (point)))
                          (eq (char-before pos) ?\)))
                 (c-fdoc-shift-type-backward)
                 (goto-char pos)
                 t))

        (c-forward-syntactic-ws))

      (when (and (or maybe-typeless backup-maybe-typeless)
                 (not got-identifier)
                 (not got-prefix)
                 at-type)
        (c-fdoc-shift-type-backward))
      (setq
       at-decl-or-cast
       (catch 'at-decl-or-cast
         (when (> paren-depth 0)
           (c-safe (goto-char (scan-lists (point) 1 paren-depth)))
           (throw 'at-decl-or-cast (eq at-decl-or-cast t)))
         (setq at-decl-end
               (looking-at (cond ((eq context '<>) "[,>]")
                                 (context "[,\)]")
                                 (t "[,;]"))))
         (if got-identifier
             (progn
               (when (and (or at-type maybe-typeless)
                          (not (or got-prefix got-parens)))
                 (throw 'at-decl-or-cast t))

               (when (and got-parens
                          (not got-prefix)
                          (not got-suffix-after-parens)
                          (or backup-at-type
                              maybe-typeless
                              backup-maybe-typeless))
                 (c-fdoc-shift-type-backward)))
           (if backup-at-type
               (progn
                 (when (= (point) start)
                   (if (and (eq (char-after) ?:)
                            (not (c-major-mode-is 'java-mode)))
                       (cond
                        ((eq at-decl-or-cast t)
                         (throw 'at-decl-or-cast t))
                        ((and c-has-bitfields
                              (eq at-decl-or-cast 'ids)) ; bitfield.
                         (setq backup-if-not-cast t)
                         (throw 'at-decl-or-cast t)))
                     (setq backup-if-not-cast t)
                     (throw 'at-decl-or-cast t)))
                 (when (and got-suffix
                            (not got-prefix)
                            (not got-parens))
                   (setq backup-if-not-cast t)
                   (throw 'at-decl-or-cast t)))
             (when (eq at-type t)
               (throw 'at-decl-or-cast t))

             (when (= (point) start)
               (if (and
                    at-decl-end
                    (cond
                     ((eq context 'decl)
                      (or (and (not c-recognize-knr-p)
                               (not c-recognize-paren-inits))
                          (memq at-type '(known found))))
                     ((eq context '<>)
                      (memq at-type '(known found)))))
                   (throw 'at-decl-or-cast t)
                 (throw 'at-decl-or-cast at-decl-or-cast))))
           (if (and got-parens
                    (not got-prefix)
                    (not context)
                    (not (eq at-type t))
                    (or backup-at-type
                        maybe-typeless
                        backup-maybe-typeless
                        (when c-recognize-typeless-decls
                          (or (not got-suffix)
                              (not (looking-at
                                    c-after-suffixed-type-maybe-decl-key))))))
               (c-fdoc-shift-type-backward)
             (when (and got-prefix (or got-parens got-suffix))
               (throw 'at-decl-or-cast t))
             (when (and at-type
                        (not got-prefix)
                        (not got-parens)
                        got-suffix-after-parens
                        (eq (char-after got-suffix-after-parens) ?\())
               (throw 'at-decl-or-cast nil))))
         (when at-decl-or-cast
           (throw 'at-decl-or-cast t))
         (when (and got-identifier
                    (not context)
                    (looking-at c-after-suffixed-type-decl-key)
                    (if (and got-parens
                             (not got-prefix)
                             (not got-suffix)
                             (not (eq at-type t)))
                        (progn (c-fdoc-shift-type-backward) t)
                      got-suffix-after-parens))
           (throw 'at-decl-or-cast t))
         (when (and (or got-prefix (not got-parens))
                    (memq at-type '(t known)))
           (throw 'at-decl-or-cast t))
         (unless (or at-decl-end (looking-at "=[^=]"))
           (throw 'at-decl-or-cast at-decl-or-cast))
         (when (memq at-type '(t known))
           (throw 'at-decl-or-cast t))
         (when (and (c-major-mode-is 'c++-mode)
                    identifier-type
                    (or (memq identifier-type '(found known))
                        (and (eq (char-after identifier-start) ?~)
                             (or (save-excursion
                                   (goto-char (1+ identifier-start))
                                   (c-forward-syntactic-ws)
                                   (c-with-syntax-table
                                       c-identifier-syntax-table
                                     (looking-at c-known-type-key)))
                                 (save-excursion
                                   (goto-char (1+ identifier-start))
                                   (c-check-type (point)
                                                 (progn (c-forward-type)
                                                        (point))))))))
           (throw 'at-decl-or-cast t))
         (if got-identifier
             (progn
               (when (and got-prefix-before-parens
                          at-type
                          (or at-decl-end (looking-at "=[^=]"))
                          (not context)
                          (not got-suffix))
                 (throw 'at-decl-or-cast t))
               (when (and (or got-suffix-after-parens
                              (looking-at "=[^=]"))
                          (eq at-type 'found)
                          (not (eq context 'arglist)))
                 (throw 'at-decl-or-cast t)))
           (when (and context
                      (or got-prefix
                          (and (eq context 'decl)
                               (not c-recognize-paren-inits)
                               (or got-parens got-suffix))))
             (throw 'at-decl-or-cast t)))
         (eq context 'decl))))
    (cond
     ((save-excursion
        (and
         c-cast-parens
         (> preceding-token-end (point-min))
         (memq (char-before preceding-token-end) c-cast-parens)
         (progn
           (c-forward-syntactic-ws)
           (looking-at "\\s\)"))
         (let (pos)
           (forward-char)
           (c-forward-syntactic-ws)
           (setq cast-end (point))
           (and (looking-at c-primary-expr-regexp)
                (progn
                  (setq pos (match-end 0))
                  (or
                   (match-beginning 2)
                   (if (match-beginning 1)
                       (or at-decl-or-cast
                           (memq at-type '(t known found)))
                     (not (looking-at c-keywords-regexp)))))
                (or (not (looking-at c-nonsymbol-token-regexp))
                    (<= (match-end 0) pos))))
         (> preceding-token-end (point-min))
         (progn
           (goto-char (1- preceding-token-end))
           (or (eq (point) last-cast-end)
               (progn
                 (c-backward-syntactic-ws)
                 (if (< (skip-syntax-backward "w_") 0)
                     (looking-at c-simple-stmt-key)
                   (and
                    (not (memq (char-before) '(?\) ?\])))
                    (not (c-on-identifier)))))))))
      (when (and c-record-type-identifiers at-type (not (eq at-type t)))
        (let ((c-promote-possible-types t))
          (goto-char type-start)
          (c-forward-type)))
      (goto-char cast-end)
      'cast)
     (at-decl-or-cast
      (when backup-if-not-cast
        (c-fdoc-shift-type-backward t))
      (when (and (eq context 'decl) (looking-at ","))
        (c-put-c-type-property (point) 'c-decl-arg-start))
      (when (and c-record-type-identifiers at-type (not (eq at-type t)))
        (let ((c-promote-possible-types t))
          (save-excursion
            (goto-char type-start)
            (c-forward-type))))
      (cons id-start
            (and (or at-type-decl at-typedef)
                 (cons at-type-decl at-typedef))))
     (t
      (setq c-record-type-identifiers save-rec-type-ids
            c-record-ref-identifiers save-rec-ref-ids)
      nil))))

;; TODO: we need to use when generating TAGS, but before we
;; can use it, we need a good way to generate TAGS for HaXe code
(defcustom haxe-std-library nil
  "The location of HaXe built-ins, it is needed for TAGS generation"
  :type 'string :group 'haxe-mode)

(defun haxe-flymake-install ()
  "Install flymake stuff for HaXe files."
  ;; TODO: It looks like Flymake will use a special variable to get
  ;; the errors / warnings message, we should be planning ahead for
  ;; plugging into it, rather then using the compile.el variables.
  ;; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  ;; (if (boundp 'flymake-err-line-patterns)
  ;;     (add-to-list
  ;;      'flymake-err-line-patterns
  ;;      '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
  ;;        1 2 3)))
  (add-to-list
   'compilation-error-regexp-alist
   '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
     1 2 3))
  (flymake-log 3 "HaXe flymake installed")
  (save-window-excursion
    (let ((con (haxe-start-waiting-server)))
      (oset con project haxe-current-project)))
  ;; FIXME: This looks like it has to happen after we are connected
  ;; to the server the server start asynchronously
  (let* ((key "\\.hx\\'")
         (haxeentry (assoc key flymake-allowed-file-name-masks)))
    (if haxeentry
        (setcdr haxeentry
                '(haxe-flymake-init
                  haxe-flymake-cleanup
                  haxe-get-real-file-name))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'haxe-flymake-init
             'haxe-flymake-cleanup
             'haxe-get-real-file-name)))))

(defun haxe-get-real-file-name (fake-name)
  "Helper function for flymake: flymake needs to know what's the real
name of the file was that we have requested the check"
  (message "haxe-get-real-file-name: %s -> %s" fake-name
           (haxe-path-from-flymake-path haxe-current-project fake-name))
  (haxe-path-from-flymake-path haxe-current-project fake-name))

(defun haxe-flymake-init ()
  "initialize flymake for HaXe."
  (unless haxe-completion-requested
    (haxe-log 3 "Flymake HaXe init")
    (flymake-get-syntax-check-program-args
     (buffer-file-name) (haxe-resolve-project-root)
     nil nil 'haxe-flymake-get-cmdline)))

(defun haxe-flymake-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a HaXe buffer.
This gets called by flymake itself. The output is a list of two elements:
the command to run, and a list of arguments.  The resulting command is like:

  $ haxe `(haxe-resolve-project-root)'/`haxe-build-hxml'

"
  (let ((file-name
         (haxe-strip-flymake-dir
          haxe-current-project
          (haxe-flymake-source haxe-current-project (current-buffer)))))
    (haxe-with-connection-project
        (con (compiler host port) haxe-current-project)
        (list compiler
              (append
               (list "--connect" (format "%s:%d" host port))
               (haxe-build-flymake-list
                (haxe-replace-all
                 (file-name-sans-extension file-name) "/" ".") t))))))

(defun haxe-flymake-cleanup ()
  "Called by flymake when it needs to cleanup after reporting"
  (flymake-simple-cleanup))

(defun haxe-flymake-create-temp-intemp (file-name prefix)
  "We need this to let flymake create a temp buffer 
so that it doesn't kill our files..."
  (make-temp-file
   (file-name-nondirectory
    (file-name-sans-extension file-name)) nil "tmp"))

;; ----------------------------------------------------------------------------
;; Ritchie Turner (blackdog@cloudshift.cl)
;; remake of https://github.com/cloudshift/hx-emacs/blob/master/hxc-complete.el

(defun haxe-build-flymake-list (source &optional flymake)
  "Builds the command run by flymake on the current buffer.
If FLYMAKE is not `nil', then this will produce a source path for flymake:
that is this path will put as -cp a flymake special directory rather then
the actual source."
  (let ((conditionals (haxe-conditional-comps))
        (result
         (append
          (haxe-build-cwd)
          (haxe-read-hxml flymake)
          (list source))))
    (if conditionals
        (append conditionals result)
      result)))

;; Filed bugreport for this: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=15184
(defadvice flymake-get-real-file-name-function
  (around haxe+flymake-get-real-file-name-function (file-name))
  (message "advised flymake-get-real-file-name-function %s -> %s"
           file-name (nth 2 (flymake-get-file-name-mode-and-masks file-name)))
  (setf ad-return-value
        (or (nth 2 (flymake-get-file-name-mode-and-masks file-name))
            'flymake-get-real-file-name)))

(defadvice flymake-process-sentinel
  (around haxe+flymake-process-sentinel (process event))
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

      (flymake-log 2 "process %d exited with code %d"
                   (process-id process) exit-status)
      (when (eql major-mode 'haxe-mode)
        ;; When there is a syntax error haxe will exit with non-zero code
        ;; but we don't care if that's the case.
        (setf exit-status 0))
      (condition-case err
          (progn
            (flymake-log 3 "cleaning up using %s" cleanup-f)
            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (funcall cleanup-f)))

            (delete-process process)
            (setq flymake-processes (delq process flymake-processes))

            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer

                (flymake-parse-residual)
                (flymake-post-syntax-check exit-status command)
                (setq flymake-is-running nil))))
        (error
         (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                source-buffer (error-message-string err))))
           (flymake-log 0 err-str)
           (with-current-buffer source-buffer
             (setq flymake-is-running nil))))))))

(defadvice flymake-after-change-function
  (around haxe+flymake-after-change-function (start stop len))
  "Overrides `flymake-after-change-function' to prevent it from running
when autocompletion is in progress"
  ;; haxe-received-status is some variable meant for deletion
  ;; While we only run flymake on save, it is save to do it, but
  ;; if we will try to run it upon newlines and such, we'll get
  ;; in trouble because as of now, we save the buffer before
  ;; running flymake, and this will invoke it recurisively causing
  ;; flymake-check-start-time to be nil and fail some arithmetics.
  ;; (unless (= 2 haxe-received-status) ad-do-it)
  ad-do-it)

(defun haxe-kill-network-process ()
  "Kill connection to HaXe compiler server and Flymake process in this buffer"
  (when (equal major-mode 'haxe-mode)
    (let ((fly-proc (get-buffer-process (buffer-name))))
      (haxe-stop-waiting-server-if-needed
       haxe-current-project (buffer-file-name))
      (when fly-proc
	(flymake-mode -1)
	(delete-process fly-proc)
	(haxe-log 3 "Flymake process killed")))))

(when (featurep 'speedbar)
  (make-local-variable 'speedbar-dynamic-tags-function-list)
  (speedbar-add-supported-extension ".hx")
  (setq speedbar-dynamic-tags-function-list
        '((haxe-fetch-dynamic-tags . haxe-insert-tag-list)))
  (message "added speedbar handlers"))

(defun haxe-insert-tag-list (level list)
  (message "need to insert: %s at %d" list level)
  ;; (speedbar-insert-imenu-list level list)
  (speedbar-insert-etags-list level list))

(defun haxe-fetch-dynamic-tags (file)
  "Calls `haxe-etags-program' with specially crafter arguments to obtain
tag information for FILE"
  (message "haxe-fetch-dynamic-tags called")
  (let ((buff-contents (buffer-string))
        (x 0) (y 0)
        newlist line word type)
    (save-excursion
      (switch-to-buffer (get-buffer-create " *haxe-tags-parser*"))
      (erase-buffer)
      (shell-command
       (concat haxe-etags-program " \\
--lang=none --regex='/[ \\t]*class[ \\t]+\\([^ \\t{\\/]+\\)/\\1/' \\
--regex='/[ \\t]*interface[ \\t]+\\([^ \\t{\\/]+\\)/\\1/' \\
--regex='/[ \\t]*typedef[ \\t]+\\([^ \\t{\\/=]+\\)/\\1/' \\
--regex='/[ \\t]*enum[ \\t]+\\([^ \\t{\\/]+\\)/\\1/' \\
--regex='/[ \\t]*\\(\\(public\\|private\\|static\\|override\\|inline\\)[ \\t]\\)+function[ \\t]\\([^ \\t(]+\\)/\\3/' \\
--regex='/[ \\t]*\\(\\(public\\|private\\|static\\|override\\|inline\\)[ \\t]\\)+var[ \\t]\\([^ \\t:=]+\\)/\\3/' \\
-o - " (expand-file-name file)) " *haxe-tags-parser*")
      (goto-char (point-min))
      (forward-line)
      (while (not (eobp))
        (forward-line)
        (beginning-of-line-text 1)
        (setq type nil)
        (while (not (eolp))
          (setq word (thing-at-point 'symbol))
          (cond
           ((null word))
           ((or (string= word "class")
                (string= word "function") 
                (string= word "var")
                (string= word "enum")
                (string= word "typedef"))
            (setq type (concat word " ")))
           ((or (string= word "private")
                (string= word "public")
                (string= word "override")
                (string= word "static")
                (string= word "inline")))
           (t (move-end-of-line nil)
              (backward-word)
              (setq newlist
                    (cons (cons
                           (concat type " " word)
                           (1+ (string-to-number (thing-at-point 'symbol)))) newlist))
              (move-end-of-line nil)))
          (forward-word)))
      (if (and (boundp 'speedbar-sort-tags)
               speedbar-sort-tags)
          (sort newlist (lambda (a b) (string< (car a) (car b))))
        (reverse newlist)))))

;; TODO: What was this meant for?
;; (defun haxe-calculate-offset-from-vector (y x string)
;;   (let ((moved 0) current)
;;     (while (and (not (zerop x)) (not (zerop y)))
;;       (if (not (zerop y))
;;           (when (position current "\r\n") (decf y))
;;         (return (+ moved x)))
;;       (incf moved))))

;; Commenting to pass compilation w/o warnings
;; (defun haxe-generate-import (for-type)
;;   (interactive (list (if for-type for-type (haxe-suggest-type))))
;;   (save-excursion ))

;; --------------- end my changes ---------------------------------------------

(defcustom haxe-mode-hook nil
  "*Hook called by `haxe-mode'."
  :type 'hook
  :group 'c)

(defun haxe-mode ()
  "Major mode for editing haXe code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `haxe-mode-hook'.

Key bindings:
\\{haxe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table haxe-mode-syntax-table)
  (setq major-mode 'haxe-mode
        mode-name "haXe"
        local-abbrev-table haxe-mode-abbrev-table
        abbrev-mode t)
  (use-local-map haxe-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars haxe-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'haxe-mode)
  ;; For some reason, comment-start-skip has to be set manually.
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  ;; --------------------------- my changes ---------------------------
  
  (c-set-offset 'substatement-open 0)
  ;; TODO: put these definitions into map declaration
  ;; (local-set-key "." haxe-completion-method)
  (local-set-key "(" 'haxe-hint-paren)
  (local-set-key (kbd "C-c h") 'haxe-electric-help)
  ;; (setq flymake-log-level 3)
  ;; Here we should start setting up the connection.
  (haxe-identify-project-root)
  ;; Should already know enough to start the connection.
  ;; Now, can initialize flymake
  ;; (this will also start HaXe waiting server)
  (haxe-flymake-install)
  (if haxe-current-project
      (haxe-with-connection-project
       (con (compiler) haxe-current-project)
       ;; haxe-build-hxml should be transformed into slot of the project
       (setq compile-command
             (concat compiler " "
                     (haxe-resolve-project-root) haxe-build-hxml)))
    ;; Perhaps this is just a single file, or for some other
    ;; reason we failed to locate the project associated with
    ;; this source
    (setq compile-command
          (concat haxe-compiler-default " "
                  (haxe-resolve-project-root) haxe-build-hxml)))
  ;; make tag search case-insensitive
  (setq tags-case-fold-search nil)
  (flymake-mode 1)
  
  (add-to-list 'ac-sources 'haxe-ac-dot-sources)
  (add-to-list 'ac-modes 'haxe-mode)
  (setq ac-auto-start 2)
  (auto-complete-mode 1)
  (ad-activate 'flymake-get-real-file-name-function)
  (ad-activate 'flymake-after-change-function)
  (ad-activate 'flymake-process-sentinel)
  (ad-activate 'c-forward-annotation)
  (ad-activate 'c-forward-decl-or-cast-1)
  (ad-activate 'c-beginning-of-macro)
  (add-hook 'kill-buffer-hook #'haxe-kill-network-process nil t)
  ;; (haxe-try-set-ecb-outlines)
  ;; ---------------------------- end my changes ----------------------
  (run-hooks 'c-mode-common-hook 'haxe-mode-hook)
  (c-update-modeline))

(provide 'haxe-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-mode.el ends here.
