;;; Commentary:

;; ------------------------------------------------------------------------
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

;; This program is an addition to haxe-mode.el to add auto-completion support
;; to HaXe projects. It requires auto-complete library to function.
;; (http://cx4a.org/software/auto-complete/)

;; haxe-completion is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(require 'cl)
(require 'haxe-log)
(require 'haxe-utils)
(require 'haxe-project)
(require 'haxe-help)
(require 'haxe-compiler-mode)
(require 'xml)
(require 'auto-complete)

;; TODO: obtain the values of $HAXE_HOME and $HAXE_LIBRARY_PATH
;; perhaps alert the user, if these are not set!

(defcustom haxe-completion-method #'haxe-complete-dot-ac
  "The function to use to perform after-dot completion. This can be either:
`haxe-complete-dot-ac' or `haxe-complete-dot-ido'. `haxe-complete-dot-ac' uses
auto-complete library to show the completion options, `haxe-complete-dot-ido' uses
ido to do the same."
  :type 'function :group 'haxe-mode)

;;;###autoload
(defvar haxe-ac-dot-sources
  '((init . haxe-ac-init)
    (requires . 0)
    (candidates . haxe-ac-candidates)
    (document . haxe-ac-documentation)
    (match . haxe-completion-filter)
    (prefix . haxe-ac-prefix-matcher)
    (symbol . "s"))
  "The source generator for autocompletion needed for interaction 
with auto-complete")

(defvar haxe-last-ac-candidates nil
  "The last time completion was called this variable was updated to
save the last result returnded from a request for completion sent to
HaXe compiler. This variable is set automatically, don't change it")

(defvar haxe-documentation-hash (make-hash-table :test #'equal)
  "We store here the documentation strings for the last completion requested")

(defvar haxe-last-ac-candidates-filtered nil
  "The list of completion candidates after the last filter was applied")

(defvar haxe-string-to-complete ""
  "This string is collected while requesting completion")

(defvar haxe-completion-pos -1
  "The position the completion started recording")

(defvar haxe-folding-delimiters '(?\ ?\t ?\n)
  "Characters used to delimit the words when padding a region")

(defvar haxe-folding-terminators '(?\- ?\. ?\, ?\? ?\! ?\; ?\: ?\) ?\] ?\")
  "Characters that terminate words when padding a region")

(defvar haxe-folding-exceptions
  '((?\. . ?\)) (?\. . ?\") (?\. . ?\') (?\. . ?\]) (?\. . ?\.)
    (?\! . ?\)) (?\! . ?\") (?\! . ?\') (?\! . ?\]) (?\! . ?\.)
    (?\! . ?\!) (?\! . ?\?) (?\? . ?\)) (?\? . ?\") (?\? . ?\')
    (?\? . ?\]) (?\? . ?\.) (?\? . ?\!) (?\? . ?\?))
  "Character pairs that should not be split, when word-wrapping a region,
unless there is only one word in the line")

(defvar haxe-last-completion-file nil
  "This variable is set when we try to ensure that the temporary file
required for the completion exists. If this variable is not NIL, while
creating of the new temporary file, the old one will be removed. This is
needed so we don't eventually feed the compiler the old sources from the
completion directory")

(defun haxe-package ()
  "Get the name of the package of the current file"
  ;; TODO This is a little too naive, we have to also check that the face of the
  ;; word package is also a proper face.
  (let ((bs (buffer-string)))
    (when (string-match "package\\s-\\(.*?\\);" bs)
      (match-string 1 bs))))

(defun haxe-conditional-comps ()
  "Reads conditional compilation settings from `haxe-build-hxml'"
  (let ((bs (buffer-string)))
       (when (string-match "hxc:\\s-\\(.*\\)" bs)
	 (match-string 1 bs))))

(defun haxe-build-cwd ()
  "Builds a part of command for HaXe compiler to change current directory to
the `project-root'."
  ;; It's not "/src", when we create the project, there's $source property
  ;; we then have to save that property somewhere and ONLY use "/src" if
  ;; we didn't have it.
  (list "--cwd"
	(concat (expand-file-name (haxe-resolve-project-root)))))

(defun haxe-read-hxml (&optional flymake)
  "Reads the contents of `haxe-project-build-command'
SEPARATOR is used to delimit lines read from the file"
  ;; TODO: this entire thing should go, be replaced by some method specializing
  ;; on `haxe-project'
  (with-temp-buffer
    (insert-file-contents
     (if (and haxe-build-hxml (haxe-resolve-project-root))
         (concat (file-name-as-directory (haxe-resolve-project-root)) haxe-build-hxml)
       (error "You need to specify `project-root' and `haxe-build-hxml'")))
    (delete-non-matching-lines "^-cp\\|^-lib\\|^-swf")
    (let (result pos)
      (dolist (i (delete-dups (split-string (buffer-string) haxe-eol)))
        (setq pos (position " " i))
        (if pos 
            (setq result (cons (substring i 0 pos) result)
                  result (cons (substring i pos) result))
          (setq result (cons i result))))
      (setq haxe-project-build-command (mapconcat #'identity result " "))
      (if flymake
          (append result
           (list (concat "-cp " (oref haxe-current-project directory)
                   (oref haxe-current-project flymake-dir))))
        result))))

(defun haxe-class-name (pkg)
  "Generates the fully qualified name of the HaXe class"
  (concat (when pkg (concat pkg "."))
          (file-name-nondirectory
           (file-name-sans-extension (buffer-name)))))

(defun haxe-ac-prefix-matcher ()
  "Loop back until we either find a dot to complete after, or
find nothing and return nil."
  (message "haxe-ac-prefix-matcher")
  (unless (member (haxe-face-at-point)
                  '(font-lock-string-face
                    font-lock-comment-face
                    font-lock-preprocessor-face))
    (++
      (with ((w " ") last))
      (for i from (point) downto (point-min))
      (setf (aref w 0) (char-before i))
      (unless (string-match "[[:alnum:]_]" w)
        (return
         ;; filter out floats
         (let ((result (and (string-match "\\." w)
                            (not (string-match "\\d" (make-string 1 last))))))
           (when result (haxe-ensure-completion-file))
           result)))
      (setf last (char-before i)))))

(defun haxe--relative-path (file)
  (let ((source (haxe--source-dir-of-file file)))
    (if source (substring file (length source))
      file)))

(defun haxe-ac-init ()
  "This function is called by `auto-complete' when it starts autocompleting"
  (message "haxe-ac-init")
  (let ((old-proc (get-process haxe-compiler-process)))
    ;; (when (or (not old-proc)
    ;;           (not (equal (process-status old-proc) 'open)))
    ;;   (setq haxe-network-process nil)
    ;;   (haxe-connect-to-compiler-server)
    ;;   (sleep-for 1)
    ;;   (setq old-proc (get-process haxe-compiler-process)))
    ;; (let ((ac-request
    ;;        (haxe-build-compile-string
    ;;         (haxe-package)
    ;;         (progn
    ;;           (haxe-ensure-completion-file)
    ;;           (haxe--relative-path (buffer-file-name))))))
    ;;   (setq haxe-last-ac-candidates nil
    ;;         haxe-last-ac-candidates-filtered nil
    ;;         haxe-last-compiler-response nil
    ;;         haxe-received-status 2)
    ;;   (clrhash haxe-documentation-hash)
    ;;   (process-send-string old-proc ac-request)
    ;;   (process-send-string old-proc "\000")
    ;;   (process-send-eof old-proc)
    ;;   (message "sent to process: <%s>" ac-request)
    ;;   (haxe-log 3 "haxe-ac-init sent request: %s\n completing: %s"
    ;;             ac-request
    ;;             (substring (buffer-string)
    ;;                        (max (point-min) (- (point) 10))
    ;;                        (point))))
    ;; (with-local-quit
    ;;   (with-timeout
    ;;       (5 (haxe-log 0 "Failing to fetch all completion options, giving up"))
    ;;     (while (not haxe-last-ac-candidates)
    ;;       (accept-process-output old-proc)
    ;;       (haxe-log 3 "statsus: %s"
    ;;                 (when haxe-last-compiler-response
    ;;                   (concat
    ;;                    (substring haxe-last-compiler-response
    ;;                               0 (min (length haxe-last-compiler-response) 42)) "...")))
    ;;       (when (and haxe-last-compiler-response (= haxe-received-status 2))
    ;;         (if (string= haxe-response-terminator "</list>\n")
    ;;             (haxe-parse-ac-response haxe-last-compiler-response)
    ;;           (haxe-parse-hint-response haxe-last-compiler-response))))))
    )
  haxe-last-ac-candidates)

(defun haxe-build-compile-string (pkg temp-file)
  "Builds `haxe-project-build-command'"
  (let ((conditionals (haxe-conditional-comps)))
    (concat haxe-eol
            (mapconcat #'identity (haxe-build-cwd) " ") haxe-eol
            (mapconcat #'identity conditionals " ")
            (if conditionals haxe-eol "")
            (mapconcat #'identity (haxe-read-hxml) haxe-eol) haxe-eol
            (concat "-main " (haxe-class-name pkg) haxe-eol)
            (concat "-cp " (haxe-resolve-project-root) ".completion") haxe-eol
            (concat "--display " temp-file "@"
                    (number-to-string (point-in-bytes))) haxe-eol)))

(defun point-in-bytes ()
  (let ((sub (substring (buffer-string) 0 (point))))
    (length (encode-coding-string sub 'utf-8))))

(defun haxe-ac-candidates ()
  "Requests autocompletion candidates and returns them"
  ;; (debug)
  ;; these do exist
  ;; (message "haxe-last-ac-candidates %s" haxe-last-ac-candidates)
  (message "haxe-last-ac-candidates-filtered %s" haxe-last-ac-candidates-filtered)
  haxe-last-ac-candidates-filtered)

(defun haxe-completion-filter (prefix candidates &rest rest)
  (message "haxe-completion-filter %s, rest: %s" prefix rest)
  (when (and (char-equal (char-before (- (point) (length prefix))) ?.)
             (not (char-equal (char-before (- (point) (1+ (length prefix)))) ?.))
             (not (member (haxe-face-at-point)
                          '(font-lock-string-face
                            font-lock-comment-face
                            font-lock-preprocessor-face))))
    (if (= 0 (length prefix))
        (setq haxe-last-ac-candidates-filtered haxe-last-ac-candidates)
      (setq haxe-last-ac-candidates-filtered
            (filter-candidates-exact haxe-last-ac-candidates))))
  ;; (let ((collected (substring (buffer-string) haxe-completion-pos (1- (point)))))
  ;;   (if (string= collected ".")
  ;;       (setq haxe-last-ac-candidates-filtered haxe-last-ac-candidates)
  ;;     (setq haxe-string-to-complete collected
  ;;           haxe-last-ac-candidates-filtered
  ;;           (filter-candidates-exact haxe-last-ac-candidates)
  ;;           ;; (remove-dupes
  ;;           ;;  (nconc (filter-candidates-exact haxe-last-ac-candidates)
  ;;           ;; 	  (filter-candidates-sturdy haxe-last-ac-candidates)
  ;;           ;; 	  (filter-candidates-fuzzy haxe-last-ac-candidates)))
  ;;           ))
  ;;   (message "haxe-completion-filter %s %s %s %s"
  ;;            prefix collected haxe-last-ac-candidates-filtered
  ;;            haxe-last-ac-candidates)
  ;;   haxe-last-ac-candidates-filtered)
  haxe-last-ac-candidates-filtered)

(defun fliter-candidates-levenstain (candidates)
  "Filters the candidates by establishing Levenstein distance from
the `haxe-string-to-complete' to the candidate"
  (let ((hash (make-hash-table))
        (max-distance 0)
        current result)
    (dolist (i candidates
               (dotimes (j max-distance result)
                 (setf current (gethash hash j))
                 (when current
                   (setf result (append result current)))))
      (setf current (haxe-levenstain haxe-string-to-complete i)
            max-distance (max max-distance current)
            (gethash hash current) (cons i (gethash hash current))))))

(defun filter-candidates-exact (candidates)
  "Filters CANDIDATES list by matching the exact beginning of every name
to `haxe-string-to-complete'"
  (let ((result
         (remove-if
          #'(lambda (x)
              (let ((mlen (length haxe-string-to-complete)))
                (or (< (length x) mlen)
                    (not (string= (substring x 0 mlen) haxe-string-to-complete)))))
          candidates)))
    (message "filter-candidates-exact result %s" result) result))

(defun filter-candidates-sturdy (candidates)
  "Filters CANDIDATES list by applying the following rule:

Characters in `haxe-string-to-complete' are taken to be the first
characters of the part constituting a word, such as for example:
\"do_me_a_favor\" or \"doMeAFavor\" corresponds to \"dmaf\" or \"DMAF\"
respectively."
  ;; TODO: consequent uppercase letters are treated incorrectly
  (remove-if
   #'(lambda (x)
       (let ((mlen (length haxe-string-to-complete))
             parts last)
         (dotimes (i (length x))
           (let ((current (aref x i)))
             (when (or (not last)
                       (and (char-equal last ?_) (not (char-equal current ?_)))
                       (and (not (char-equal (upcase last) last))
                            (char-equal (upcase current) current)))
               (setq parts (cons current parts)
                     last current))))
         (if (< mlen (length parts))
             (dotimes (i mlen)
               (setq last (aref haxe-string-to-complete i))
               (unless (char-equal last (aref parts i))
                 (return t))) nil))) candidates))

(defun filter-candidates-fuzzy (candidates)
  "Filters CANDIDATES list to see if all charactes of
`haxe-string-to-complete' are present in all list elements"
  (remove-if
   #'(lambda (x)
       (dotimes (i (length haxe-string-to-complete))
         (unless (position (aref haxe-string-to-complete i) x)
           (return t))))
   candidates))

(defun haxe-ac-documentation (symbol)
  "Requests documentation for SYMBOL from HaXe complier and returns it"
  (gethash symbol haxe-documentation-hash))

(defun haxe-complete-dot-ac ()
  "Calls HaXe compiler to get completion for properties.
This function is bound to \\[haxe-complete-dot-ac]"
  ;; There's one annoying thing about autocompletion, if you select it
  ;; from menu, it will delete the dot...
  (interactive)
  (insert ".")
  (let ((face (haxe-face-at-point)))
    (unless
        (or (not (char-equal (char-before (1- (point))) ?.))
            (not (member (haxe-face-at-point)
                         '(font-lock-string-face
                           font-lock-comment-face
                           font-lock-preprocessor-face))))
      (setq haxe-response-terminator "</list>\n"
            haxe-string-to-complete "."
            haxe-completion-pos (1- (point))
            haxe-last-ac-candidates nil
            haxe-last-ac-candidates-filtered nil
            haxe-completion-requested t)
      (save-buffer)
      ;; (when (fboundp 'auto-complete)
      ;;   (auto-complete (list haxe-ac-dot-sources)))
      )))

(defun hxc-parse-methods (raw)
  ;; TODO: dummy need to figure out what was this
  )

(defun hxc-modify-by-sig (sig selection)
  ;; TODO: what is this?
  )

(defun hxc-lookup-help (selection methodlist)
  ;; TODO: what is this?
  )

(defun hxc-msg (message)
  ;; TODO: what is this?
  )

(defun hxc-lookup-signature (selection methodlist)
  ;; TODO: what is this?
  )

(defun haxe-complete-dot-ido (raw)
  (let ((methodlist (hxc-parse-methods raw)))
    (when (length methodlist)
      (let ((selection
             (ido-completing-read
              "-> "
              (mapcar (lambda (el) (car el)) methodlist))))
        (forward-char)
        (when selection
          (let ((sig (hxc-lookup-signature selection methodlist))
                (help (hxc-lookup-help selection methodlist)))
            (message  sig)
            (hxc-msg (concat sig "\n\n" help ))
            (insert (hxc-modify-by-sig sig selection))))))))

(defun haxe-hint-paren ()
  "Calls HaXe compiler to get hint for function arguments.
This function is bound to \\[haxe-hint-paren]"
  (interactive)
  (insert "(")
  (let ((face (haxe-face-at-point))
        found)
    (unless (equal face 'font-lock-string-face)
      (save-excursion
        (while (and (not (eobp))
                    (position (char-before) " \t\r\n"))
          (backward-char))
        (unless (eobp)
          (backward-char)
          (when (equal (haxe-face-at-point) 'default)
            (setq found t))))
      (when found
        (setq haxe-response-terminator "</type>\n"
              haxe-completion-requested t)
        (haxe-ac-init)))))

(defun haxe-parse-ac-response (xml)
  "Parses the completion options supplied by HaXe compiler.
XML has to contain child nodes named \"i\", their \"n\" attribute
is taken to be the name of the field to complete and their child node
\"d\" is taken to be the documentation of the field."
  (condition-case var
      (with-temp-buffer
        (let* ((root (progn (insert xml)
                            (xml-parse-region (point-min) (point-max))))
               (options (car root))
               (is (xml-get-children options 'i)))
          (i++ (for i in is)
               (collect (cdar (xml-node-attributes i)) into completions)
               (collect (haxe-fold-string
                         (haxe-condence-white-string
                          (haxe-replace-all
                           (haxe-trim-string
                            (car (last (car (xml-get-children i 'd)))))
                           "\t" " ")) 42 3 2)
                        into docs)
               (puthash (car completions)
                        (concat (car completions) "\n" (car docs))
                        haxe-documentation-hash)
               (finally (setf haxe-last-ac-candidates completions)))))
    (error (haxe-log 0 "Error when parsing completion options %s, %s" var xml))))

(defun haxe-exception-p (first-char second-char exceptions)
  "Werifies whether the EXCEPTIONS contains a pair (FIRST-CHAR SECOND-CHAR)"
  (i++ (for (a b) in exceptions)
       (when (and (char-equal a first-char)
                  (char-equal b second-char))
         (return t))))

(defun haxe-read-word (input position delimiters ends exceptions)
  "Reads the first word from the INPUT, starting from position. The word
is a substring that may be terminated by any of the ENDS characters, or
before any of DELIMITERS characters. However, if the last character of
the word is the `car' of any of EXCEPTION pairs and the character that follows
that character is that same pairs `cdr', then the word is not terminated
and the process is repeated until the next DELIMITER or END is encountered."
  (let (word char)
    (catch 't
      (while (< position (length input))
        (setq char (aref input position))
        (cond
         ((member char delimiters)
          (if (and (< position (1- (length input)))
                   (haxe-exception-p char (aref input (1+ position)) exceptions))
              (setq word (cons char word))
            (throw 't t)))
         ((member char ends)
          (setq word (cons char word))
          (unless (and (< position (1- (length input)))
                       (haxe-exception-p char (aref input (1+ position)) exceptions))
            (throw 't t)))
         (t (setq word (cons char word))))
        (incf position)))
    (coerce (reverse word) 'string)))

(defun haxe-fold-string-words
  (input max-length &optional pad-left pad-right delimiters ends exceptions)
  "Creates a block of text which has no more than MAX-LENGTH characters
in one line, is padded by PAD-LEFT characters on the left and PAD-RIGHT
characters on the right. The text will break words only when the word is
longer then MAX-LENGTH.
DELIMITERS are the characters which cannot be part of the word.
ENDS are the characters that end a word.
EXCEPTIONS are the pairs of characters (an assoc list) that should never be
split, unless it is the only word on the line."
  (let* ((delimiters (or delimiters haxe-folding-delimiters))
         (ends (or ends haxe-folding-terminators))
         (exceptions
          (or exceptions haxe-folding-exceptions))
         (tab-delimiter-p (member ?\t delimiters))
         (pad-left (or pad-left 0))
         (pad-right (or pad-right 0))
         (pos 0) (line-built 0) word line-has-words next-char)
    (when (> (+ pad-right pad-left) max-length)
      (error "The sum of paddings must be smaller then the line length."))
    (with-output-to-string
      (while (< pos (length input))
        (dotimes (i pad-left)
          (princ " ")
          (incf line-built))
        (setq word (haxe-read-word input pos delimiters ends exceptions))
        (if (<= (+ line-built (length word)) (- max-length pad-right))
            (progn
              (princ word)
              (setq line-has-words t)
              (incf line-built (length word))
              (incf pos (length word))
              (when (and (< pos (1- (length input)))
                         (member (aref input pos) delimiters))
                (setq next-char (aref input pos))
                (cond
                 ((char-equal ?\t next-char)
                  (incf pos tab-width)
                  (incf line-built tab-width))
                 ((char-equal ?\n next-char)
                  (incf pos)
                  (dotimes (i (- max-length line-built))
                    (princ " "))
                  (setq line-built 0 line-has-words nil)
                  (terpri))
                 (t (incf pos)
                    (incf line-built)))
                (princ (char-to-string next-char))))
          (progn
            (if line-has-words
                (dotimes (i (- max-length line-built))
                    (princ " "))
              (progn
                (princ (subseq word 0 (- max-length pad-right line-built)))
                (incf pos (- max-length pad-right line-built))
                (dotimes (i pad-right)
                  (princ " "))))
            (terpri)
            (setq line-built 0 line-has-words nil)))))))

(defun haxe-pad-region (start end width &optional prefix pad-left pad-right)
  "Creates a column from the seclected region between START and END of the
width WIDTH.
PREFIX argument is populated when this function is called interactively.
With default prefix argument, the column will be padded by 1 character on the
right and on the left. If you provide numberical argument other than default,
then you will be prompted to provide the padding for left and right sides.
Non-interactive callers must not provide PREFIX argument if they wish to
specify paddings other then 0.

See also `haxe-folding-delimiters', `haxe-folding-terminators',
`haxe-folding-exceptions' and `haxe-fold-string-words'"
  (interactive "r\nnHow wide should the created columnbe? \nP")
  (if prefix
      (if (equal prefix 4)
          (setq pad-left (read-string "Columns to pad on the left: " prefix)
                pad-right (read-string "Columns to pad on the right: " prefix))
        (setq pad-left prefix pad-right prefix))
    (setq pad-left (or pad-left 0) pad-right (or pad-right 0)))
  (let ((input (haxe-fold-string-words
                (buffer-substring start end) width pad-left pad-right)))
    (kill-region start end)
    (insert input)))

(defun haxe-parse-hint-response (xml)
  "Parses the function hint supplied by HaXe compiler."
  (condition-case var
      (let* ((root (with-temp-buffer
                     (insert xml)
                     (xml-parse-region (point-min) (point-max))))
             (options (car root))
             (signature
              (replace-regexp-in-string
               "&lt;" "<"
               (replace-regexp-in-string
                "&gt;" ">"
                (xml-node-children options)))))
        (message "haxe-parse-hint-response %s" signature))
    (setq haxe-completion-requested nil)
    (error (haxe-log 0 "Error when parsing completion options %s, %s" var xml))))

;; FIXME: this seems useless as this is what make-directory does...
(defun haxe-ensure-directories (path &optional root)
  (unless (listp path) (setq path (split-string path "/" t)))
  (unless root (setq root "/"))
  (unless (null path)
    (let ((dst (concat root (car path))))
      (unless (file-exists-p dst)
        (make-directory dst))
      (when (cdr path)
        (setf (cadr path) (concat (car path) "/" (cadr path))))
      (haxe-ensure-directories (cdr path) root))))

(defun haxe--source-dir-of-file (file)
  (let ((file (expand-file-name file))
        (sources haxe-project-sources)
        prefix result)
    (while (and (not result) sources)
      (let ((s (car sources)))
        (when (>= (length file) (length s))
          (setq prefix (substring file 0 (length s)))
          (when (string= prefix s)
            (setq result s)))))
    result))

(defun haxe-ensure-completion-file ()
  "Creates all necessary directories and the file needed for autocompletion and
returns the absolute file name."
  (let* ((current (expand-file-name (buffer-file-name)))
         (content (buffer-string))
         (source-dir (haxe--source-dir-of-file current))
         temp)
    (unless haxe-project-root
      (haxe-resolve-project-root))
    (message "ensured project root")
    (if (and haxe-project-root source-dir)
        (progn
          (when haxe-last-completion-file
            (when (file-exists-p haxe-last-completion-file)
              (delete-file haxe-last-completion-file))
            (setq haxe-last-completion-file nil))
          (setq temp
                (concat
                 haxe-project-root
                 ".completion/"
                 (substring current (length source-dir)
                            (- (length (file-name-nondirectory current))))))
          (message "will save temp file in %s" temp)
          (unless (file-exists-p temp)
            (haxe-ensure-directories temp))
          (setq haxe-last-completion-file
                (concat temp (file-name-nondirectory current)))
          (with-temp-file haxe-last-completion-file
            (insert content)) haxe-last-completion-file)
      (error "Didn't know where to create temporary completion file.
Project root: <%s>,
File to complete: <%s>
Soure directory: <%s>" haxe-project-root current source-dir))))

(provide 'haxe-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-completion.el ends here.
