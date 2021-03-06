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

;; haxe-compiler-mode is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(require 'cl)
(require 'eieio)
(require 'haxe-log)
(require 'haxe-utils)

(deflocal haxe-compiler-in-start 0
  "Position of the beginning of the last message sent to haxe compiler")

(deflocal haxe-compiler-in-end 0
  "Position of the end of the last message sent to haxe compiler")

(deflocal haxe-compiler-out-start 0
  "Position of the beginning of the last message received from haxe compiler")

(deflocal haxe-compiler-out-end 0
  "Position of the end of the last message received from haxe compiler")

(deflocal haxe-network-idle-timer nil
  "The idle timer that watches after the connection to the compilation
server and tries to bring it up, if it was broken")

(deflocal haxe-message-state 'composing
  "The state of the communication buffer, possible values are:
- composing (default), this state allows regular input to take place.
- in, this is the state of the text that has been sent to the server.
- out, this is the state of the text received from the server.")

(defvar haxe-compiler-default "haxe"
  "The default haxe compiler to use")

(defvar haxe-host-default "127.0.0.1"
  "The default host to connect to for haxe compiler")

(defvar haxe-port-default 1257
  "The default port on which to connect to haxe compiler")

(defvar haxe-max-responses 100
  "The number of responses from the compiler server to keep in cache")

(defconst haxe-network-connection "haxe-connection"
  "The name given to the process conneting to Haxe waiting server")

(defconst haxe-times-to-reconnect 10
  "How many times should the idle timer try to reconnect before considering
the connection to be dead")

(defvar haxe-compiler-mode-hook nil
  "Hooks run by thaxe-compiler-mode when it starts")

(defun haxe-insert-conditionally ()
  (interactive)
  (when (eql (getlocal haxe-message-state) 'composing)
    (call-interactively #'self-insert-command)))

(defvar haxe-compiler-mode-map nil
  "Keymap for haxe-compiler major mode")
(unless haxe-compiler-mode-map
  (setq haxe-compiler-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap self-insert-command]
            #'haxe-insert-conditionally)
          (define-key map "\C-c\C-c" #'haxe-send-to-server)
          (define-key map "\C-c\C-o" #'haxe-compier-clean)
          map)))

(defvar haxe-connections nil
  "The list of all currently active compilation servers
 (the list of `haxe-connection')")

(defvar haxe-network-process-buffer "*haxe-network-process-buffer*"
  "The buffer to hold the network process connecting to HaXe compiler server.
This is needed because otherwise the process get's lost somehow D:")

(defvar haxe-eol "\n"
  "The string used as line separator when building commands to HaXe compiler.")

(defvar haxe-ps (make-string 1 1)
  "The character used to delimit requests and responses sent / received from
HaXe server.")

(defvar haxe-response-terminator "</list>\n"
  "This variable is set according to the kind of completion we request
it may be \"</list>\n\" or \"</type>\n\" (first is for dot completion
the second is for paren hint")

;; TODO: this should return to haxe-completion.el once the `haxe-listen-filter'
;; starts making more sense.
(defvar haxe-completion-requested nil
  "This variable is set to T when dot autocompletion starts")

(defface haxe-face-in
  `((t (:foreground "pink")))
  "Face for highlighting the text sent to haxe compiler server."
  :group 'haxe-mode)

(defface haxe-face-out
  `((t (:foreground "chartreuse")))
  "Face for highlighting the text received from haxe compiler server."
  :group 'haxe-mode)

(defface haxe-face-error
  `((t (:foreground "red")))
  "Face for highlighting the text received from haxe compiler server."
  :group 'haxe-mode)

(defface haxe-face-pending
  `((t (:foreground "white")))
  "Face for highlighting the text pending to be sent haxe compiler server."
  :group 'haxe-mode)

(defclass haxe-connection ()
  ((host :initarg :host
         :initform "127.0.0.1"
         :type string
         :documentation "Host this connection is working with.")
   (port :initarg :port
         :initform 1257
         :type integer
         :documentation "Port this connection is working with.")
   (compiler :initarg :compiler
             :initform "haxe"
             :type string
             :documentation "Path to the compiler executable.")
   (process :initarg :process
            :type (or null process)
            :documentation "The process of this connection.")
   (connection :initarg :connection
               :initform nil
               :type (or null process)
               :documentation
               "The connection created to this instance of server.")
   (request :initform nil
            :type (or null string)
            :documentation "The message sent to server.")
   (expected-prefix :initform nil
                    :type (or nil string)
                    :documentation "The prefix we expect from compiler server.")
   (expected-suffix :initform nil
                    :type (or nil string)
                    :documentation "The suffix we expect from compiler server.")
   (response :initform nil
            :type (or null string)
            :documentation "The response received from server.")
   (response-handlers
    :initform nil
    :type list
    :documentation "Callbacks to invoke once the message from server is received,
Callbacks will receive one argument, this argument being the message received.")
   (project :intiarg :project
            :initform nil
            :type (or symbol haxe-ede-project)
            :documentation "This is the project associated with a group of
HaXe source files all of which will use this connection when requesting
flymake or compilation. However, projects may define multiple connection,
so this is not a one to one correspondence.")
   (interactive-buffer
    :initarg :interactive-buffer
    :initform nil
    :type buffer
    :documentation "The interactive buffer for communicating with Haxe compiler")
   (buffers :initarg :buffers
            :initform nil
            :type list
            :documentation "All buffers who share this connection.")
   (filter :initarg :filter
           :initform nil
           :type symbol
           :documentation "The filter function for this connection
 (used in `make-network-process').")
   (sentinel :initarg :sentinel
             :initform nil
             :type symbol
             :documentation "The sentinel function for this connection
 (used in `make-network-process')."))
  :allow-nil-initform t
  :documentation
  "This class describes the shared connection to the HaXe compiler server")

(defmethod haxe-handle-server-response ((this haxe-connection))
  (loop for handler in (oref this response-handlers) do
        (funcall handler (oref this response)))
  (oset this response ""))

(defmethod haxe-add-connection-handler ((this haxe-connection) handler)
  (oset this response-handlers (cons handler (oref this response-handlers))))

(defmethod haxe-remove-connection-handler ((this haxe-connection) handler)
  (oset this response-handlers (remove handler (oref this response-handlers))))

(defmacro haxe-with-connection (spec &rest body)
  "Similar to `with-slots', but will search for the connection object in the
buffer of the connection SPEC, the format of the speck is:
 (CONNECTION (SLOT-SPECS) &optional BUFFER)
CONNECTION is the `haxe-connection' object which operates on this buffer
SLOT-SPECS are the slot specification exactly as they appear in `with-slots'
Optional BUFFER can be a buffer name, a buffer object or nil. In the later
case, the current buffer is used."
  (declare (indent 3))
  (let ((con (car spec))
        (slots (cadr spec))
        (buffer (or (caddr spec) '(current-buffer))))
    `(with-current-buffer ,buffer
       (let ((,con (haxe--connection-for-buffer ,buffer)))
         (with-slots ,slots ,con
           ,@body)))))

(defmacro haxe-with-connection-process (spec &rest body)
  "Similar to `haxe-with-connection' except for that the process is
used to identify the connection."
  (declare (indent 3))
  (let ((con (car spec))
        (slots (cadr spec))
        (proc (caddr spec)))
       `(let ((,con (haxe--connection-for-process ,proc)))
          (with-current-buffer (oref ,con interactive-buffer) 
            (with-slots ,slots ,con
              ,@body)))))

(defmacro haxe-with-connection-project (spec &rest body)
  "Similar to `haxe-with-connection' except for that the project is
used to identify the connection."
  (declare (indent 3))
  (let ((con (car spec))
        (slots (cadr spec))
        (proj (caddr spec)))
    `(let ((,con (haxe--connection-for-project ,proj)))
       (with-current-buffer (oref ,con interactive-buffer) 
         (with-slots ,slots ,con
           ,@body)))))

(defun haxe-listen-filter (proc input)
  "Is called by the running HaXe server to report events, if any."
  ;; We are only interested in recording the completion XMLs
  (message "filter <%s>" input)
  (haxe-with-connection-process
      (con (expected-prefix expected-suffix request response) proc)
      (message "current-buffer: %s" (current-buffer))
      (setf response (concat response input))))

(defun haxe-network-process-sentinel (process input)
  "This function is used for monitoring the TCP connection established
between HaXe compilation server and some buffer."
  (message "haxe-network-process-sentinel <%s>" input)
  (when (stringp input)
    (haxe-with-connection-process
        (con (response interactive-buffer) process)
        (cond
         ((haxe-string-starts-with input "connection broken by")
          ;; Compiler sent the entire message, or broke.
          ;; Let's assume it's the entire message - worst case
          ;; the user will restart the waiting server.
          (haxe-append-server-response response :response)
          (haxe-handle-server-response con))
         ((haxe-string-starts-with input "failed with code")
          (haxe-append-server-response input :error)
          (error "There was a communication error: \"%s\",
switch to %s buffer to see more details" input interactive-buffer))
         (t                             ; Keep collecting incoming
                                        ; messages
          (haxe-append-server-response input :technical))))))

(defun haxe--connection-for-buffer (buffer)
  "A helper function for `haxe-with-connection' macro, locates
the `haxe-connection' object."
  (unless (bufferp buffer) (setq buffer (get-buffer buffer)))
  (loop for connection in haxe-connections
        for found = 
        (loop for buf in (oref connection buffers)
              do (when (eql (if (bufferp buffer) buffer
                              (get-buffer buffer)) buf)
                   (return connection)))
        do (when found (return found))))

(defun haxe--connection-for-process (process)
  "A helper function for `haxe-with-connection-process' macro, locates
the `haxe-connection' object."
  (loop for con in haxe-connections
        for found = (when (eql (oref con connection) process) con)
        do (when found (return found))))

(defun haxe--connection-for-project (project)
  "A helper function for `haxe-with-connection-project' macro, locates
the `haxe-connection' object."
  (loop for con in haxe-connections
        for found = (when (eql (oref con project) project) con)
        do (when found (return found))))

(defun haxe-do-send-to-server (message)
  (haxe-with-connection
      (con (connection request filter sentinel host port))
      (setq request (concat message "\000")
            connection
            (make-network-process
                 :name haxe-network-connection
                 :family 'ipv4
                 :host host
                 ;; :nowait t
                 :service port
                 ;; :buffer haxe-network-process-buffer
                 :filter filter
                 :sentinel sentinel))
      (process-send-string connection request)))

(defun haxe-send-to-server ()
  "Sends the substring `haxe-compiler-in-start' to
`haxe-compiler-in-end' from the current buffer to the compiler server"
  (interactive)
  ;; Could use our formatting library here
  (let* ((end (point-max))
         (start (- end (haxe-face-start 'haxe-face-pending)))
         (message
          (concat
           (mapconcat #'identity
                      (haxe-split-string
                       (buffer-substring-no-properties
                        start end) ?\n t)
                      haxe-eol)
           (format "%s\000" haxe-eol))))
    (let ((inhibit-read-only t))
      (kill-region start end))
    (goto-char (point-max))
    (haxe-do-send-to-server message)
    (haxe-compiler-insert :request message)
    (goto-char (point-max))
    (setf (local haxe-compiler-in-end) (point-max)
          (local haxe-compiler-in-start) (point-max))))

(defun haxe--sanitize-response (response)
  "A helper function for removing non-printable characters
from RESPONSE."
  (with-output-to-string 
    (loop for i across response
          do (when (>= i ?\ ) (write-char i)))))

(defun haxe-append-server-response (response &optional how)
  "Appends HaXe server response to the current buffer."
  (goto-char (point-max))
  (haxe-compiler-insert how (haxe--sanitize-response response))
  (put-text-property
   (- (point-max) 2) (1- (point-max)) 'invisible t)
  (goto-char (point-max))
  (setf (local haxe-compiler-in-start) (point)
        (local haxe-compiler-in-end) (point)))

(defun haxe-compier-clean ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setf (local haxe-compiler-in-start) (point-min)
          (local haxe-compiler-out-start) (point-min)
          (local haxe-compiler-in-end) (point-min)
          (local haxe-compiler-out-end) (point-min))))

(defun haxe--make-buffer-and-process (compiler host port)
  (message "haxe--make-buffer-and-process: %s, %s, %s" compiler host port)
  (let* ((new-buffer
          (get-buffer-create
           (generate-new-buffer-name
            " *haxe-waiting-server*")))
         (proc (start-process compiler new-buffer compiler
                              "--wait" 
                              (format "%s:%d" host port))))
    (bury-buffer new-buffer) new-buffer))

(defun haxe--create-default-connection ()
  (let ((buff
         (switch-to-buffer
          (get-buffer-create
           (generate-new-buffer-name
            "*haxe-interactive-server*")))))
    (haxe-compiler-mode)
    (push (make-instance 'haxe-connection
                         :compiler haxe-compiler-default
                         :host haxe-host-default
                         :port haxe-port-default
                         :filter #'haxe-listen-filter
                         :sentinel #'haxe-network-process-sentinel
                         :buffers (list buff)
                         :interactive-buffer buff)
          haxe-connections)
    (car haxe-connections)))

;;;###autoload
(defun haxe-start-waiting-server (&optional restart compiler host port)
  "Starts HaXe `haxe-compiler' on `haxe-server-host':`haxe-server-port'
with \"--wait\" for the future requests made by autocompletion
or flymake.
RESTART is only used when called non-interactively, this instructs to
reuse the old buffer and its connection rather then create a new one.
This function is bound to \\[haxe-start-waiting-server]"
  (interactive
   (let ((compiler-i
          (read-string "HaXe compiler: "
                       haxe-compiler-default t
                       haxe-compiler-default))
         (host-i
          (read-string "HaXe server host: "
                       haxe-host-default t haxe-host-default))
         (port-i
          (read-number "HaXe server port: " haxe-port-default)))
     (list nil compiler-i host-i port-i)))
  (if (called-interactively-p 'interactive)
      ;; TODO: must remember to kill process before restarting.
      (let ((new-buffer (haxe--make-buffer-and-process compiler host port)))
        (if (not restart)
            (haxe--create-default-connection)
          (let ((con (haxe--connection-for-buffer (current-buffer))))
            (with-slots (compiler host port) con
              (haxe-log 0 "restarted server and reset the process")
              (setq compiler compiler
                    host host
                    port port)))))
    (let ((con (haxe--connection-for-buffer (current-buffer))))
      ;; If connection is nil, then we must've been started in
      ;; a "wrong" buffer, i.e. non-interactively, perhaps from a
      ;; *.hx file opened for editing. We then need to create a new
      ;; buffer and associate a connection with it.
      (message "called non-interactively: %s, %s, %s"
               haxe-compiler-default
               haxe-host-default haxe-port-default)
      (if con
          (progn
            (unless compiler
              (setq compiler
                    (or (oref con compiler) haxe-compiler-default)))
            (unless host
              (setq host (or (oref con host) haxe-host-default)))
            (unless port
              (setq port (or (oref con port) haxe-port-default))) con)
        (let ((new-buffer
               (haxe--make-buffer-and-process
                haxe-compiler-default
                haxe-host-default haxe-port-default)))
          (haxe--create-default-connection))))))

(defun haxe-server-cleanup-hook ()
  "The hook that runs after the buffer with the process is closed
it should look for orphan processes and destroy them as well as
remove orhpan connection objects."
  ;; TODO: need a separate function for forcing the cleanup if
  ;; this hook fails
  (ignore-errors                        ; Better fail, then
                                        ; keep the buffer live
    (haxe-with-connection
        (con (buffers process connection))
        (setq buffers
              (remove-if (lambda (x) (eql x (current-buffer)))
                         buffers))
        (unless buffers
          (delete-process process)
          (when connection
            (delete-process connection))
          (setq haxe-connections
                (remove-if (lambda (x) (eql x con))
                           haxe-connections))))))

(defun haxe-compiler-insert (how &rest strings)
  "Works like a regular insert, except it will also take care of
read-only attribute and applying the proper face."
  (let* (start
         end
         (face 
          (cond
           ((eql how :response)
            (setf start "<response>" end "</response>")
            'haxe-face-pending)
           ((eql how :request)
            (setf start "<request>" end "</request>")
            'haxe-face-pending)
           ((eql how :technical)
            (setf start "<technical>" end "</technical>")
            'haxe-face-in)
           ((eql how :error)
            (setf start "<error>" end "</error>")
            'haxe-face-error)))
         (input (reduce #'concat strings))
         (inhibit-read-only t))
    (insert start input end "\n")))

(define-derived-mode haxe-compiler-mode fundamental-mode
  "Haxe Compiler interaction mode"
  "Major mode interacting with HaXe compiler server.
This mode uses its own keymap:
\\{haxe-compiler-mode-map}"
  (kill-all-local-variables)
  (erase-buffer)
  (setq major-mode 'haxe-compiler-mode)
  (use-local-map haxe-compiler-mode-map)
  (setf mode-name "HaXe Interactive Compiler")
  (haxe-local-init*
      ((haxe-compiler-in-start (point-min))
       (haxe-compiler-in-end (point-min))
       (haxe-compiler-out-start (point-min))
       (haxe-compiler-out-end (point-min))
       (haxe-network-idle-timer nil)
       (haxe-message-state 'composing)))
  (run-hooks 'haxe-compiler-mode-hook)
  (add-hook 'kill-buffer-hook #'haxe-server-cleanup-hook nil t)
  (insert haxe-ps)
  (put-text-property
   (point-min) (point-max) 'invisible t)
  (put-text-property
   (point-min) (point-max) 'face
   'haxe-face-pending)
  ;; (add-hook 'before-change-functions
  ;;           #'haxe-before-change-hook nil 'local)
  ;; (add-hook 'after-change-functions
  ;;           #'haxe-after-change-hook nil 'local)
  )

(provide 'haxe-compiler-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-compiler-mode.el ends here.
