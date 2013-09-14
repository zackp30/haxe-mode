(defvar haxe-date-re
  (cl-format "%{[[:digit:]]\\{%d\\}%^-%} %{[[:digit:]]\\{%d\\}%^:%}" '(4 2 2) '(2 2 2))
  "Regex used to match serialized dates")

(defvar haxe-float-re
  "-?\\(:?[[:digit:]]*\\.[[:digit:]]+\\(e[+-]?[[:digit:]]\\)?\\)\\|[[:digit:]]+"
  "Regex used to match serialized floats")

(defvar haxe-int-re "-?[[:digit:]]+"
  "Regex used to match serialized integers")

(cl-defstruct haxe-serializer-cache table sindex oindex)

(defun haxe-make-serializer-cache ()
  (make-haxe-serializer-cache
   :table (make-hash-table :test #'equal) :sindex 0 :oindex 0))

(defun haxe-serializer-put-cache (object cache kind)
  (let (index)
    (cl-case kind
      (?R (setf index (haxe-serializer-cache-sindex cache)
                (haxe-serializer-cache-sindex cache) (1+ index)))
      (?r (setf index (haxe-serializer-cache-oindex cache)
                (haxe-serializer-cache-oindex cache) (1+ index))))
    (puthash (format "%c%d" kind index)
             object (haxe-serializer-cache-table cache))))

(defun haxe-serializer-read (string &optional cache)
  (unless cache (setf cache (haxe-make-serializer-cache)))
  (cl-case (aref string 0)
    ((?n ?f) (list nil 1))
    (?z (list 0 1))
    (?i (haxe-serializer-read-int string cache))
    (?t (list t 1))
    (?k (list 0.0e+NAN 1))
    (?m (list 1.0e-INF 1))
    (?p (list 1.0e+INF 1))
    (?d (haxe-serializer-read-float string cache))
    (?y (haxe-serializer-read-string string cache))
    (?o (haxe-serializer-read-struct string cache))
    (?l (haxe-serializer-read-list string cache))
    (?a (haxe-serializer-read-array string cache))
    (?v (haxe-serializer-read-date string cache))
    (?b (haxe-serializer-read-string-hash string cache))
    (?q (haxe-serializer-read-int-hash string cache))
    (?s (haxe-serializer-read-bytes string cache))
    (?x (haxe-serializer-read-exception string cache))
    (?c (haxe-serializer-read-class string cache))
    (?C (haxe-serializer-read-custom string cache))
    (?M (haxe-serializer-read-map string cache))
    (?w (haxe-serializer-read-enum string cache))
    (?r (haxe-serializer-read-object-cache string cache))
    (?R (haxe-serializer-read-string-cache string cache))
    (otherwise (error "invalid head: %c" (aref string 0)))))

(defun haxe-serializer--read-num (string reader regex &rest reader-args)
  (string-match regex string 1)
  (let ((digits (substring string (match-beginning 0) (match-end 0))))
    (list (apply reader digits reader-args) (1+ (length digits)))))

(defun haxe-serializer-read-int (string cache)
  (haxe-serializer--read-num string 'parse-integer haxe-int-re))

(defun haxe-serializer-read-float (string cache)
  (haxe-serializer--read-num string 'string-to-number haxe-float-re 10))

(defun haxe-serializer-read-date (string cache)
  (haxe-serializer--read-num string 'date-to-time haxe-date-re))

(defun haxe-serializer-read-string (string cache)
  (cl-destructuring-bind (len chars)
      (haxe-serializer-read-int string cache)
    (let ((result (url-unhex-string (substring string (1+ chars) (+ 1 chars len)) t)))
      (haxe-serializer-put-cache result cache ?R)
      (list result (+ 1 chars len)))))

;; Somewhat misleading name, but it isn't really a struct in Haxe, it's more
;; like a pairlist really
(defun haxe-serializer-read-struct (string cache &optional terminator)
  (cl-loop with result = nil and extension = 1
           and cell = nil and string = (substring string 1)
           and terminator = (or terminator ?g)
           until (char-equal (aref string 0) terminator)
           for key = (haxe-serializer-read string cache)
           do (cl-destructuring-bind (key-result key-chars) key
                (cl-incf extension key-chars)
                (setf cell key-result string (substring string key-chars)))
           for value = (haxe-serializer-read string cache)
           do (cl-destructuring-bind (value-result value-chars) value
                (cl-incf extension value-chars)
                (setf string (substring string value-chars))
                (push (cons cell value-result) result))
           finally
           (let ((result (nreverse result)))
             (haxe-serializer-put-cache result cache ?r)
             (cl-return (list result (1+ extension))))))

(defun haxe-serializer-read-list (string cache)
  (cl-loop with result = nil and string = (substring string 1)
           and extension = 0
           until (char-equal (aref string 0) ?h)
           do (cl-destructuring-bind (element len)
                  (haxe-serializer-read string cache)
                (push element result)
                (setf extension (+ extension len)
                      string (substring string len)))
           finally
           (let ((result (nreverse result)))
             (haxe-serializer-put-cache result cache ?r)
             (cl-return (list result (+ 2 extension))))))
        
(defun haxe-serializer-read-array (string cache)
  (cl-destructuring-bind (list len) (haxe-serializer-read-list string cache)
    (list (cl-coerce list 'array) len)))

(defun haxe-serializer-read-string-hash (string cache)
  (cl-destructuring-bind (list len) (haxe-serializer-read-struct string cache ?h)
    (let ((result (make-hash-table :test #'equal)))
      (mapc (lambda (x) (puthash (car x) (cdr x) result)) list)
      (list result len))))

(defun haxe-serializer-read-int-hash (string cache)
  (cl-destructuring-bind (list len) (haxe-serializer-read-struct string cache ?h)
    (let ((result (make-hash-table)))
      (mapc (lambda (x) (puthash (car x) (cdr x) result)) list)
      (list result len))))

(defun haxe-serializer-read-bytes (string cache)
  (cl-destructuring-bind (string len) (haxe-serializer-read-string string cache)
    (list (base64-decode-string
           (concat string (make-string (- 4 (mod (length string) 4)) ?=))) len)))

(defun haxe-serializer-read-exception (string cache)
  (error "not implemented"))

(defun haxe-serializer-read-class (string cache)
  ;; also does custom deserialization
  (error "not implemented"))

(defun haxe-serializer-read-enum (string cache)
  (error "not implemented"))

(defun haxe-serializer-read-custom (string cache)
  (error "not implemented"))

(defun haxe-serializer-read-object-cache (string cache &optional prefix)
  (cl-destructuring-bind (index len) (haxe-serializer-read-int string cache)
    (unless prefix (setf prefix ?r))
    (list (gethash (format "%c%d" prefix index)
                   (haxe-serializer-cache-table cache)) len)))

(defun haxe-serializer-read-string-cache (string cache)
  (haxe-serializer-read-object-cache string cache ?R))

;; writing

(defun haxe-serializer-write (object)
  (with-output-to-string
    (haxe-serializer-write-1
     object
     (haxe-make-serializer-cache) (haxe-make-serializer-cache))))

(defun haxe-serializer-write-1 (object scache ocache)
  (funcall 
   (cl-typecase object
     (null 'haxe-serializer-write-bool)
     (integer 'haxe-serializer-write-int)
     (float 'haxe-serializer-write-float)
     ((or string symbol) 'haxe-serializer-write-string)
     (cons 'haxe-serializer-write-list)
     (array 'haxe-serializer-write-array)
     (hash-table 'haxe-serializer-write-hash)
     (t (if (eq object t) haxe-serializer-write-bool
          (error "Don't know how to serialize %s" object))))
   object scache ocache))

(defun haxe-serializer-write-bool (object scache ocache)
  (princ (if object "t" "n")))

(defun haxe-serializer-write-int (object scache ocache)
  (princ (if (zerop object) "z") (format "i%d" object)))

(defun haxe-serializer-write-float (object scache ocache)
  (princ
   (cl-case object
     ((0.0e+NAN 0.0e-NAN) "k")
     (1.0e-INF "m")
     (1.0e+INF "p")
     (otherwise (format "d%f" object)))))

(defun haxe-serializer-write-string (object scache ocache)
  (when (symbolp object) (setf object (cl-coerce object 'string)))
  (setf object (url-hexify-string object))
  (princ (format "y%d:%s" (length object) object)))

(defun haxe-alist-p (list)
  (cl-loop for element in list
           unless (and (consp element) (cdr element)) do (cl-return)
           finally (cl-return t)))

(defun haxe-serializer-write-list (object scache ocache)
  (let* ((alistp (haxe-alist-p object))
         (start (if alistp "o" "l"))
         (end (if alistp "g" "h")))
    (princ start)
    (cl-loop for element in object do
             (haxe-serializer-write-1 element scache ocache))
    (princ end)))

(defun haxe-serializer-write-array (object scache ocache)
  (princ "a")
  (cl-loop for element across object do
           (haxe-serializer-write-1 element scache ocache))
  (princ "h"))

(defun haxe-hash-type (table)
  (cl-loop with types = (list ?b ?q ?M)
           for key being the hash-key of object do
           (cl-typecase key
             (integer (setf types (delete ?b types)))
             (string (setf types (delete ?q types))))
           finally (cl-return (car (nreverse types)))))

(defun haxe-serializer-write-hash (object scache ocache)
  (write-char (haxe-hash-type object))
  (cl-loop for key being the hash-key of object
           using (hash-value value) do
           (haxe-serializer-write-1 key scache ocache)
           (haxe-serializer-write-1 value scache ocache))
  (princ "h"))

(defun haxe-serializer-write-enum (object scache ocache)
  (error "not implemented"))

(provide 'haxe-serializer)
