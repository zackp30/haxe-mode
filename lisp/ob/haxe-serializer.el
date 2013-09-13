(defstruct haxe-serializer-cache table index)

(defun haxe-make-serializer-cache ()
  (make-haxe-serializer-cache
   :table (make-hash-table :test #'equal) :index 0))

(defun haxe-serializer-put-cache (object cache kind)
  (let ((index (1+ (haxe-serializer-cache-index cache))))
    (puthash (format "%c%d" kind index)
             object (haxe-serializer-cache-table cache))))

(defun haxe-serializer-read (string &optional cache)
  (unless cache (setf cache (haxe-make-serializer-cache)))
  (case (aref string 0)
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
    (?w (haxe-serializer-read-enum string cache))
    (?r (haxe-serializer-read-object-cache string cache))
    (?R (haxe-serializer-read-string-cache string cache))
    (otherwise (error "invalid head: %c" (aref string 0)))))

(defun haxe-serializer-read-int (string cache)
  (string-match "[[:digit:]]+" string 1)
  (let ((digits (substring string (match-beginning 0) (match-end 0))))
    (list (parse-integer digits) (1+ (length digits)))))

(defun haxe-serializer-read-float (string cache)
  (string-match
   "\\(:?[[:digit:]]*\\.[[:digit:]]+\\(e[+-]?[[:digit:]]\\)?\\)\\|[[:digit:]]+"
   string 1)
  (let ((digits (substring string (match-beginning 0) (match-end 0))))
    (list (string-to-number digits 10) (1+ (length digits)))))

(defun haxe-serializer-read-string (string cache)
  (destructuring-bind (len chars)
      (haxe-serializer-read-int string cache)
    (let ((result (url-unhex-string (substring string (1+ chars) (+ 1 chars len)) t)))
      (haxe-serializer-put-cache result cache ?R)
      (list result (+ 1 chars len)))))

;; Somewhat misleading name, but it isn't really a struct in Haxe, it's more
;; like a pairlist really
(defun haxe-serializer-read-struct (string cache &optional terminator)
  (loop with result = nil and extension = 1
        and cell = nil and string = (substring string 1)
        and terminator = (or terminator ?g)
        until (char-equal (aref string 0) terminator)
        for key = (haxe-serializer-read string cache)
        do (destructuring-bind (key-result key-chars) key
             (incf extension key-chars)
             (setf cell key-result string (substring string key-chars)))
        for value = (haxe-serializer-read string cache)
        do (destructuring-bind (value-result value-chars) value
             (incf extension value-chars)
             (setf string (substring string value-chars))
             (push (cons cell value-result) result))
        finally
        (let ((result (nreverse result)))
          (haxe-serializer-put-cache result cache ?r)
          (return (list result (1+ extension))))))

(defun haxe-serializer-read-list (string cache)
  (loop with result = nil and string = (substring string 1)
        and extension = 0
        until (char-equal (aref string 0) ?h)
        do (destructuring-bind (element len)
               (haxe-serializer-read string cache)
             (push element result)
             (setf extension (+ extension len)
                   string (substring string len)))
        finally
        (let ((result (nreverse result)))
          (haxe-serializer-put-cache result cache ?r)
          (return (list result (+ 2 extension))))))
        
(defun haxe-serializer-read-array (string cache)
  (destructuring-bind (list len) (haxe-serializer-read-list string cache)
    (list (coerce list 'array) len)))

(defun haxe-serializer-read-date (string cache)
  (error "not implemented"))

(defun haxe-serializer-read-string-hash (string cache)
  (destructuring-bind (list len) (haxe-serializer-read-struct string cache ?h)
    (let ((result (make-hash-table :test #'equal)))
      (mapc (lambda (x) (puthash (car x) (cdr x) result)) list)
      (list result len))))

(defun haxe-serializer-read-int-hash (string cache)
  (destructuring-bind (list len) (haxe-serializer-read-struct string cache ?h)
    (let ((result (make-hash-table)))
      (mapc (lambda (x) (puthash (car x) (cdr x) result)) list)
      (list result len))))

(defun haxe-serializer-read-bytes (string cache)
  (destructuring-bind (string len) (haxe-serializer-read-string string cache)
    (loop for f in '("%s" "%s=" "%s==") do
          (condition-case _
              (return (list (base64-decode-string (format f string)) len))
              (error ())))))

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
  (destructuring-bind (index len) (haxe-serializer-read-int string cache)
    (unless prefix (setf prefix ?r))
    (list (gethash (format "%c%d" prefix index)
                   (haxe-serializer-cache-table cache)) len)))

(defun haxe-serializer-read-string-cache (string cache)
  (haxe-serializer-read-object-cache string cache ?R))
