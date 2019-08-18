;;;;
;;;; Bryan Eastwood 2019
;;;;
;;;; Lexing rules adapted from
;;;; http://pubs.opengroup.org/onlinepubs/009695399/utilities/awk.html
;;;;

(defmethod read-until ((c character) stream)
  (read-until (lambda (x) (char= x c)) stream))

(defmethod read-until ((f function) stream)
  (loop with buf = (make-array 32 :fill-pointer 0
                                  :adjustable t)
        for c = (if (listen stream)
                    (peek-char nil stream)
                    (return-from read-until (coerce buf 'string))) do
    (if (funcall f c)
        (return-from read-until (coerce buf 'string))
        (vector-push-extend (read-char stream) buf))))

(defun read-string-literal (stream)
  ;; TODO: Implement string parsing according
  ;; to IEEE Std 1003.1-2001 Ch. 5
  (read-until #\" stream))

(defun read-ere (stream)
  ;; TODO: Implement string parsing according
  ;; to IEEE Std 1003.1-2001 Ch. 5, and #6
  ;; `Expressions in Decreasing Precedence in awk'
  (read-until #\/ stream))

(defun read-number (stream)
  (let* ((str (read-until (lambda (c) (not (find c "0123456789."))) stream))
         (p (position #\. str)))
    (if p
        (coerce
          (+ (parse-integer str :end p)
             (/ (parse-integer str :start (1+ p) :end (length str))
                (expt 10 (- (length str) (1+ p)))))
          'double-float)
        (parse-integer str))))

(defun find-function (word)
  (if (find word '("atan2" "close" "cos" "exp" "gsub" "index" "int" "length"
                   "log" "match" "rand" "sin" "split" "sprintf" "sqrt" "srand"
                   "sub" "substr" "system" "tolower" "toupper") :test #'string=)
      (list 'builtin-func-name (intern (string-upcase word)))
      (list 'func-name word)))

(defun find-name (word)
  (if (find word '("BEGIN" "break" "continue" "delete" "do" "else" "END" "exit"
                   "for" "function" "getline" "if" "in" "next" "print" "printf"
                   "return" "while") :test #'string=)
      (list (intern (string-upcase word)))
      (list 'name word)))

(defun read-word (stream)
  (let* ((word (read-until (lambda (c) (not (or (alpha-char-p c)
                                                (digit-char-p c)
                                                (char= c #\_))))
                  stream))
         (c (peek-char nil stream nil #\Null)))
    (cond ((char= c #\() (find-function word))
          (t (find-name word)))))

(defun read-other-token (stream &aux (c (peek-char nil stream)))
  (cond ((find c "+-*/%^|&!=<>")
          (read-char stream)
          (let* ((d (peek-char nil stream))
                 (op (find-if (lambda (x)
                           (string= (cadr x)
                             (concatenate 'string (list c d))))
                       '((add-assign "+=")
                         (sub-assign "-=")
                         (mul-assign "*=")
                         (div-assign "/=")
                         (mod-assign "%=")
                         (pow-assign "^=")
                         (or "||")
                         (and "&&")
                         (no-match "!~")
                         (eq "==")
                         (le "<=")
                         (ge ">=")
                         (ne "!=")
                         (incr "++")
                         (decr "--")
                         (append ">>")))))
            (if op
                (list (car op))
                (list c))))

        ((find c "{}()[],;+-*%^!><|?:~$=") (read-char stream) (list c))))

(defun take-token (stream &aux (c (peek-char nil stream)))
  (cond ((char= c #\#) (read-until #\newline stream)
                       (take-token stream))

        ((char= c #\newline) (read-char stream)
                             (list 'newline))

        ((char= c #\\) (read-char stream)
                       (if (char= (peek-char nil stream) #\newline)
                           (progn (read-char stream) (take-token stream))
                           (error "Unexpected \\")))

        ((char= c #\") (prog2 (read-char stream)
                              (list 'string (read-string-literal stream))
                              (read-char stream)))

        ((char= c #\/) (prog2 (read-char stream)
                              (list 'ere (read-ere stream))
                              (read-char stream)))

        ((or (char= c #\space)
             (char= c #\tab)) (read-char stream)
                              (take-token stream))

        ((digit-char-p c) (list 'number (read-number stream)))

        ((or (alpha-char-p c)
             (char= c #\_)) (read-word stream))

        (t (or (read-other-token stream) (error "Unrecognized character: ~a" c)))))

(defmethod tokenize ((string string))
  (with-input-from-string (stream string)
    (tokenize stream)))

(defmethod tokenize ((stream stream))
  (loop while (peek-char t stream nil nil)
        collect (take-token stream)))
