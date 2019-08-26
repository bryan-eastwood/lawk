(defun read-grammar (filename)
  (with-open-file (stream filename)
    (let ((eof-value (gensym)))
      (loop for rule = (read stream nil eof-value)
            while (not (eql rule eof-value)) collect rule))))

(defparameter *grammar* (read-grammar "grammar.lisp"))
(defparameter *terminals* '(name number string ere func-name
                            begin end break continue delete
                            do else exit for function if in
                            next print printf return while
                            builtin-func-name getline
                            add-assign sub-assign mul-assign
                            div-assign mod-assign pow-assign
                            or and no-match eq le ge ne incr
                            decr append #\{ #\} #\( #\) #\[
                            #\] #\, #\; newline #\+ #\- #\*
                            #\% #\^ #\! #\> #\< #\| #\? #\:
                            #\~ #\$ #\=))

(defparameter *start* 'program)

(defun get-txs (s)
  (loop for rule in *grammar*
        when (eql (car rule) s) return (cdr rule)))

(defun terminalp (s)
  (member s *terminals*))

(defun completep (seq)
  (every #'terminalp seq))
