(defmacro aif (test true-form false-form)
  `(if ,test
       ,true-form
       ,false-form))

(defmacro switch ((val &key (test 'equal)) &body cases)
  (let ((valname (gensym)))
    `(let ((,valname ',val))
      ,(cons 'cond
        (loop for form in cases collect
          (if (eql (car form) 'otherwise)
              (cons t (cdr form))
              `((,test ,valname ',(car form)) ,@(cdr form))))))))
