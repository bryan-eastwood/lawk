;; START program

;; program          : item-list
;;                  | actionless-item-list
;;                  ;

(program           (item-list)
                   (actionless-item-list))

(item-list         (newline-opt)
                   (actionless-item-list item terminator)
                   (item-list            item terminator)
                   (item-list          action terminator))


(actionless-item-list   (item-list            pattern terminator)
                        (actionless-item-list pattern terminator))


(item              (pattern action)
                   (function NAME      #\( param-list-opt #\)
                       newline-opt action)
                   (function FUNC-NAME #\( param-list-opt #\)
                       newline-opt action))


(param-list-opt    (nil)
                   (param-list))


(param-list        (NAME)
                   (param-list #\, NAME))


(pattern           (begin)
                   (end)
                   (expr)
                   (expr #\, newline-opt expr))


(action            (#\{ newline-opt                             #\})
                   (#\{ newline-opt terminated-statement-list   #\})
                   (#\{ newline-opt unterminated-statement-list #\}))


(terminator        (terminator #\;)
                   (terminator NEWLINE)
                   (#\;)
                   (NEWLINE))


(terminated-statement-list   (terminated-statement)
                             (terminated-statement-list terminated-statement))


(unterminated-statement-list   (unterminated-statement)
                               (terminated-statement-list unterminated-statement))


(terminated-statement   (action newline-opt)
                   (if #\( expr #\) newline-opt terminated-statement)
                   (if #\( expr #\) newline-opt terminated-statement
                       else newline-opt terminated-statement)
                   (while #\( expr #\) newline-opt terminated-statement)
                   (for #\( simple-statement-opt #\;
                      expr-opt #\; simple-statement-opt #\) newline-opt
                      terminated-statement)
                   (for #\( NAME in NAME #\) newline-opt
                      terminated-statement)
                   (#\; newline-opt)
                   (terminatable-statement NEWLINE newline-opt)
                   (terminatable-statement #\;     newline-opt))


(unterminated-statement   (terminatable-statement)
                   (if #\( expr #\) newline-opt unterminated-statement)
                   (if #\( expr #\) newline-opt terminated-statement
                      else newline-opt unterminated-statement)
                   (while #\( expr #\) newline-opt unterminated-statement)
                   (for #\( simple-statement-opt #\;
                      expr-opt #\; simple-statement-opt #\) newline-opt
                      unterminated-statement)
                   (for #\( NAME in NAME #\) newline-opt
                      unterminated-statement))


(terminatable-statement   (simple-statement)
                          (break)
                          (continue)
                          (next)
                          (exit expr-opt)
                          (return expr-opt)
                          (do newline-opt terminated-statement
                              while #\( expr #\)))


(simple-statement-opt   (nil)
                        (simple-statement))


(simple-statement   (delete NAME #\[ expr-list #\])
                    (expr)
                    (print-statement))


(print-statement   (simple-print-statement)
                   (simple-print-statement output-redirection))


(simple-print-statement   (print  print-expr-list-opt)
                          (print  #\( multiple-expr-list #\))
                          (printf print-expr-list)
                          (printf #\( multiple-expr-list #\)))


(output-redirection   (#\>    expr)
                      (APPEND expr)
                      (#\|    expr))


(expr-list-opt     (nil)
                   (expr-list))


(expr-list         (expr)
                   (multiple-expr-list))


(multiple-expr-list   (expr #\, newline-opt expr)
                      (multiple-expr-list #\, newline-opt expr))


(expr-opt          (nil)
                   (expr))


(expr              (unary-expr)
                   (non-unary-expr))


(unary-expr        (#\+ expr)
                   (#\- expr)
                   (unary-expr #\^      expr)
                   (unary-expr #\*      expr)
                   (unary-expr #\/      expr)
                   (unary-expr #\%      expr)
                   (unary-expr #\+      expr)
                   (unary-expr #\-      expr)
                   (unary-expr          non-unary-expr)
                   (unary-expr #\<      expr)
                   (unary-expr LE       expr)
                   (unary-expr NE       expr)
                   (unary-expr EQ       expr)
                   (unary-expr #\>      expr)
                   (unary-expr GE       expr)
                   (unary-expr #\~      expr)
                   (unary-expr NO-MATCH expr)
                   (unary-expr in NAME)
                   (unary-expr AND newline-opt expr)
                   (unary-expr OR  newline-opt expr)
                   (unary-expr #\? expr #\: expr)
                   (unary-input-function))


(non-unary-expr    (#\( expr #\))
                   (#\! expr)
                   (non-unary-expr #\^      expr)
                   (non-unary-expr #\*      expr)
                   (non-unary-expr #\/      expr)
                   (non-unary-expr #\%      expr)
                   (non-unary-expr #\+      expr)
                   (non-unary-expr #\-      expr)
                   (non-unary-expr          non-unary-expr)
                   (non-unary-expr #\<      expr)
                   (non-unary-expr LE       expr)
                   (non-unary-expr NE       expr)
                   (non-unary-expr EQ       expr)
                   (non-unary-expr #\>      expr)
                   (non-unary-expr GE       expr)
                   (non-unary-expr #\~      expr)
                   (non-unary-expr NO-MATCH expr)
                   (non-unary-expr in NAME)
                   (#\( multiple-expr-list #\) in NAME)
                   (non-unary-expr AND newline-opt expr)
                   (non-unary-expr OR  newline-opt expr)
                   (non-unary-expr #\? expr #\: expr)
                   (NUMBER)
                   (STRING)
                   (lvalue)
                   (ERE)
                   (lvalue INCR)
                   (lvalue DECR)
                   (INCR lvalue)
                   (DECR lvalue)
                   (lvalue POW-ASSIGN expr)
                   (lvalue MOD-ASSIGN expr)
                   (lvalue MUL-ASSIGN expr)
                   (lvalue DIV-ASSIGN expr)
                   (lvalue ADD-ASSIGN expr)
                   (lvalue SUB-ASSIGN expr)
                   (lvalue #\= expr)
                   (FUNC-NAME #\( expr-list-opt #\))
                      ;; no white space allowed before #\(
                   (BUILTIN-FUNC-NAME #\( expr-list-opt #\))
                   (BUILTIN-FUNC-NAME)
                   (non-unary-input-function))


(print-expr-list-opt   (nil)
                       (print-expr-list))


(print-expr-list   (print-expr)
                   (print-expr-list #\, newline-opt print-expr))


(print-expr        (unary-print-expr)
                   (non-unary-print-expr))


(unary-print-expr   (#\+ print-expr)
                    (#\- print-expr)
                    (unary-print-expr #\^      print-expr)
                    (unary-print-expr #\*      print-expr)
                    (unary-print-expr #\/      print-expr)
                    (unary-print-expr #\%      print-expr)
                    (unary-print-expr #\+      print-expr)
                    (unary-print-expr #\-      print-expr)
                    (unary-print-expr          non-unary-print-expr)
                    (unary-print-expr #\~      print-expr)
                    (unary-print-expr NO-MATCH print-expr)
                    (unary-print-expr in NAME)
                    (unary-print-expr AND newline-opt print-expr)
                    (unary-print-expr OR  newline-opt print-expr)
                    (unary-print-expr #\? print-expr #\: print-expr))


(non-unary-print-expr   (#\( expr #\))
                        (#\! print-expr)
                        (non-unary-print-expr #\^      print-expr)
                        (non-unary-print-expr #\*      print-expr)
                        (non-unary-print-expr #\/      print-expr)
                        (non-unary-print-expr #\%      print-expr)
                        (non-unary-print-expr #\+      print-expr)
                        (non-unary-print-expr #\-      print-expr)
                        (non-unary-print-expr          non-unary-print-expr)
                        (non-unary-print-expr #\~      print-expr)
                        (non-unary-print-expr NO-MATCH print-expr)
                        (non-unary-print-expr in NAME)
                        (#\( multiple-expr-list #\) in NAME)
                        (non-unary-print-expr AND newline-opt print-expr)
                        (non-unary-print-expr OR  newline-opt print-expr)
                        (non-unary-print-expr #\? print-expr #\: print-expr)
                        (NUMBER)
                        (STRING)
                        (lvalue)
                        (ERE)
                        (lvalue INCR)
                        (lvalue DECR)
                        (INCR lvalue)
                        (DECR lvalue)
                        (lvalue POW-ASSIGN print-expr)
                        (lvalue MOD-ASSIGN print-expr)
                        (lvalue MUL-ASSIGN print-expr)
                        (lvalue DIV-ASSIGN print-expr)
                        (lvalue ADD-ASSIGN print-expr)
                        (lvalue SUB-ASSIGN print-expr)
                        (lvalue #\= print-expr)
                        (FUNC-NAME #\( expr-list-opt #\))
                          ;; no white space allowed before #\(
                        (BUILTIN-FUNC-NAME #\( expr-list-opt #\))
                        (BUILTIN-FUNC-NAME))


(lvalue            (NAME)
                   (NAME #\[ expr-list #\])
                   (#\$ expr))


(non-unary-input-function   (simple-get)
                            (simple-get #\< expr)
                            (non-unary-expr #\| simple-get))


(unary-input-function   (unary-expr #\| simple-get))


(simple-get        (GETLINE)
                   (GETLINE lvalue))


(newline-opt       (nil)
                   (newline-opt NEWLINE))
