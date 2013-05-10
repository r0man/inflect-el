
;; TODO: Handle arrays
(defun inflect-transform (arg transform-fn)
  "Transform ARG with TRANSFORM-FN. ARG can be an assoc list,
hash table, string or a symbol. If ARG is an assoc list or hash
table only the keys will be transformed."
  (cond
   ;; ((and (consp arg))
   ;;  (mapcar (lambda (c) (cons (inflect-transform (car c) transform-fn) (cdr c))) arg))
   ((and (listp arg) (listp (car arg)))
    (mapcar (lambda (c) (cons (inflect-transform (car c) transform-fn) (cdr c))) arg))
   ((and (listp arg) (atom (car arg)))
    (cons (inflect-transform (car arg) transform-fn)
          (cdr arg)))
   ((hash-table-p arg)
    (let ((other (make-hash-table :test 'equal)))
      (maphash (lambda (key value) (puthash (inflect-transform key transform-fn) value other)) arg)
      other))
   ((stringp arg)
    (funcall transform-fn arg))
   ((symbolp arg)
    (intern (inflect-transform (symbol-name arg) transform-fn)))))

(defun inflect-dasherize (arg)
  "Replace each underscore in ARG with a dash. ARG can be an
association list, hash table, string or a symbol. If ARG is an
association list or hash table only the keys will be dasherized."
  (inflect-transform arg (lambda (string) (replace-regexp-in-string "_" "-" string))))

(defun inflect-underscore (arg)
  "Replace each underscore in ARG with a dash. ARG can be an
association list, hash table, string or a symbol. If ARG is an
association list or hash table only the keys will be underscored."
  (inflect-transform arg (lambda (string) (replace-regexp-in-string "-" "_" string))))

(defun inflect-url-encode (params)
  "Return a string that is PARAMS URI-encoded. PARAMS can be a
number, string, symbol or an association list."
  (cond
   ((stringp params)
    (url-hexify-string params))
   ((symbolp params)
    (intern (inflect-url-encode (symbol-name params))))
   ((listp params)
    (if (listp (car params))
        (mapconcat 'inflect-url-encode params "&")
      (format "%s=%s"
              (inflect-url-encode (car params))
              (inflect-url-encode (if (atom (cdr params)) (cdr params) (cadr params))))))
   (t (url-hexify-string (format "%s" params)))))

(provide 'inflect)
