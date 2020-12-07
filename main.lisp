(in-package #:isthmus)

(defun clean (s)
  (substitute #\_ #\- (string-downcase (string s))))

(defun type-map (type)
  (let* ((type-string (string-upcase (string type))))
    (cond
      ((string= type-string "INTEGER")
       "integer")
      ((or (string= type-string "SINGLE-FLOAT")
	   (string= type-string "DOUBLE-FLOAT"))
       "real")
      ((string= type-string "STRING")
       "text")
      (t (error "ah crap")))))

(defun create-field-sql (slot)
  (format nil "~a ~a"
	  (c2mop:slot-definition-name slot)
	  (type-map
	   (c2mop:slot-definition-type slot))))

(defun remove-id (slots)
  (remove-if (lambda (s) (string= (clean s) "id"))
	     slots :key #'c2mop:slot-definition-name))

(defun get-all-slots (class-name)
  (c2mop:class-slots (find-class class-name)))

(defun get-slots (class-name)
  (remove-id (get-all-slots class-name)))

(defun get-all-slot-names (class-name)
  (mapcar #'c2mop:slot-definition-name (get-all-slots class-name)))

(defun get-slot-names (class-name)
  (mapcar #'c2mop:slot-definition-name (get-slots class-name)))

(defun prefix-symbol (str sym)
  (intern (string-upcase (concatenate 'string str (symbol-name sym)))))

(defun sandwich-symbol (str1 sym str2)
  (intern (string-upcase (concatenate 'string str1 (symbol-name sym) str2))))

(defun create-table-sql (class-name)
  (let* ((slots (get-slots class-name)))
    (clean
     (format nil "create table ~a (id integer primary key ~{, ~a ~})"
	     (string class-name)
	     (loop for s in slots collect (create-field-sql s))))))

(defun create-index-sql (class-name field-name)
  (clean
   (format nil "create index idx_~a_~a on ~a (~a)" class-name field-name class-name field-name)))

(defun get-field-names (field-map)
  (loop
     for n from 1 to (length field-map)
     for field in field-map
     when (oddp n)
     collect field))
(defun get-field-values (field-map)
  (loop
     for n from 1 to (length field-map)
     for field in field-map
     when (evenp n)
     collect field))

(defun create-insert-sql (class-name slot-names)
  (clean
   (format nil "insert into ~a (~{ ~a ~^,~}) values (~{ ?~* ~^,~})" class-name slot-names slot-names)))

(defun create-insert-func (class-name)
  (let ((slot-names (get-slot-names class-name))
	(func-name (prefix-symbol "insert-" class-name)))
    `(defun ,func-name (db ,class-name)
       (with-slots (,@slot-names) ,class-name
	 (sqlite:execute-non-query
	  db
	  ,(create-insert-sql class-name slot-names)
	  ,@slot-names)
	 (setf (isthmus::id ,class-name) (sqlite:execute-single db "select last_insert_rowid()"))))))

(defun create-update-sql (class-name slot-names)
  (clean
   (format nil "update ~a set~{ ~a = ?~^ ,~} where id = ?" class-name slot-names)))

(defun create-update-func (class-name)
  (let* ((slot-names (get-slot-names class-name))
	 (slot-names+id (append slot-names (cons 'isthmus::id nil)))
	 (func-name (prefix-symbol "update-" class-name)))
    `(defun ,func-name (db ,class-name)
       (with-slots (,@slot-names+id) ,class-name
	 (sqlite:execute-non-query
	  db
	  ,(create-update-sql class-name slot-names)
	  ,@slot-names+id)))))

(defun create-save-func (class-name)
  (let ((func-name (prefix-symbol "save-" class-name))
	(update-name (prefix-symbol "update-" class-name))
	(insert-name (prefix-symbol "insert-" class-name)))
    `(defun ,func-name (db ,class-name)
       (if (isthmus::id ,class-name)
	   (,update-name db ,class-name)
	   (,insert-name db ,class-name))
       ,class-name)))

(defun create-save-list-func (class-name)
  (let ((save-tree-func (sandwich-symbol "save-" class-name "-tree"))
	(save-func (prefix-symbol "save-" class-name))
	(parm-name (sandwich-symbol "" class-name "-list")))
    `(defun ,save-tree-func (db ,parm-name)
       (let ((first (first ,parm-name))
	     (rest (rest ,parm-name)))
	 (cond
	   ((consp first) (,save-tree-func db first))
	   ((typep first (quote ,class-name)) (,save-func db first)))
	 (when rest
	   (,save-tree-func db rest)))
       ,parm-name)))

(defun create-create-table-func (class-name &optional indices)
  (let ((func-name (sandwich-symbol "create-" class-name "-table")))
    `(defun ,func-name (db)
       (sqlite:execute-non-query
	db
	,(create-table-sql class-name))
       ,@(loop for index in indices collect
	      `(sqlite:execute-non-query
		db
		,(create-index-sql class-name index))))))

(defun create-drop-table-func (class-name)
  (let ((func-name (sandwich-symbol "drop-" class-name "-table")))
    `(defun ,func-name (db)
       (sqlite:execute-non-query
	db
	,(format nil "drop table ~a" (clean class-name))))))

(defun create-inflate-func (class-name)
  (let ((func-name (prefix-symbol "inflate-" class-name))
	(parm-name (sandwich-symbol "" class-name "-attr"))
	(field-names (get-slot-names class-name)))
    `(defun ,func-name (,parm-name)
       (let ((,class-name (make-instance (quote,class-name))))
	 (with-slots ,field-names ,class-name
	   (setf (isthmus::id ,class-name) (nth 0 ,parm-name))
	   ,@(loop
		for field in field-names 
		for k from 1 to (length field-names) collect
		  `(setf ,field (nth ,k ,parm-name))))
	 ,class-name))))

(defun create-select-sql (class-name field-names)
  (clean
   (format nil "select~{ ~a~^ ,~} from ~a where id = ?" field-names class-name)))

(defun create-get-func (class-name)
  (let ((field-names (get-all-slot-names class-name))
	(func-name (prefix-symbol "get-" class-name))
	(inflate-func-name (prefix-symbol "inflate-" class-name)))
    `(defun ,func-name (db id)
       (,inflate-func-name
	(first
	 (sqlite:execute-to-list
	  db
	  ,(create-select-sql class-name field-names)
	  id))))))

(defun create-get-list-func (class-name)
  (let ((func-name (sandwich-symbol "get-" class-name "-list"))
	(inflate-func-name (prefix-symbol "inflate-" class-name)))
    `(defun ,func-name (db where &rest parms)
       (let ((full-query (concatenate 'string (format nil "select * from ~a " (clean (symbol-name (quote ,class-name)))) where)))
	 (mapcar
	  (function ,inflate-func-name)
	  (sqlite:execute-to-list
	   db
	   (apply #'format (append (list nil full-query) parms))))))))

(defun create-delete-func (class-name)
  (let ((func-name (prefix-symbol "delete-" class-name)))
    `(defun ,func-name (db id)
       (sqlite:execute-non-query
	db
	,(format nil "delete from ~a where id = ?" (clean class-name))
	id))))

#|
(defun foreign-slot-p (slot)
  (let* ((slot-name (symbol-name slot))
	 (len (length slot-name))
	 (suffix (string-upcase (subseq slot-name (- len 3) len))))
    (string= suffix "-ID")))

(defun create-foreign-access-funcs (class-name)
  (loop for slot in (get-slot-names class-name)
     when (foreign-slot-p slot)
     collect
       (let* ((sym-str (symbol-name slot))
	      (foreign-str (subseq sym-str 0 (- (length sym-str) 3)))
	      (get-func-name (sandwich-symbol "get-" class-name
					      (concatenate 'string "-" foreign-str))))
	 `(defun ,get-func-name (,class-name)
	    (with-slots (,slot) ,class-name
	      (,(prefix-symbol "get-" (intern foreign-str)) ,slot))))))
|#

(defun create-foreign-interface (class-name field-type-map)
  (loop for pair in field-type-map
     collect
       (let* ((slot (first pair))
	      (slot-type (second pair))
	      (slot-name (symbol-name slot))
	      (slot-len (length slot-name))
	      (slot-name-wo-id (subseq slot-name 0 (- slot-len 3)))
	      (get-func (prefix-symbol "get-" slot-type))
	      (foreign-suffix (concatenate 'string "-" slot-name-wo-id))
	      (foreign-func (sandwich-symbol "get-" class-name foreign-suffix)))
	 `(defun ,foreign-func (,class-name)
	    (with-slots (,slot) ,class-name
	      (,get-func ,slot))))))


(defmacro build-interface (class-name &optional indices field-type-map)
  (c2mop:ensure-finalized (find-class class-name))
  `(progn
     ,(create-create-table-func class-name indices)
     ,(create-drop-table-func class-name)
     ,(create-insert-func class-name)
     ,(create-update-func class-name)
     ,(create-save-func class-name)
     ,(create-save-list-func class-name)
     ,(create-delete-func class-name)
     ,(create-inflate-func class-name)
     ,(create-get-func class-name)
     ,(create-get-list-func class-name)
     ,@(when field-type-map
	 (create-foreign-interface class-name field-type-map))))

(defclass/std persistent () ((id :type integer)))
