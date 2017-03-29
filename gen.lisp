(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))
(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *main-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
   
    :code 
    `(with-compilation-unit
	 (include <QApplication>)
       (include <QWidget>)
       (class Widget ("public QWidget")
	      (raw "Q_OBJECT")
	      (access-specifier public)
	      (function (Widget ((parent :type QWidget* :default nullptr)) explicit
				:ctor ((QWidget parent)
				       (ui "new Widget")))
			(funcall "ui->setupUi" this))
	      (function (~Widget ())
			(delete ui))
	      (function (setName ((name :type "const QString&")) void)
			(funcall "ui->lineEdit->setText" name))
	      (function (name () nil :specifier const)
			(return (funcall "ui->lineEdit->text")))
	      (access-specifier private)
	      (decl ((ui :type Widget*))))
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (return 0)))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





