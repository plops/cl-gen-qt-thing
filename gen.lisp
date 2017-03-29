(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))


;; https://github.com/mjspncr/lzz3

(progn
  (defparameter *main-win-cpp-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main_win.cpp"
						      (user-homedir-pathname)))
  (defparameter *main-win-h-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main_win.h"
						      (user-homedir-pathname)))
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main.cpp"
						   (user-homedir-pathname)))
  
  (with-open-file (s *main-win-cpp-filename*
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
       (function (Widget ((parent :type QWidget* :default nullptr)) explicit
			 :ctor ((QWidget parent)
				(ui "new Widget")))
		 (funcall "ui->setupUi" this))
       (function (~Widget ())
		 (delete ui))
       (function (setName ((name :type "const QString&")) void)
		 (funcall "ui->lineEdit->setText" name))
       (function (name () nil :specifier const)
		 (return (funcall "ui->lineEdit->text"))))))

  (with-open-file (s *main-win-h-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
   
    :code 
    `(with-compilation-unit
	 (raw "#ifndef MAIN_WIN_H")
       (raw "#define MAIN_WIN_H")
       
	 (include <QMainWindow>)
       (include <QWidget>)
       (class MainWindow ("public QMainWindow")
	      (raw "Q_OBJECT")
	      (access-specifier public)
	      (function (MainWindow ((parent :type QWidget* :default nullptr)) explicit))
	      (function (~MainWindow ()))
	      (access-specifier private)
	      (decl ((ui :type MainWindow*))))
       (raw "#endif // MAIN_WIN_H"))))
  
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
		 (let ((a :type QApplication :ctor (comma-list argc argv))
		       (w :type Widget))
		   (funcall w.setName "shady")
		   (funcall w.show)
		   
		   (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-h-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





