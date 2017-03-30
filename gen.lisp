(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator)
  (ql:quickload :xmls))

(in-package :cl-cpp-generator)

#+nil
(with-open-file (s "~/stage/cl-gen-qt-thing/source/main_win.ui")
 (xmls:parse s))
#+nil
("ui" (("version" "4.0")) ("class" nil "MainWindow")
    ("widget" (("name" "MainWindow") ("class" "QMainWindow"))
     ("property" (("name" "geometry"))
      ("rect" nil ("x" nil "0") ("y" nil "0") ("width" nil "400")
       ("height" nil "300")))
     ("property" (("name" "windowTitle")) ("string" nil "MainWindow"))
     ("widget" (("name" "centralWidget") ("class" "QWidget"))))
    ("layoutdefault" (("margin" "11") ("spacing" "6"))) ("resources" nil)
    ("connections" nil))

#+nil
("ui" (("version" "4.0")) ("class" nil "MainWindow")
    ("widget" (("name" "MainWindow") ("class" "QMainWindow"))
     ("property" (("name" "geometry"))
      ("rect" nil ("x" nil "0") ("y" nil "0") ("width" nil "400")
       ("height" nil "300")))
     ("property" (("name" "windowTitle")) ("string" nil "MainWindow"))
     ("widget" (("name" "centralWidget") ("class" "QWidget")))
     ("widget" (("name" "menuBar") ("class" "QMenuBar"))
      ("property" (("name" "geometry"))
       ("rect" nil ("x" nil "0") ("y" nil "0") ("width" nil "400")
        ("height" nil "29"))))
     ("widget" (("name" "mainToolBar") ("class" "QToolBar"))
      ("attribute" (("name" "toolBarArea")) ("enum" nil "TopToolBarArea"))
      ("attribute" (("name" "toolBarBreak")) ("bool" nil "false")))
     ("widget" (("name" "statusBar") ("class" "QStatusBar"))))
    ("layoutdefault" (("margin" "11") ("spacing" "6"))) ("resources" nil)
    ("connections" nil))


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
	 (with-namespace Ui
		    (raw "class MainWindow;"))
       (class MainWindow ("public QMainWindow")
	      (raw "Q_OBJECT")
	      (access-specifier public)
	      (function (MainWindow ((parent :type QWidget* :default nullptr)) explicit))
	      (function (~MainWindow ()))
	      (access-specifier private)
	      (decl ((ui :type "Ui::MainWindow*"))))
       (raw "#endif // MAIN_WIN_H"))))
  (with-open-file (s *main-win-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
   
    :code 
    `(with-compilation-unit
	 (include "main_win.h")
       (include "ui_main_win.h")
       
       (function ("MainWindow::MainWindow" ((parent :type QWidget*)) nil
			 :ctor ((QMainWindow parent)
				(ui "new Ui::MainWindow")))
		 (funcall "ui->setupUi" this)
		 )
       (function ("MainWindow::~MainWindow" ())
		 (delete ui)))))  
  (with-open-file (s *main-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
    :code 
    `(with-compilation-unit
	 (include "main_win.h")
       (include <QApplication>)
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (let ((a :type QApplication :ctor (comma-list argc argv))
		       (w :type MainWindow))
		   (funcall w.show)
		   
		   (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-h-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





