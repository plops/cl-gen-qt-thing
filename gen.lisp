(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator)
  (ql:quickload :xmls))

;; qtcreator can open the cmake file
;; https://stackoverflow.com/questions/10591635/can-i-get-mouse-events-in-a-qgraphicsitem

(in-package :cl-cpp-generator)
;; -graphicssystem raster
#+nil
(with-open-file (s "~/stage/cl-gen-qt-thing/source/main_win.ui")
  (xmls:parse s))
#+nil
(xmls:toxml 
 '("ui" (("version" "4.0")) ("class" nil "MainWindow")
   ("widget" (("name" "MainWindow") ("class" "QMainWindow"))
    ("property" (("name" "geometry"))
     ("rect" nil ("x" nil "0") ("y" nil "0") ("width" nil "400")
	     ("height" nil "300")))
    ("property" (("name" "windowTitle")) ("string" nil "MainWindow"))
    ("widget" (("name" "centralWidget") ("class" "QWidget"))))
   ("layoutdefault" (("margin" "11") ("spacing" "6"))) ("resources" nil)
   ("connections" nil)))

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
	 (include <QGraphicsScene>)
       (include <QGraphicsView>)
       (include <QGraphicsRectItem>)
       (include <QGraphicsItemGroup>)
       (include <QDebug>)

	 
	 (with-namespace Ui
		    (raw "class MainWindow;"))
       #+nil (class MainWindow ("public QMainWindow")
	      (raw "Q_OBJECT")
	      (access-specifier public)
	      (function (MainWindow ((parent :type QWidget* :default nullptr)) explicit))
	      (function (~MainWindow ()))
	      (access-specifier private)
	      ;(decl ((ui :type "Ui::MainWindow*")))
	      )
       #+nil (class graph_widget ("public QGraphicsView")
	      (raw "Q_OBJECT")
	      (access-specifier public)
	      (function (graph_widget ((parent :type QWidget* :default nullptr)) explicit))
	      (access-specifier private)
	      ;(decl ((center_node :type node_t)))
	      )
       
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
					;(include "ui_main_win.h")
       
       #+nil (function ("graph_widget::graph_widget" ((parent :type QWidget*)) nil :ctor ((QGraphicsView parent))
					       )
		 (let ((scene :type QGraphicsScene* :init (new (funcall QGraphicsScene this))))
		   (funcall scene->setSceneRect -200 -200 400 400)
		   (funcall scene->addText (string "Hello"))
		   (funcall setScene scene)
		   
		   (funcall setWindowTitle (funcall tr (string "elastic nodes")))))
       #+nil (function ("MainWindow::MainWindow" ((parent :type QWidget*)) nil
			 :ctor ((QMainWindow parent)
				;(ui "new Ui::MainWindow")
				)
			 )
		 (raw "//")
		 ;(funcall "ui->setupUi" this)
		 )
       #+nil (function ("MainWindow::~MainWindow" ())
		 (raw "// ")
		 ;(delete ui)
		 ))))  
  (with-open-file (s *main-cpp-filename*
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (emit-cpp
    :str s
    :clear-env t
    :code 
    `(with-compilation-unit
	; (include "main_win.h")
       (include <QApplication>)
       ;(include <QtGui>)
       (include <QGraphicsScene>)
       (include <QGraphicsView>)
       (include <QGraphicsRectItem>)
       (include <QGraphicsLineItem>)
       (include <QGraphicsItemGroup>)
       (include <QDebug>)

	 (class CustomView ("public QGraphicsView")
		(access-specifier protected)
		(function (mouseReleaseEvent ((event :type QMouseEvent*)) void)
			  (<< (funcall qDebug) (string "Custom view mouse released."))
			  (funcall "QGraphicsView::mouseReleaseEvent" event)))
	 (class CustomRectItem (;"public QObject" ;; inherit from QObject first
				"public QGraphicsRectItem")
		#+nil (raw "Q_OBJECT")
		(access-specifier public)
		(function (CustomRectItem ((rect :type "const QRectF&"))
					  explicit
					  :parent-ctor
					  ((QGraphicsRectItem rect)))
			  (funcall this->setFlag "QGraphicsItem::ItemIsMovable")
			  ;(funcall this->setFlag "QGraphicsItem::ItemSendsGeometryChanges")
			  (funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges"))
		(function (addLine ((line :type QGraphicsLineItem*)
				    (is_first_point_p :type bool)) void)
			  (setf this->line line
				first_point_p is_first_point_p))
		(function (itemChange ((change :type GraphicsItemChange)
				       (value :type "const QVariant&")) QVariant)
			  ;; http://stackoverflow.com/questions/32192607/how-to-use-itemchange-from-qgraphicsitem-in-qt
			  
			  (if (&& (== ItemPositionChange change)
				  (funcall scene))
			      (statements
			       (raw "// value is the same as pos()")
			       (funcall moveLineToCenter (funcall value.toPointF))
			       ))
			  (return (funcall "QGraphicsItem::itemChange" change value)))
		(access-specifier protected)
		(function (mouseReleaseEvent ((event :type QGraphicsSceneMouseEvent*)) void)
			  (<< (funcall qDebug) (string "mouse released in ") (funcall this->pos))
			  (funcall moveLineToCenter (funcall this->pos))
			  (funcall "QGraphicsRectItem::mouseReleaseEvent" event))
		
		
		
		(access-specifier private)
		(function (moveLineToCenter ((newPos :type QPointF)) void)
			  (let ((p1 :init (? first_point_p newPos "line->line().p1()"))
				(p2 :init (? first_point_p "line->line().p2()" newPos)))
			    (funcall line->setLine (funcall QLineF p1 p2))))
		(decl ((line :type "QGraphicsLineItem*")
		       (first_point_p :type bool))))

	 (include "main.moc")
	 
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (if (== 0 argc)
		     (return 0))
		 (if (== nullptr argv)
		     (return 0))
		 (let ((a :type QApplication :ctor (comma-list argc argv))
		       (w :type CustomView)
		       )
		  (funcall w.setAttribute "Qt::WA_TranslucentBackground" false)
		  (let ((tr :init (funcall QTransform)))
		    (funcall tr.rotate 45 "Qt::ZAxis")
		    (funcall w.setTransform tr))
		  ;; BoundingRectViewportUpdate
		  (let ((scene :init (new (funcall QGraphicsScene 0 0 300 300 &w))))
		    (funcall scene->setBackgroundBrush "Qt::yellow")
		    (funcall w.setScene scene)
		    
		    (let ((rect  :init (new (funcall CustomRectItem (funcall QRectF -7 -7 7 7))))
			  (rect2 :init (new (funcall CustomRectItem (funcall QRectF -7 -7 7 7)))))
		      (funcall rect->setPos 50 50)
		      (funcall rect2->setPos 10 20)
		      
		      (let ((line :init (funcall scene->addLine (funcall QLineF 40 40 80 80)))
			    )
			(funcall rect->addLine line true)
			(funcall rect2->addLine line false))
		      
		     
		      (funcall scene->addItem rect)
		      (funcall scene->addItem rect2)
		      #+nil (let ((grp :init (new (funcall QGraphicsItemGroup rect))))
			      (funcall grp->addToGroup rect)
			      (funcall grp->addToGroup rect2)
			      (funcall scene->addItem grp))
		      #+nil (let ((ql :type QList<QGraphicsItem*> :ctor (list rect rect2))
				  (grp :init (funcall scene->createItemGroup ql)))
			      )
		      (let ((tr :init (funcall QTransform)))
			(funcall tr.rotate 45 "Qt::ZAxis")
			(funcall rect2->setTransform tr))
		      (funcall scene->addText (string "hello"))))
		  (funcall w.show)
		  
		  (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-h-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





