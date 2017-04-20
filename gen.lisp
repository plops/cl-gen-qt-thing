(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

;; qtcreator can open the cmake file
;; https://stackoverflow.com/questions/10591635/can-i-get-mouse-events-in-a-qgraphicsitem
;; compile small qt
;; http://stackoverflow.com/questions/41812121/qt-lite-and-configuration-changes-in-qt-5-8
;; configure -list-features
(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))


;; https://github.com/mjspncr/lzz3

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
					; (include "main_win.h")
	  (include <QApplication>)
					;(include <QtGui>)
	(include <QGraphicsScene>)
	(include <QGraphicsView>)
	(include <QGraphicsItem>)
	(include <QGraphicsRectItem>)
	(include <QGraphicsLineItem>)
	(include <QGraphicsTextItem>)
	(include <QGraphicsItemGroup>)
	(include <QDebug>)
	
	(raw "//! This program displays a line on a canvas. The parameters of the line can be adjusted with two control points. The canvas also displays a grid of square pixels and highlights the pixels that are intersected by the line.

")
	(raw "//! Movable square. Two of these are use to define a line.

//! The two control points are distinguished by the boolean member first_point_p.")

	
	(class CustomRectItem ("public QGraphicsRectItem")
	       (access-specifier public)
	       (function (CustomRectItem ((rect :type "const QRectF&"))
					 explicit
					 :parent-ctor
					 ((QGraphicsRectItem rect)))
			 (funcall this->setFlag "QGraphicsItem::ItemIsMovable")
					;(funcall this->setFlag "QGraphicsItem::ItemSendsGeometryChanges")
			 (funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges"))
	       (raw "//! Attach a line to the control point. 

//! For each line this should be called twice for two different instances of CustomRectItem and different second parameter. Call addLine before adding the CustomRectItem to the scene. This ensures that the line coordinates are in a consistent state.")
	       (function (addLine ((line :type QGraphicsLineItem*)
				   (is_first_point_p :type bool)) void)
			 (setf this->line line
			       first_point_p is_first_point_p))
	       (raw "//! Display a label below the control point. 

//! Call addLabel before setPos. This ensures that the point coordinates are displayed correctly.")
	       (function (addLabel ()
				   void)
			 (setf text (new (funcall QGraphicsTextItem)))
			 (funcall text->setPos (funcall this->pos))
			 (funcall text->setPlainText (string "Barev"))
			 (funcall "this->scene()->addItem" text))
	       (access-specifier protected)
	       (raw "//! Update line parameters when the control point is moved with the mouse.")
	       (function (itemChange ((change :type GraphicsItemChange)
				      (value :type "const QVariant&")) QVariant)
			 ;; http://stackoverflow.com/questions/32192607/how-to-use-itemchange-from-qgraphicsitem-in-qt
					;(<< (funcall qDebug) (string "change ") (funcall this->pos) (string " ") value)
			 (if (&& (== ItemPositionChange change)
				 (funcall scene))
			     (statements
			      (raw "// value is the same as pos()")
			      #+nil (let ((dx :init 20)
				    (dy :init dx)
				    (nx :init 10)
				    (ny :init nx))
				(let ((p0 :init "line->line().p1()")
				      (p1 :init "line->line().p2()")
				      (diff :init (- p1 p0))
				      (horizontal_p :init (< (funcall diff.y)
							     (funcall diff.x)))
				      (nbig :init (? horizontal_p nx ny))
				      )
				  (dotimes (i nbig)
				    (let ((j :init (+ (* dy i)))
					  (eps :init -2))
				      (let ((y1 :init (- j eps))
					    (x1 :init (- i eps))
					    (y2 :init (+ (+ 1 j) eps))
					    (x2 :init (+ (+ 1 i) eps))
					    (rect :init (? horizontal_p
							   (funcall QRectF x1 y1 dx dy)
							   (funcall QRectF y1 x1 dx dy))))
					(funcall "this->scene()->addRect" rect
						 (funcall QPen "Qt::green" 4 "Qt::SolidLine"
							  "Qt::FlatCap"
							  "Qt::MiterJoin")))))))
			      (funcall moveLineToCenter (funcall value.toPointF))
			      (if text
				  (let ((s :type QString)
					(st :type QTextStream :ctor &s))
				    (funcall st.setFieldWidth 4)
				    (funcall st.setFieldAlignment "QTextStream::AlignCenter")
				    (funcall st.setPadChar (char #\_))
				    (<< st (funcall "value.toPointF().x")
					(funcall "value.toPointF().y"))
				    
				    (funcall text->setPlainText s)
				    (funcall moveTextToCenter (funcall value.toPointF))))
			      
			      ))
			 (return (funcall "QGraphicsItem::itemChange" change value)))
	       	       
	       (access-specifier private)
	       (raw "//! Update one of the two points of the line. The bool first_point_p chooses the point.")
	       (function (moveLineToCenter ((newPos :type QPointF)) void)
			 (let ((p1 :init (? first_point_p newPos "line->line().p1()"))
			       (p2 :init (? first_point_p "line->line().p2()" newPos)))
			   (funcall line->setLine (funcall QLineF p1 p2))))
	       (raw "//! Update the text label position.")
	       (function (moveTextToCenter ((newPos :type QPointF)) void)
			 (if text
			     (funcall text->setPos newPos)))
	       (decl ((line :type "QGraphicsLineItem*" :init nullptr)
		      (text :type "QGraphicsTextItem*" :init nullptr)
		      (first_point_p :type bool :init false))))
	

	(class CustomItemGridGroup ("public QGraphicsItemGroup")
	       (access-specifier public)
	       (function (CustomItemGridGroup ((dx :type int)
					   (dy :type int)
					   (nx :type int)
					   (ny :type int))
					  explicit
					  :ctor
					  ((m_dx dx)
					   (m_dy dy)
					  (m_nx nx)
					   (m_ny ny))
					;:parent-ctor
					;((QGraphicsRectItem rect))
					  )
			 (with-compilation-unit
			  (raw "// draw grid")
			 (let ((dx :init m_dx)
			      (dy :init m_dy)
			      (nx :init m_nx)
			      (ny :init m_ny))
			  (dotimes (i ny)
			    (let ((x1 :init (* dx (+ 1 i)))
				  (y1 :init (* dy 1))
				  (x2 :init x1)
				  (y2 :init (* dy ny)))
			      (funcall this->addToGroup (new (funcall QGraphicsLineItem (funcall QLineF x1 y1 x2 y2))))))
			  #+nil (dotimes (i nx)
			    (let ((y1 :init (* dy (+ 1 i)))
				  (x1 :init (* dx 1))
				  (y2 :init y1)
				  (x2 :init (* dx nx)))
			      (funcall "scene()->addLine" (funcall QLineF x1 y1 x2 y2))))
			  (raw "// highlight one rectangle")
			  #+nil (let ((i :init 4)
				(j :init 3)
				(eps :init -2))
			    (let ((y1 :init (- (* dy j) eps))
				  (x1 :init (- (* dx i) eps))
				  (y2 :init (+ (* dy (+ 1 j)) eps))
				  (x2 :init (+ (* dx (+ 1 i)) eps)))
			      (funcall "scene()->addRect" (funcall QRectF x1 y1 (- x2 x1) (- y2 y1))
				       (funcall QPen "Qt::red" 3 "Qt::SolidLine"
						"Qt::FlatCap"
						"Qt::MiterJoin")))))))
	     
	       (access-specifier private)
	       (decl ((m_dx :type "unsigned int")
		      (m_dy :type "unsigned int")
		      (m_nx :type "unsigned int")
		      (m_ny :type "unsigned int"))))
	
	(function (main ((argc :type int)
			 (argv :type char**))
			int)
		  (if (== 0 argc)
		      (return 0))
		  (if (== nullptr argv)
		      (return 0))
		  (let ((a :type QApplication :ctor (comma-list argc argv))
			(w :type QGraphicsView))
		    (funcall w.setAttribute "Qt::WA_TranslucentBackground" false)
		    #+nil (let ((tr :init (funcall QTransform)))
			    (funcall tr.rotate 45 "Qt::ZAxis")
			    (funcall w.setTransform tr))
		    ;; BoundingRectViewportUpdate
		    (let ((scene :init (new (funcall QGraphicsScene 0 0 300 300 &w))))
		      (funcall scene->setBackgroundBrush "Qt::white")
		      (funcall w.setScene scene)

		      #+nil (with-compilation-unit
			  (raw "// draw grid")
			(let ((dx :init 20)
			      (dy :init dx)
			      (nx :init 10)
			      (ny :init nx))
			  (dotimes (i ny)
			    (let ((x1 :init (* dx (+ 1 i)))
				  (y1 :init (* dy 1))
				  (x2 :init x1)
				  (y2 :init (* dy ny)))
			      (funcall scene->addLine (funcall QLineF x1 y1 x2 y2))))
			  (dotimes (i nx)
			    (let ((y1 :init (* dy (+ 1 i)))
				  (x1 :init (* dx 1))
				  (y2 :init y1)
				  (x2 :init (* dx nx)))
			      (funcall scene->addLine (funcall QLineF x1 y1 x2 y2))))
			  (raw "// highlight one rectangle")
			  (let ((i :init 4)
				(j :init 3)
				(eps :init -2))
			    (let ((y1 :init (- (* dy j) eps))
				  (x1 :init (- (* dx i) eps))
				  (y2 :init (+ (* dy (+ 1 j)) eps))
				  (x2 :init (+ (* dx (+ 1 i)) eps)))
			      (funcall scene->addRect (funcall QRectF x1 y1 (- x2 x1) (- y2 y1))
				       (funcall QPen "Qt::red" 3 "Qt::SolidLine"
						"Qt::FlatCap"
						"Qt::MiterJoin"))))))

		      (with-compilation-unit
			  (raw "// two handles to define the line")
			(let ((w :init 17.0)
			      (c :init (/ w -2.0))
			      (grid :init (new (funcall CustomItemGridGroup 20 20 10 10)))
			      (handle_center  :init (new (funcall CustomRectItem (funcall QRectF c c w w))))
			      (handle_periph :init (new (funcall CustomRectItem (funcall QRectF c c w w)))))
			  
			  
			  
			  (let ((line :init (funcall scene->addLine (funcall QLineF 40 40 80 80))))
			    (raw "// initiate the line to some random ")
			    (funcall handle_center->addLine line true)
			    (funcall handle_periph->addLine line false))

			  
			  (funcall scene->addItem grid)
			  (funcall scene->addItem handle_center)
			  (funcall scene->addItem handle_periph)
			  
			  (funcall handle_center->addLabel)
			  (funcall handle_periph->addLabel)

			  
			  (raw "// change position of handles now, so that the line is redrawn by CustomRect::itemChange")
			  
			  (funcall handle_center->setPos 150 150)
			  (funcall handle_periph->setPos 130 280)
			  
			  
			  
			  #+nil (let ((grp :init (new (funcall QGraphicsItemGroup rect))))
				  (funcall grp->addToGroup rect)
				  (funcall grp->addToGroup rect2)
				  (funcall scene->addItem grp))
			  #+nil (let ((ql :type QList<QGraphicsItem*> :ctor (list rect rect2))
				      (grp :init (funcall scene->createItemGroup ql)))
				  )
			  (let ((tr :init (funcall QTransform)))
			    (funcall tr.rotate 45 "Qt::ZAxis")
			    (funcall handle_center->setTransform tr))
			  
			  (funcall scene->addText (string "hello")))))
		    (funcall w.show)
		    
		    (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))



