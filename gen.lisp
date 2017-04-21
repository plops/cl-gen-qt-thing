(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator))

;; qtcreator can open the cmake file
;; https://stackoverflow.com/questions/10591635/can-i-get-mouse-events-in-a-qgraphicsitem
;; compile small qt
;; http://stackoverflow.com/questions/41812121/qt-lite-and-configuration-changes-in-qt-5-8
;; configure -list-features

;; event propagation between graphicsitems
;; http://stackoverflow.com/questions/10590881/events-and-signals-in-qts-qgraphicsitem-how-is-this-supposed-to-work

;; http://stackoverflow.com/questions/4922801/adding-signals-slots-qobject-to-qgraphicsitem-performance-hit
;; qt-graphics-view-in-depth https://www.youtube.com/watch?v=s3sHIheBe94

;; ttps://woboq.com/blog/how-qt-signals-slots-work.html
(in-package :cl-cpp-generator)

(defmacro e (&body body)
  `(statements (<< "std::cout" ,@(loop for e in body collect
				      (cond ((stringp e) `(string ,e))
					    (t e))) "std::endl")))


;; https://github.com/mjspncr/lzz3

(defun write-source (name extension code)
  (let ((fn (merge-pathnames (format nil "stage/cl-gen-qt-thing/source/~a.~a" name extension)
			     (user-homedir-pathname))))
    (with-open-file (s fn
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (emit-cpp
       :str s
       :clear-env t
       :code 
       code))
    (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring fn)))))

(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main.cpp"
						      (user-homedir-pathname)))
  (let (
	(code `(with-compilation-unit
		  ; (include <QApplication>)
		 ;(include <QGraphicsScene>)
		 ;(include <QGraphicsView>)
		 ;(include <QGraphicsItem>)
		 ;(include <QGraphicsRectItem>)
		 (include <CustomLineItem.h>)
		 (include <CustomRectItem.h>)
		 ;(include <QGraphicsLineItem>)
		 ;(include <QGraphicsTextItem>)
		 ;(include <QGraphicsItemGroup>)
		 (include <QDebug>)
		 ;(include <vector>)
		 ;(include <utility>)

		 (function ("CustomLineItem::CustomLineItem" ((line :type "const QLineF&"))
							     nil 
							     :parent-ctor
							     ((QGraphicsLineItem line)))
			   (let ((w :init 17)
				 (h :init w))
			     (setf m_p1 (new (funcall CustomRectItem
						      ;; upper left corner and size
						      (funcall QRectF (* -.5 (funcall QPointF w h))
							       (funcall QSizeF w h))
						      this
						      this
						      true)))
			     (funcall m_p1->setPos (funcall line.p1))
			     (setf m_p2 (new (funcall CustomRectItem
						      (funcall QRectF (* -.5 (funcall QPointF w h))
							       (funcall QSizeF w h))
						      this
						      this
						      false)))
			     (funcall m_p2->setPos (funcall line.p2))))
		 (function ("CustomLineItem::itemChange" ((change :type GraphicsItemChange)
							  (value :type "const QVariant&")) QVariant)
			   (<< (funcall qDebug) (string "change customLine ") (funcall this->pos) (string " ") value)
			   (if (&& (== ItemPositionChange change)
				   (funcall scene))
			       (statements
				(raw "// value is the same as pos()")
				(<< (funcall qDebug) (string "change pos customLine ") (funcall this->pos) (string " ") value)))
			   (return (funcall "QGraphicsItem::itemChange" change value)))))
	(header `(with-compilation-unit
		     (raw "#pragma once")
		   (include <QtCore>)
		   (include <QGraphicsItem>)
		   (raw "class CustomRectItem;")
		     (class CustomLineItem ("public QGraphicsLineItem")
		    (access-specifier public)
		    (function (CustomLineItem ((line :type "const QLineF&"))
					      explicit
					      ;:parent-ctor
					      ;((QGraphicsLineItem line))
					      )
			      )
		      (function (itemChange ((change :type GraphicsItemChange)
						 (value :type "const QVariant&")) QVariant)
				)
		    (access-specifier private)
		    (decl ((m_p1 :type "CustomRectItem*" :init nullptr )
			   (m_p2 :type "CustomRectItem*" :init nullptr )))))))
    (write-source "CustomLineItem" "h" header)
    (write-source "CustomLineItem" "cpp" code)
    )

  (let (
	(code `(with-compilation-unit
		   ;; (include <QApplication>)
		 ;; (include <QGraphicsScene>)
		 ;; (include <QGraphicsView>)
		 ;; (include <QGraphicsItem>)
		 ;; (include <QGraphicsRectItem>)
		 ;; (include <CustomLineItem.h>)
		    (include <CustomRectItem.h>)
		 ;; (include <QGraphicsLineItem>)
		 ;; (include <QGraphicsTextItem>)
		 ;; (include <QGraphicsItemGroup>)
		 ;; (include <QDebug>)
		 ;; (include <vector>)
		 ;; (include <utility>)

		   (function ("CustomRectItem::CustomRectItem" ((rect :type "const QRectF&")
								(parent :type QGraphicsItem*)
								(line :type CustomLineItem*)
								(first_point_p :type bool))
							       nil
							       :ctor ((m_line line)
								      (m_first_point_p first_point_p))
							       :parent-ctor
							       ((QGraphicsRectItem rect parent)))
		    (funcall this->setFlag "QGraphicsItem::ItemIsMovable")
		    (funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges"))
		 (function ("CustomRectItem::itemChange" ((change :type GraphicsItemChange)
							  (value :type "const QVariant&")) QVariant)
			   #+nil (<< (funcall qDebug) (string "change rect ") (funcall this->pos) (string " ")  (string " ") value)
			   (if (&& (== ItemPositionChange change)
				   (funcall scene))
			       (statements
				(funcall moveLineToCenter (funcall value.toPointF))))
			   (return (funcall "QGraphicsItem::itemChange" change value)))
		 (function ("CustomRectItem::moveLineToCenter" ((newPos :type QPointF)) void)
			   (let ((p1 :init (? m_first_point_p newPos "m_line->line().p1()"))
				 (p2 :init (? m_first_point_p "m_line->line().p2()" newPos)))
			     (funcall "m_line->setLine" (funcall QLineF p1 p2))))))
	(header `(with-compilation-unit
		     (raw "#pragma once")
		   (include <QtCore>)

		   (include <QGraphicsItem>)
		   (include <CustomLineItem.h>)
		   (class CustomRectItem ("public QGraphicsRectItem")
			(access-specifier public)
			(function (CustomRectItem ((rect :type "const QRectF&")
						   (parent :type QGraphicsItem*)
						   (line :type CustomLineItem*)
						   (first_point_p :type bool))
						  explicit))
			(access-specifier private)
			(function (itemChange ((change :type GraphicsItemChange)
					       (value :type "const QVariant&")) QVariant))
			(function (moveLineToCenter ((newPos :type QPointF)) void))
			(decl ((m_line :type "CustomLineItem*"  :init nullptr
				       )
					;(m_text :type "QGraphicsTextItem*" :init nullptr)
					;(m_pixels :type "CustomItemPixelsGroup*" :init nullptr)
			       (m_first_point_p :type bool  :init false
						)))))))
    (write-source "CustomRectItem" "h" header)
    (write-source "CustomRectItem" "cpp" code))
  
  (with-open-file (s *main-cpp-filename*
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (emit-cpp
     :str s
     :clear-env t
     :code 
     `(with-compilation-unit
					;(include <QtGui>)
					; (include "main_win.h")
	  (include <QtCore>)
	(include <QApplication>)
	(include <QGraphicsScene>)
	(include <QGraphicsView>)
	(include <QGraphicsItem>)
	;(include <QGraphicsRectItem>)
	;(include <CustomRectItem.h>)
	(include <CustomLineItem.h>)
	;(include <QGraphicsLineItem>)
	;(include <QGraphicsTextItem>)
	;(include <QGraphicsItemGroup>)
					;(include <QDebug>)
	(include <vector>)
	(include <utility>)
	
	(raw "//! This program displays a line on a canvas. The parameters of the line can be adjusted with two control points. The canvas also displays a grid of square pixels and highlights the pixels that are intersected by the line.

")

	(class CustomItemPixelsGroup ("public QGraphicsItemGroup")
	       (access-specifier public)
	       (function (CustomItemPixelsGroup ((dx :type int)
						 (dy :type int)
						 (nx :type int)
						 (ny :type int)
						 (vecs :type "std::vector<std::pair<int,int> >"))
						explicit
						:ctor
						((m_dx dx)
						 (m_dy dy)
						 (m_nx nx)
						 (m_ny ny)))
			 (with-compilation-unit
			     (let ((dx :init m_dx)
				   (dy :init m_dy)
				   (nx :init m_nx)
				   (ny :init m_ny))
			       (for-range (v vecs)
					  (let ((i :init "v.first")
						(j :init "v.second")
						(eps :init -2))
					    (let ((y1 :init (- (* dy j) eps))
						  (x1 :init (- (* dx i) eps))
						  (y2 :init (+ (* dy (+ 1 j)) eps))
						  (x2 :init (+ (* dx (+ 1 i)) eps)))
					      (funcall "this->addToGroup" (new (funcall QGraphicsRectItem
											(funcall QRectF x1 y1 (- x2 x1) (- y2 y1))
											#+nil  (funcall QPen "Qt::red" 3 "Qt::SolidLine"
													"Qt::FlatCap"
													"Qt::MiterJoin"))))))))))
	       
	       (access-specifier private)
	       (decl ((m_dx :type "unsigned int")
		      (m_dy :type "unsigned int")
		      (m_nx :type "unsigned int")
		      (m_ny :type "unsigned int"))))


	
	
	
	

	
	
	
	

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
			     (dotimes (i nx)
			       (let ((y1 :init (* dy (+ 1 i)))
				     (x1 :init (* dx 1))
				     (y2 :init y1)
				     (x2 :init (* dx nx)))
				 (funcall this->addToGroup (new (funcall QGraphicsLineItem (funcall QLineF x1 y1 x2 y2)))))))))
	       
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
			      #+nil (pos :type "std::vector<std::pair<int,int> >" :init (list (list 1 1)
											      (list 2 2)
											      (list 2 3)))
					;(pixels :init (new (funcall CustomItemPixelsGroup 20 20 10 10 pos)))
					;(handle_center  :init (new (funcall CustomRectItem (funcall QRectF c c w w))))
					;(handle_periph :init (new (funcall CustomRectItem (funcall QRectF c c w w))))
			      )
			  
			  
			  
			  (let ((line :init (new (funcall CustomLineItem (funcall QLineF 40 40 80 80)))#+nil (funcall scene->addLine (funcall QLineF 40 40 80 80))))
			    (funcall scene->addItem line)
			    (raw "// initiate the line to some random ")
					;(funcall handle_center->addLine line true)
					;(funcall handle_periph->addLine line false)
			    )

			  
			  (funcall scene->addItem grid)
					;(funcall scene->addItem pixels)
					;(funcall scene->addItem handle_center)
					;(funcall scene->addItem handle_periph)
			  
					;(funcall handle_center->addLabel)
					;(funcall handle_periph->addLabel)

			  
			  (raw "// change position of handles now, so that the line is redrawn by CustomRect::itemChange")
			  
					;(funcall handle_center->setPos 150 150)
					;(funcall handle_periph->setPos 130 280)
			  
			  
			  
			  #+nil (let ((grp :init (new (funcall QGraphicsItemGroup rect))))
				  (funcall grp->addToGroup rect)
				  (funcall grp->addToGroup rect2)
				  (funcall scene->addItem grp))
			  #+nil (let ((ql :type QList<QGraphicsItem*> :ctor (list rect rect2))
				      (grp :init (funcall scene->createItemGroup ql)))
				  )
			  #+nil (let ((tr :init (funcall QTransform)))
				  (funcall tr.rotate 45 "Qt::ZAxis")
				  (funcall handle_center->setTransform tr))
			  
			  (funcall scene->addText (string "hello")))))
		    (funcall w.show)
		    
		    (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))



#+nil (class CustomRectItem ("public QGraphicsRectItem")
			      (access-specifier public)
			      (function (CustomRectItem ((rect :type "const QRectF&")
							 )
							explicit
							:parent-ctor
							((QGraphicsRectItem rect)))
					(funcall this->setFlag "QGraphicsItem::ItemIsMovable")
					;(funcall this->setFlag "QGraphicsItem::ItemSendsGeometryChanges")
					(funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges"))
			      (raw "//! Attach a line to the control point. 

//! For each line this should be called twice for two different instances of CustomRectItem and different second parameter. Call addLine before adding the CustomRectItem to the scene. This ensures that the line coordinates are in a consistent state.")
			      (function (addLine ((line :type CustomLineItem*)
						  (is_first_point_p :type bool)) void)
					(setf m_line line
					      m_first_point_p is_first_point_p))
			      (raw "//! Display a label below the control point. 

//! Call addLabel before setPos. This ensures that the point coordinates are displayed correctly.")
			      (function (addLabel ()
						  void)
					(setf m_text (new (funcall QGraphicsTextItem)))
					(funcall m_text->setPos (funcall this->pos))
					(funcall m_text->setPlainText (string "Barev"))
					(funcall "this->scene()->addItem" m_text))
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
					     (funcall moveLineToCenter (funcall value.toPointF))
					     #+nil (funcall updatePixels)
					     (if m_text
						 (let ((s :type QString)
						       (st :type QTextStream :ctor &s))
						   (funcall st.setFieldWidth 4)
						   (funcall st.setFieldAlignment "QTextStream::AlignCenter")
						   (funcall st.setPadChar (char #\_))
						   (<< st (funcall "value.toPointF().x")
						       (funcall "value.toPointF().y"))
						   
						   (funcall m_text->setPlainText s)
						   (funcall moveTextToCenter (funcall value.toPointF))))
					     
					     ))
					(return (funcall "QGraphicsItem::itemChange" change value)))
			      
			      (access-specifier private)
			      (raw "//! Update one of the two points of the line. The bool m_first_point_p chooses the point.")
			      (function (moveLineToCenter ((newPos :type QPointF)) void)
					(let ((p1 :init (? m_first_point_p newPos "m_line->line().p1()"))
					      (p2 :init (? m_first_point_p "m_line->line().p2()" newPos)))
					  (funcall m_line->setLine (funcall QLineF p1 p2))))
			      (raw "//! Update the text label position.")
			      (function (moveTextToCenter ((newPos :type QPointF)) void)
					(if m_text
					    (funcall m_text->setPos newPos)))
			      (raw "//! Update the list of highlighted pixels. Call this aftes moveLineToCenter.")
			      #+nil (function (updatePixels () void)
					      (if m_line
						  (statements
						   (let ((p1 :init "m_line->line().p1()")
							 (p2 :init "m_line->line().p2()")
							 (pos :type "std::vector<std::pair<int,int> >" :init (list (list 1 1)
														   (list 2 2)
														   (list 2 3))))
						     (if m_pixels
							 (statements
							  (funcall "this->scene()->removeItem" m_pixels)))
						     (setf m_pixels (new (funcall CustomItemPixelsGroup 20 20 10 10 pos)))
						     (funcall "this->scene()->addItem" m_pixels)))))
			      (decl ((m_line :type "CustomLineItem*" :init nullptr)
				     (m_text :type "QGraphicsTextItem*" :init nullptr)
					;(m_pixels :type "CustomItemPixelsGroup*" :init nullptr)
				     (m_first_point_p :type bool :init false))))
