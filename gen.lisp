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

;; http://www.qtcentre.org/threads/6929-Any-fast-way(-lt-10ms)-to-display-640*480-QImage-on-QGraphicsScene
;; 26th March 2011, 17:04

;; http://stackoverflow.com/questions/12841170/how-to-define-a-2d-array-in-c-and-stl-without-memory-manipulation
;; std::array<std::array<int,3>,2> a {{
;;     {{1,2,3}},
;;     {{4,5,6}}
;; }};

(defparameter *file-hashes* (make-hash-table))


(defun write-source (name extension code)
  (let* ((fn (merge-pathnames (format nil "stage/cl-gen-qt-thing/source/~a.~a" name extension)
			     (user-homedir-pathname)))
	(code-str (emit-cpp
		   :clear-env t
		   :code code))
	(fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
     (when (or (not exists) (/= code-hash old-code-hash))
       ;; store the sxhash of the c source in the hash table
       ;; *file-hashes* with the key formed by the sxhash of the full
       ;; pathname
       (setf (gethash fn-hash *file-hashes*) code-hash)
       (with-open-file (s fn
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
	 (write-sequence code-str s))
       (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring fn)))))))

(defun function-declarations (ls)
  (loop for e in ls collect
       (let ((name (getf e :name))
	     (params (getf e :params))
	     (ret (getf e :ret))
	     (ctor (getf e :ctor))
	     (specifier (getf e :specifier))
	     (parent-ctor (getf e :parent-ctor))
	     )
	 `(function (,name ,params ,ret :ctor ,ctor :specifier ,specifier :parent-ctor ,parent-ctor)))))


(progn
  (defparameter *main-cpp-filename*  (merge-pathnames "stage/cl-gen-qt-thing/source/main.cpp"
						      (user-homedir-pathname)))
  (let* ((img-type "std::array<std::array<std::array<unsigned char,IMG_H>,IMG_W>,IMG_C>")
	 (img-type-ptr (format nil "~a*" img-type))
	 (img-type-ref (format nil "~a&" img-type))
	 (code `(with-compilation-unit
		   
		   (include <CustomLineItem.h>)
		 (include <CustomRectItem.h>)
		 (include <QGraphicsScene>)
		 (include <QDebug>)
		 (include <assert.h>)

		 (include <sstream>)
		 (include <iomanip>)
		 ;(include <iostream>)
		 
		 
		 (include <utility>)
		 (include <vector>)
		 (include <QVector2D>)

		 
		 
		 (function ("CustomLineItem::createPPMHeader" (
							       (image :type ,img-type-ref)
					;(w :type int) (h :type int)
							       )
							      int)
			   (funcall m_ppm_data.fill 0)
			   (let ((colors :init (funcall image.size))
				 (w :init (funcall (slot-value (aref image 0) size)))
				 (h :init (funcall (slot-value (aref image 0 0) size))))
			     (funcall assert (<= w (* DX (- NX 1))))
			     (funcall assert (<= h (* DY (- NY 1))))
			     (funcall assert (<= w 9999))
			     (funcall assert (<= h 9999))
			     (funcall assert (== 3 colors))
			     ;; "P6  580  580 255 "
			     (let ((oss :type "std::ostringstream"))
			       (<< oss
				   (string "P6 ")
				   (funcall "std::setw" 4) w (string " ")
				   (funcall "std::setw" 4) h (string " ")
				   (funcall "std::setw" 3) 255 (string " "))
			       
			       (let ((i0 :init 0)
				     )
				 (for-range ((c :type "const auto") (funcall oss.str)) 
					    (setf (aref m_ppm_data i0) (funcall "static_cast<unsigned char>" c))
					    (+= i0 1))
				 (let ((sum :init i0))
				   (dotimes (j h)
				    (dotimes (i w)
				      (dotimes (k colors)
					(setf (aref m_ppm_data (+ i0 k (* 3 (+ (* w j) i)))) (aref image k i j))
					(+= sum 1))))
				  (return sum))))))

		 (function ("CustomLineItem::updatePixmapFromImage" ((image :type ,img-type-ref
									    )) void)
			   (let    ((colors :init (funcall image.size))
				 (w :init (funcall (slot-value (aref image 0) size)))
				 (h :init (funcall (slot-value (aref image 0 0) size))))
			     #+nil ((w :init (* DX (- NX 1)))
				    (h :init (* DY (- NY 1)))
				    
				    #+nil (pixmap :type QPixmap ; :ctor (comma-list w h)
					    ))
			     (let ((n :ctor (funcall createPPMHeader image)))
			       (funcall assert (funcall m_pixmap->loadFromData (funcall m_ppm_data.data) n ;(funcall m_ppm_data.size)
							(string "PPM")
							)))
			     (funcall m_pixmap_item->setPixmap *m_pixmap))

			   ;(<< (funcall qDebug) (string "thread id ") (funcall "QThread::currentThreadId"))
			   ;(<< "std::cout"  (string "updatePixmapFromImage") "std::endl")
			   )
		 
		 (function ("CustomLineItem::CustomLineItem" ((line :type "const QLineF&"))
							     nil 
							     :parent-ctor
							     ((QGraphicsLineItem line)))
			   (raw "// the order of initialization is important so that items are layered correctly. Unfortunately it is not possible to bring the line  (this object) above the pixmap from within this constructor. Instead I have to change the parents after object creation")
			   (setf m_pixmap_item (new (funcall QGraphicsPixmapItem this))
				 m_pixmap (new (funcall QPixmap IMG_W IMG_H)))
			   
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
			     (funcall m_p2->setPos (funcall line.p2))
			     )
			   #+nil (let ((image :type ,img-type))
			    (dotimes (j IMG_H)
			      (dotimes (i IMG_W)
				(let nil
				    #+nil ((v :init (funcall getDistanceFromPoint (funcall QPointF
										     (+ i .5s0)
										     (+ j .5s0))))
				     (vu :init (funcall "static_cast<unsigned char>" v)))
				  (setf (aref image 0 i j) 255
					(aref image 1 i j) (- 255 i)
					(aref image 2 i j) 255))))
			    
			    (funcall updatePixmapFromImage image))
			   

			   (let ((pos :type "std::vector<std::pair<int,int> >" :init (list (list 1 1)
											   (list 2 2)
											   (list 2 3))))
			     
			     (setf m_pixels (new (funcall CustomItemPixelsGroup DX DY NX NX pos this))))
			   
			   )
		 #+nil (function ("CustomLineItem::itemChange" ((change :type GraphicsItemChange)
							  (value :type "const QVariant&")) QVariant)
					;(<< (funcall qDebug) (string "change customLine ") (funcall this->pos) (string " ") value)
			   (if (&& (== ItemPositionChange change)
				   (funcall scene))
			       (statements
				(raw "// value is the same as pos()")
				(<< (funcall qDebug) (string "change pos customLine ") (funcall this->pos) (string " ") value)))
			   (return (funcall "QGraphicsItem::itemChange" change value)))
		 (function ("CustomLineItem::getPixels" () CustomItemPixelsGroup*)
			   (return m_pixels))
		 #+nil (function ("CustomLineItem::getImage" () ,img-type-ptr)
			   (return &m_image))
		 (function ("CustomLineItem::setPixels" ((vecs :type "std::vector<std::pair<int,int> >")) void)
			   (let ,(loop for e in '(dx dy nx ny) collect `(,e :init (funcall ,(format nil "m_pixels->~a" e))))
			     (delete m_pixels)
			     (setf m_pixels (new (funcall CustomItemPixelsGroup
							  dx dy nx ny
							  vecs this)))))
		 (raw "//! Computes distance from a point p0 to the line through the points m_p1 and m_p2. Make sure m_p{1,2}->pos are initialized correctly before calling this function.")
		 (function ("CustomLineItem::getDistanceFromPoint" (;(line :type QLineF)
						  (p0 :type QPointF))
						 float)
			   (raw "// https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line vector formulation")
			   (let ((p1 :ctor (funcall m_p1->pos) ;(funcall line.p1)
				   )
				 (p2 :ctor (funcall m_p2->pos) ;(funcall line.p2)
				   )
				 (n_len :init (funcall QVector2D (- p1 p2)))
				 (n :ctor (funcall (slot-value n_len  normalized)))
				 (a_p :ctor (funcall QVector2D (- p1 p0)))
				 (a_p_dot_n :ctor (funcall "QVector2D::dotProduct" a_p n))
				 )
			     (return (funcall (slot-value (- a_p (* a_p_dot_n n))
							  length)))))
		 (function ("CustomLineItem::getImageItem" () QGraphicsPixmapItem*)
			   (return m_pixmap_item))))
	 (header `(with-compilation-unit
		     (raw "#pragma once")
		   (include <QtCore>)
		   (include <QGraphicsItem>)
		   (include <CustomItemPixelsGroup.h>)
		   (raw "class CustomRectItem;")
		   (enum Coord
			(DX 20)
			(DY 20)
			(NX 10)
			(NY 10)
			(IMG_C 3)
			(IMG_W (* DX (- NX 1)))
			(IMG_H (* DY (- NY 1)))
			(PPM_IMAGE_BYTES (* IMG_C IMG_W IMG_H))
			(PPM_HEADER_LENGTH ,(length (let ((w 1024)
							  (h 1280)
							  (c 255))
						      (format nil "P6 ~4d ~4d ~3d " w h c)))))
		   (class CustomLineItem ("public QGraphicsLineItem")
			  (access-specifier public)
			  (function (CustomLineItem ((line :type "const QLineF&"))  explicit))
			  #+nil (function (itemChange ((change :type GraphicsItemChange)
						 (value :type "const QVariant&")) QVariant))
			  (function (getPixels () CustomItemPixelsGroup*))
			  ;(function (getImage () ,img-type-ptr))
			  (function (getImageItem () QGraphicsPixmapItem*))
			  (function (setPixels ((vecs :type "std::vector<std::pair<int,int> >")) void))
			  (function (updatePixmapFromImage ((image :type ,(format nil "~a&" img-type))) void))
			  (function (getDistanceFromPoint ((p0 :type QPointF))
						 float))
			  (access-specifier private)
			  (function (createPPMHeader ((image :type ,img-type-ref)
						      )
						     int))
			  (decl ((m_p1 :type "CustomRectItem*" :init nullptr )
				 (m_p2 :type "CustomRectItem*" :init nullptr )
				 (m_pixels :type "CustomItemPixelsGroup*" :init nullptr )
				 (m_pixmap_item :type "QGraphicsPixmapItem*" :init nullptr)
				 (m_pixmap :type "QPixmap*" :init nullptr)
				 (m_ppm_data 
				  :type ,(format nil "std::array<unsigned char,~a>"
						 (emit-cpp :code `(+ PPM_HEADER_LENGTH 
								     PPM_IMAGE_BYTES))))
				 ;(m_image :type ,img-type)
				 )))
		   )))
    (write-source "CustomLineItem" "h" header)
    (write-source "CustomLineItem" "cpp" code))

  (let ((code `(with-compilation-unit
		   (include <CustomRectItem.h>)
		 (include <QGraphicsScene>)
		 (include <CustomLineItem.h>)
		 ;(include <iostream>)
		 (function ("CustomRectItem::CustomRectItem" ((rect :type "const QRectF&")
							      (parent :type QGraphicsItem*)
							      (line :type CustomLineItem*)
							      (first_point_p :type bool)
							      )
							     nil
							     :ctor ((m_line line)
								    (m_first_point_p first_point_p))
							     :parent-ctor ((QGraphicsRectItem rect parent)))
			   (funcall this->setFlag "QGraphicsItem::ItemIsMovable")
			   (funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges"))

		 

		 
		 (function ("CustomRectItem::itemChange" ((change :type GraphicsItemChange)
							  (value :type "const QVariant&")) QVariant)
			   #+nil (<< (funcall qDebug) (string "change rect ") (funcall this->pos) (string " ")  (string " ") value)
			   (if (&& (== ItemPositionChange change)
				   (funcall scene))
			       (statements
				(funcall moveLineToCenter (funcall value.toPointF))
				(funcall "m_line->scene()->removeItem" (funcall m_line->getPixels))
				(let ((pos :type "std::vector<std::pair<int,int> >" )
				      (line :init "m_line->line()")
				      ,@(loop for e in '(dx dy nx ny) collect
					     `(,e :type int :init (funcall (slot->value "m_line->getPixels()" ,e)))))
				  (dotimes (j (- ny 1))
				    (dotimes (i (- nx 1))
				      (if (<
					   (funcall fabsf
						    (funcall m_line->getDistanceFromPoint (funcall QPointF
												(* dx (+ i .5s0))
												(* dy (+ j .5s0)))))
					   (* 1.0s0 (funcall sqrtf (* dx dy))))
					  (statements
					   (funcall pos.push_back (funcall "std::make_pair" i j))))))
				  (funcall m_line->setPixels pos)
				  (let ((img :type  "static std::array<std::array<std::array<unsigned char,IMG_H>,IMG_W>,IMG_C>" ;:ctor (deref (funcall m_line->getImage))
					     ))
				   (dotimes (j IMG_H)
				     (dotimes (i IMG_W)
				       (let
					((v :init (funcall m_line->getDistanceFromPoint (funcall QPointF
												       (+ i .5s0)
												       (+ j .5s0))))
					 (v2 :init (* v v))
					 (vu :init (funcall "static_cast<unsigned char>" (? (< v2 100.0) v2 100))))
					(setf (aref img 0 i j) 255-vu
					      (aref img 1 i j) 255-vu
					      (aref img 2 i j) 255-vu))))
				   ;; (<< "std::cout"  (string "rect :: item change") "std::endl")
				   (funcall m_line->updatePixmapFromImage img)
				   )
				  )))
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
			  (decl ((m_line :type "CustomLineItem*"  :init nullptr)
					;(m_text :type "QGraphicsTextItem*" :init nullptr)
					;(m_pixels :type "CustomItemPixelsGroup*" :init nullptr)
				 (m_first_point_p :type bool  :init false
						  )))))))
    (write-source "CustomRectItem" "h" header)
    (write-source "CustomRectItem" "cpp" code))

  (let ((code `(with-compilation-unit
		   (include <CustomItemPixelsGroup.h>)
		 (include <QGraphicsRectItem>)
		 (function ("CustomItemPixelsGroup::CustomItemPixelsGroup" ((dx :type int)
									    (dy :type int)
									    (nx :type int)
									    (ny :type int)
									    (vecs :type "std::vector<std::pair<int,int> >")
									    (parent :type QGraphicsItem*))
									   nil
									   :ctor
									   ((m_dx dx)
									    (m_dy dy)
									    (m_nx nx)
									    (m_ny ny))
									   :parent-ctor ((QGraphicsItemGroup parent)))
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
		 ,@(loop for e in '(dx dy nx ny) collect
		      `(function (,(format nil "CustomItemPixelsGroup::~a" e) () int)
				(return ,(format nil "m_~a" e))))))
	(header `(with-compilation-unit
		     (raw "#pragma once")

		   
		   (include <QGraphicsItemGroup>)
		   (include <vector>)
		   (include <utility>)
		   (class CustomItemPixelsGroup ("public QGraphicsItemGroup")
	       (access-specifier public)
	       (function (CustomItemPixelsGroup ((dx :type int)
						 (dy :type int)
						 (nx :type int)
						 (ny :type int)
						 (vecs :type "std::vector<std::pair<int,int> >")
						 (parent :type QGraphicsItem*))
						explicit))
	       ,@(loop for e in '(dx dy nx ny) collect
		      `(function (,(format nil "~a" e) () int)))
	       (access-specifier private)
	       (decl ((m_dx :type "unsigned int")
		      (m_dy :type "unsigned int")
		      (m_nx :type "unsigned int")
		      (m_ny :type "unsigned int")))))))
    (write-source "CustomItemPixelsGroup" "h" header)
    (write-source "CustomItemPixelsGroup" "cpp" code))

  (let ((code `(with-compilation-unit
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
			       (let ((x1 :init (* dx i))
				     (y1 :init (* dy 0))
				     (x2 :init x1)
				     (y2 :init (* dy (- ny 1))))
				 (funcall this->addToGroup (new (funcall QGraphicsLineItem (funcall QLineF x1 y1 x2 y2))))))
			     (dotimes (i nx)
			       (let ((y1 :init (* dy i))
				     (x1 :init (* dx 0))
				     (y2 :init y1)
				     (x2 :init (* dx (- nx 1))))
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
		  ;(<< (funcall qDebug) (string "main thread id ") (funcall "QThread::currentThreadId"))
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
			  
			  
			  
			  (let ((line :init (new (funcall CustomLineItem (funcall QLineF 40 40 80 80)))))
			    (raw "// the following reasignment of parents is required for the line to be drawn on top of the pixmap")
			    (funcall "line->getImageItem()->setParentItem" nullptr)
			    (funcall line->setParentItem (funcall line->getImageItem))
			    (funcall scene->addItem (funcall line->getImageItem))
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
		    
		    (return (funcall a.exec)))))
	  ))
    (write-source "main" "cpp" code))
  )



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
