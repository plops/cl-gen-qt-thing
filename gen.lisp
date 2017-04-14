(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-cpp-generator)
  (ql:quickload :xmls))

;; qtcreator can open the cmake file
;; https://stackoverflow.com/questions/10591635/can-i-get-mouse-events-in-a-qgraphicsitem
;; compile small qt
;; http://stackoverflow.com/questions/41812121/qt-lite-and-configuration-changes-in-qt-5-8
;; configure -list-features
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

	 #+nil (class CustomView ("public QGraphicsView")
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
			  (funcall this->setFlag "QGraphicsItem::ItemSendsScenePositionChanges")
			  )
		(function (addLine ((line :type QGraphicsLineItem*)
				    (is_first_point_p :type bool)) void)
			  (setf this->line line
				first_point_p is_first_point_p))
		(function (addLabel (())))
		(access-specifier protected)
		(function (itemChange ((change :type GraphicsItemChange)
				       (value :type "const QVariant&")) QVariant)
			  ;; http://stackoverflow.com/questions/32192607/how-to-use-itemchange-from-qgraphicsitem-in-qt
			  ;(<< (funcall qDebug) (string "change ") (funcall this->pos) (string " ") value)
			  (if (&& (== ItemPositionChange change)
				  (funcall scene))
			      (statements
			       (raw "// value is the same as pos()") 
			       (funcall moveLineToCenter (funcall value.toPointF))
			       ))
			  (return (funcall "QGraphicsItem::itemChange" change value)))
		
		#+nil (function (mouseReleaseEvent ((event :type QGraphicsSceneMouseEvent*)) void)
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

	 ;(include "main.moc")
	 
       (function (main ((argc :type int)
			(argv :type char**))
		       int)
		 (if (== 0 argc)
		     (return 0))
		 (if (== nullptr argv)
		     (return 0))
		 (let ((a :type QApplication :ctor (comma-list argc argv))
		       (w :type QGraphicsView #+nil CustomView)
		       )
		  (funcall w.setAttribute "Qt::WA_TranslucentBackground" false)
		  (let ((tr :init (funcall QTransform)))
		    (funcall tr.rotate 45 "Qt::ZAxis")
		    (funcall w.setTransform tr))
		  ;; BoundingRectViewportUpdate
		  (let ((scene :init (new (funcall QGraphicsScene 0 0 300 300 &w))))
		    (funcall scene->setBackgroundBrush "Qt::lightGray")
		    (funcall w.setScene scene)
		    
		    (let ((w :init 17.0)
			  (c :init (/ w -2.0))
			  (handle_center  :init (new (funcall CustomRectItem (funcall QRectF c c w w))))
			  (handle_periph :init (new (funcall CustomRectItem (funcall QRectF c c w w)))))
		      
		      
		      
		      (let ((line :init (funcall scene->addLine (funcall QLineF 40 40 80 80))))
			(raw "// initiate the line to some random ")
			(funcall handle_center->addLine line true)
			(funcall handle_periph->addLine line false))

		      (funcall scene->addItem handle_center)
		      (funcall scene->addItem handle_periph)

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
		      (funcall scene->addText (string "hello"))))
		  (funcall w.show)
		  
		  (return (funcall a.exec)))))))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-cpp-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-win-h-filename*)))
  (sb-ext:run-program "/usr/bin/clang-format" (list "-i" (namestring *main-cpp-filename*))))





;; martin@martin-MS-7978:/dev/shm/qt-everywhere-opensource-src-5.9.0-beta$ for i in `cat featoff |cut -d " " -f 1 `;do echo "-no-feature-"$i;done|tr '\12' ' '
;; -no-feature-accessibility -no-feature-animation -no-feature-appstore-compliant -no-feature-bearermanagement -no-feature-big_codecs -no-feature-calendarwidget -no-feature-clipboard -no-feature-codecs -no-feature-colordialog -no-feature-columnview -no-feature-commandlineparser -no-feature-commandlinkbutton -no-feature-completer -no-feature-concurrent -no-feature-contextmenu -no-feature-cssparser -no-feature-cups -no-feature-cursor -no-feature-d3d12 -no-feature-datawidgetmapper -no-feature-datestring -no-feature-datetimeedit -no-feature-desktopservices -no-feature-dial -no-feature-dialog -no-feature-dialogbuttonbox -no-feature-dirmodel -no-feature-dockwidget -no-feature-dom -no-feature-draganddrop -no-feature-effects -no-feature-errormessage -no-feature-filedialog -no-feature-filesystemiterator -no-feature-filesystemmodel -no-feature-filesystemwatcher -no-feature-fontcombobox -no-feature-fontdialog -no-feature-formlayout -no-feature-freetype -no-feature-fscompleter -no-feature-ftp -no-feature-gestures -no-feature-graphicseffect -no-feature-groupbox -no-feature-highdpiscaling -no-feature-http -no-feature-iconv -no-feature-identityproxymodel -no-feature-im -no-feature-image_heuristic_mask -no-feature-image_text -no-feature-imageformat_bmp -no-feature-imageformat_jpeg -no-feature-imageformat_png -no-feature-imageformat_ppm -no-feature-imageformat_xbm -no-feature-imageformat_xpm -no-feature-imageformatplugin -no-feature-inputdialog -no-feature-itemmodel -no-feature-itemviews -no-feature-keysequenceedit -no-feature-label -no-feature-lcdnumber -no-feature-library -no-feature-lineedit -no-feature-listview -no-feature-listwidget -no-feature-localserver -no-feature-mdiarea -no-feature-menu -no-feature-menubar -no-feature-messagebox -no-feature-mimetype -no-feature-movie -no-feature-networkdiskcache -no-feature-networkinterface -no-feature-networkproxy -no-feature-paint_debug -no-feature-pdf -no-feature-pepper-plugins -no-feature-picture -no-feature-printdialog -no-feature-printer -no-feature-printing-and-pdf -no-feature-printpreviewdialog -no-feature-printpreviewwidget -no-feature-process -no-feature-processenvironment -no-feature-progressbar -no-feature-progressdialog -no-feature-properties -no-feature-proprietary-codecs -no-feature-proxymodel -no-feature-pushbutton -no-feature-qml-interpreter -no-feature-qml-network -no-feature-qml-profiler -no-feature-quick-animatedimage -no-feature-quick-canvas -no-feature-quick-designer -no-feature-quick-flipable -no-feature-quick-gridview -no-feature-quick-listview -no-feature-quick-particles -no-feature-quick-path -no-feature-quick-pathview -no-feature-quick-positioners -no-feature-quick-shadereffect -no-feature-quick-sprite -no-feature-quickcontrols2-material -no-feature-quickcontrols2-universal -no-feature-quicktemplates2-hover -no-feature-regularexpression -no-feature-resizehandler -no-feature-rubberband -no-feature-scroller -no-feature-sessionmanager -no-feature-settings -no-feature-sha3-fast -no-feature-sharedmemory -no-feature-shortcut -no-feature-sizegrip -no-feature-socks5 -no-feature-sortfilterproxymodel -no-feature-spellchecker -no-feature-splashscreen -no-feature-splitter -no-feature-stackedwidget -no-feature-standarditemmodel -no-feature-statemachine -no-feature-statusbar -no-feature-statustip -no-feature-stringlistmodel -no-feature-style-stylesheet -no-feature-syntaxhighlighter -no-feature-systemsemaphore -no-feature-systemtrayicon -no-feature-tabbar -no-feature-tabletevent -no-feature-tableview -no-feature-tablewidget -no-feature-tabwidget -no-feature-temporaryfile -no-feature-textbrowser -no-feature-textcodec -no-feature-textdate -no-feature-textedit -no-feature-texthtmlparser -no-feature-textodfwriter -no-feature-timezone -no-feature-toolbar -no-feature-toolbox -no-feature-toolbutton -no-feature-tooltip -no-feature-topleveldomain -no-feature-translation -no-feature-treeview -no-feature-treewidget -no-feature-udpsocket -no-feature-undocommand -no-feature-undogroup -no-feature-undostack -no-feature-undoview -no-feature-validator -no-feature-webrtc -no-feature-whatsthis -no-feature-wheelevent -no-feature-widgettextcontrol -no-feature-wizard -no-feature-xml-schema -no-feature-xmlstream -no-feature-xmlstreamreader -no-feature-xmlstreamwriter
;;martin@martin-MS-7978:/dev/shm/qt-everywhere-opensource-src-5.9.0-beta$ for i in `cat featon |cut -d " " -f 1 `;do echo "-feature-"$i;done|tr '\12' ' '
;;-feature-abstractbutton -feature-abstractslider -feature-action -feature-buttongroup -feature-checkbox -feature-colornames -feature-combobox -feature-embedded -feature-graphicsview -feature-mainwindow -feature-radiobutton -feature-scrollarea -feature-scrollbar -feature-slider -feature-spinbox

;; ./configure -opensource -confirm-license -release -static -c++std  c++1z -no-dbus -no-cups  -no-zlib -qt-libjpeg -qt-libpng -no-alsa -no-pulseaudio -no-gstreamer -no-openssl -no-ico -no-iconv


;; ./configure -opensource -confirm-license -release -static -c++std  c++1z -no-dbus -no-cups  -no-zlib -qt-libjpeg -qt-libpng -no-alsa -no-pulseaudio -no-gstreamer -no-openssl -no-ico -no-iconv -feature-abstractbutton -feature-abstractslider -feature-action  -feature-checkbox -feature-colornames -feature-embedded -feature-graphicsview -feature-radiobutton -feature-scrollarea -feature-widgettextcontrol -feature-scrollbar -feature-slider  -no-feature-accessibility -no-feature-animation -no-feature-appstore-compliant -no-feature-bearermanagement -no-feature-big_codecs -no-feature-calendarwidget -no-feature-clipboard -no-feature-codecs -no-feature-colordialog -no-feature-columnview -no-feature-commandlineparser -no-feature-commandlinkbutton -no-feature-completer -no-feature-concurrent -no-feature-contextmenu -no-feature-cssparser -no-feature-cups -no-feature-cursor -no-feature-d3d12 -no-feature-datawidgetmapper -no-feature-datestring -no-feature-datetimeedit -no-feature-desktopservices -no-feature-dial -no-feature-dialog -no-feature-dialogbuttonbox -no-feature-dirmodel -no-feature-dockwidget -no-feature-dom -no-feature-draganddrop -no-feature-effects -no-feature-errormessage -no-feature-filedialog -no-feature-filesystemiterator -no-feature-filesystemmodel -no-feature-filesystemwatcher -no-feature-fontcombobox -no-feature-fontdialog -no-feature-formlayout -no-feature-freetype -no-feature-fscompleter -no-feature-ftp -no-feature-gestures -no-feature-graphicseffect -no-feature-groupbox -no-feature-highdpiscaling -no-feature-http -no-feature-iconv -no-feature-identityproxymodel -no-feature-im -no-feature-image_heuristic_mask -no-feature-image_text -no-feature-imageformat_bmp -no-feature-imageformat_jpeg -no-feature-imageformat_png -no-feature-imageformat_ppm -no-feature-imageformat_xbm -no-feature-imageformat_xpm -no-feature-imageformatplugin -no-feature-inputdialog -no-feature-itemmodel -no-feature-itemviews -no-feature-keysequenceedit -no-feature-label -no-feature-lcdnumber -no-feature-library -no-feature-lineedit -no-feature-listview -no-feature-listwidget -no-feature-localserver -no-feature-mdiarea -no-feature-menu -no-feature-menubar -no-feature-messagebox -no-feature-mimetype -no-feature-movie -no-feature-networkdiskcache -no-feature-networkinterface -no-feature-networkproxy -no-feature-paint_debug -no-feature-pdf -no-feature-pepper-plugins -no-feature-picture -no-feature-printdialog -no-feature-printer -no-feature-printing-and-pdf -no-feature-printpreviewdialog -no-feature-printpreviewwidget -no-feature-process -no-feature-processenvironment -no-feature-progressbar -no-feature-progressdialog -no-feature-properties -no-feature-proprietary-codecs -no-feature-proxymodel -no-feature-pushbutton -no-feature-qml-interpreter -no-feature-qml-network -no-feature-qml-profiler -no-feature-quick-animatedimage -no-feature-quick-canvas -no-feature-quick-designer -no-feature-quick-flipable -no-feature-quick-gridview -no-feature-quick-listview -no-feature-quick-particles -no-feature-quick-path -no-feature-quick-pathview -no-feature-quick-positioners -no-feature-quick-shadereffect -no-feature-quick-sprite -no-feature-quickcontrols2-material -no-feature-quickcontrols2-universal -no-feature-quicktemplates2-hover -no-feature-regularexpression -no-feature-resizehandler -no-feature-rubberband -no-feature-scroller -no-feature-sessionmanager -no-feature-settings -no-feature-sha3-fast -no-feature-sharedmemory -no-feature-shortcut -no-feature-sizegrip -no-feature-socks5 -no-feature-sortfilterproxymodel -no-feature-spellchecker -no-feature-splashscreen -no-feature-splitter -no-feature-stackedwidget -no-feature-standarditemmodel -no-feature-statemachine -no-feature-statusbar -no-feature-statustip -no-feature-stringlistmodel -no-feature-style-stylesheet -no-feature-syntaxhighlighter -no-feature-systemsemaphore -no-feature-systemtrayicon -no-feature-tabbar -no-feature-tabletevent -no-feature-tableview -no-feature-tablewidget -no-feature-tabwidget -no-feature-temporaryfile -no-feature-textbrowser -no-feature-textcodec -no-feature-textdate -no-feature-textedit -no-feature-texthtmlparser -no-feature-textodfwriter -no-feature-timezone -no-feature-toolbar -no-feature-toolbox -no-feature-toolbutton -no-feature-tooltip -no-feature-topleveldomain -no-feature-translation -no-feature-treeview -no-feature-treewidget -no-feature-udpsocket -no-feature-undocommand -no-feature-undogroup -no-feature-undostack -no-feature-undoview -no-feature-validator -no-feature-webrtc -no-feature-whatsthis -no-feature-wheelevent -no-feature-wizard -no-feature-xml-schema -no-feature-xmlstream -no-feature-xmlstreamreader -no-feature-xmlstreamwriter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: Also available for Linux: linux-clang linux-kcc linux-icc linux-cxx ;;
;; 									     ;;
;; Note: Using static linking will disable the use of dynamically	     ;;
;; loaded plugins. Make sure to import all needed static plugins,	     ;;
;; or compile needed modules into the library.				     ;;
;; 									     ;;
;; Note: Dropped compiler flags '-pthread' when detecting library 'glib'.    ;;
;; 									     ;;
;; WARNING: Accessibility disabled. This configuration of Qt is unsupported. ;;
;; 									     ;;
;; Qt is now configured for building. Just run 'make'.			     ;;
;; Once everything is built, you must run 'make install'.		     ;;
;; Qt will be installed into '/usr/local/Qt-5.9.0'.			     ;;
;; 									     ;;
;; Prior to reconfiguration, make sure you remove any leftovers from	     ;;
;; the previous build.							     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

