#include <QApplication>
#include <QDebug>
#include <QGraphicsItemGroup>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsView>
class CustomView : public QGraphicsView {
protected:
  void mouseReleaseEvent(QMouseEvent *event) {
    (qDebug() << "Custom view mouse released.");
    QGraphicsView::mouseReleaseEvent(event);
  }
};

int main(int argc, char **argv) {
  if ((0 == argc)) {
    return 0;
  }

  if ((nullptr == argv)) {
    return 0;
  }

  {
    QApplication a(argc, argv);
    CustomView w;

    w.setAttribute(Qt::WA_TranslucentBackground, false);
    {
      auto tr = QTransform();

      tr.rotate(45, Qt::ZAxis);
      w.setTransform(tr);
    }

    {
      auto scene = new QGraphicsScene(0, 0, 300, 300, &w);

      scene->setBackgroundBrush(Qt::yellow);
      w.setScene(scene);
      {
        auto rect = new QGraphicsRectItem(0, 0, 9, 9);
        auto rect2 = new QGraphicsRectItem(0, 0, 9, 9);

        rect->setFlag(QGraphicsItem::ItemIsSelectable);
        rect->setPos(50, 50);
        rect2->setFlag(QGraphicsItem::ItemIsMovable);
        scene->addItem(rect);
        scene->addItem(rect2);
        {
          auto tr = QTransform();

          tr.rotate(45, Qt::ZAxis);
          rect2->setTransform(tr);
        }

        scene->addText("hello");
      }
    }

    w.show();
    return a.exec();
  }
}
