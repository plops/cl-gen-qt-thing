#include "main_win.h"
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

class CustomRectItem : public QObject, public QGraphicsRectItem {
  Q_OBJECT
public:
  explicit CustomRectItem(qreal x, qreal y, qreal w, qreal h,
                          QGraphicsItem *parent = nullptr)
      : QGraphicsRectItem(x, y, w, h, parent) {
    this->setFlag(QGraphicsItem::ItemSendsGeometryChanges);
    this->setFlag(QGraphicsItem::ItemIsMovable);
  }

protected:
  void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) {
    (qDebug() << "mouse released in " << this->pos());
    QGraphicsRectItem::mouseReleaseEvent(event);
  }

  QVariant itemChange(GraphicsItemChange change, const QVariant value) {
    (qDebug() << "item change");
    if ((QGraphicsItem::ItemPositionHasChanged == change)) {
      // value is the same as pos();
      (qDebug() << "item changed to " << value);
    }

    return QGraphicsItem::itemChange(change, value);
  }
};

#include "main.moc"
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
        auto rect2 = new CustomRectItem(0, 0, 9, 9);

        rect->setFlag(QGraphicsItem::ItemIsSelectable);
        rect->setPos(50, 50);
        rect2->setPos(10, 20);
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
