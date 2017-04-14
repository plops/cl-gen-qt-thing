#include <QApplication>
#include <QDebug>
#include <QGraphicsItemGroup>
#include <QGraphicsLineItem>
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

class CustomRectItem : public QGraphicsRectItem {
public:
  explicit CustomRectItem(const QRectF &rect) : QGraphicsRectItem(rect) {
    this->setFlag(QGraphicsItem::ItemIsMovable);
    this->setFlag(QGraphicsItem::ItemSendsGeometryChanges);
    this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
  }

protected:
  void mouseReleaseEvent(QGraphicsSceneMouseEvent *event) {
    (qDebug() << "mouse released in " << this->pos());
    QGraphicsRectItem::mouseReleaseEvent(event);
  }

  QVariant itemChange(GraphicsItemChange change, const QVariant value) {
    (qDebug() << "item change");
    if (((QGraphicsItem::ItemPositionHasChanged == change) && scene())) {
      // value is the same as pos();
      (qDebug() << "item changed to " << value);
    }

    return QGraphicsItem::itemChange(change, value);
  }

private:
  QGraphicsLineItem *line;
  bool first_point_p;
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
        auto rect = new CustomRectItem(QRectF(0, 0, 9, 9));
        auto rect2 = new CustomRectItem(QRectF(0, 0, 9, 9));

        rect->setPos(50, 50);
        rect2->setPos(10, 20);
        { auto line = scene->addLine(QLineF(40, 40, 80, 80)); }

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
