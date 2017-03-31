#include <QApplication>
#include <QGraphicsItemGroup>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsView>
int main(int argc, char **argv) {
  {
    QApplication a(argc, argv);
    QGraphicsView w;
    auto scene = new QGraphicsScene(0, 0, 300, 300, &w);

    scene->setBackgroundBrush(Qt::yellow);
    w.setScene(scene);
    {
      auto tr = QTransform();

      tr.rotate(45, Qt::ZAxis);
      w.setTransform(tr);
    }

    {
      auto rect = new QGraphicsRectItem(50, 50, 59, 59);
      auto rect2 = new QGraphicsRectItem(0, 0, 9, 9);

      rect->setFlag(QGraphicsItem::ItemIsSelectable);
      rect2->setFlag(QGraphicsItem::ItemIsMovable);
      {
        QList<QGraphicsItem *> ql({rect, rect2});
        auto grp = scene->createItemGroup(ql);
      }

      {
        auto tr = QTransform();

        tr.rotate(45, Qt::ZAxis);
        rect2->setTransform(tr);
      }

      scene->addText("hello");
    }

    w.show();
    return a.exec();
  }
}
