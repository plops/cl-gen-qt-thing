#include <QApplication>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsView>
int main(int argc, char **argv) {
  {
    QApplication a(argc, argv);
    QGraphicsView w;
    QGraphicsScene *scene = new QGraphicsScene(0, 0, 300, 300, &w);

    scene->setBackgroundBrush(Qt::yellow);
    w.setScene(scene);
    {
      auto tr = QTransform();

      tr.rotate(45, Qt::ZAxis);
      w.setTransform(tr);
    }

    {
      QGraphicsRectItem *rect = new QGraphicsRectItem(50, 50, 100, 100);

      rect->setFlag(QGraphicsItem::ItemIsSelectable);
      scene->addItem(rect);
      scene->addText("hello");
    }

    w.show();
    return a.exec();
  }
}
