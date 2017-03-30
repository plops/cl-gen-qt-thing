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
      QGraphicsRectItem *rect = new QGraphicsRectItem(50, 50, 100, 100);

      scene->addItem(rect);
    }

    w.show();
    return a.exec();
  }
}
