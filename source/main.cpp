#include "main_win.h"
#include <QApplication>
#include <QtGui>
int main(int argc, char **argv) {
  {
    QApplication a(argc, argv);
    QGraphicsView w;
    QGraphicsScene *scene = new QGraphicsScene(0, 0, 300, 300, &w);

    scene->setBackgroundBrush(Qt::yellow);
    w.setScene(scene);
    w.show();
    return a.exec();
  }
}
