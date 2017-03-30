#include "main_win.h"
graph_widget::graph_widget(QWidget *parent) : QGraphicsView(parent) {
  {
    QGraphicsScene *scene = new QGraphicsScene(this);

    scene->setSceneRect(-200, -200, 400, 400);
    scene->addText("Hello");
    setScene(scene);
    setWindowTitle(tr("elastic nodes"));
  }
}

MainWindow::MainWindow(QWidget *parent) : QMainWindow(parent) {
  //;
}

MainWindow::~MainWindow() {
  // ;
}
