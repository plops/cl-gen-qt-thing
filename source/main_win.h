#ifndef MAIN_WIN_H
#define MAIN_WIN_H
#include <QMainWindow>
#include <QWidget>
namespace Ui {
class MainWindow;
} // namespace Ui

class MainWindow : public QMainWindow {
  Q_OBJECT
public:
  explicit MainWindow(QWidget *parent = nullptr);
  ~MainWindow();

private:
};

class graph_widget : public QGraphicsView {
  Q_OBJECT
public:
  explicit graph_widget(QWidget *parent = nullptr);

private:
};

#endif // MAIN_WIN_H