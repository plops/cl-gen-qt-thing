#ifndef MAIN_WIN_H
#define MAIN_WIN_H
#include <QMainWindow>
#include <QWidget>
class MainWindow : public QMainWindow {
  Q_OBJECT
public:
  explicit MainWindow(QWidget *parent = nullptr);
  ~MainWindow();

private:
  MainWindow *ui;
};

#endif // MAIN_WIN_H