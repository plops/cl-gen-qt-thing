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
  Ui::MainWindow *ui;
};

#endif // MAIN_WIN_H