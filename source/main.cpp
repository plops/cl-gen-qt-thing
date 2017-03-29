#include <QApplication>
#include <QWidget>
class Widget : public QWidget {
  Q_OBJECT
public:
  explicit Widget(QWidget *parent = nullptr) : QWidget(parent), ui(new Widget) {
    ui->setupUi(this);
  }

  ~Widget() { delete ui; }

  void setName(const QString &name) { ui->lineEdit->setText(name); }

  name() const { return ui->lineEdit->text(); }

private:
  Widget *ui;
};

int main(int argc, char **argv) { return 0; }
