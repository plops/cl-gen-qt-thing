#include <QApplication>
#include <QWidget>
explicit Widget(QWidget *parent = nullptr) : QWidget(parent), ui(new Widget) {
  ui->setupUi(this);
}

~Widget() { delete ui; }

void setName(const QString &name) { ui->lineEdit->setText(name); }

name() const { return ui->lineEdit->text(); }
