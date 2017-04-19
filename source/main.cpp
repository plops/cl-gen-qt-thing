#include <QApplication>
#include <QDebug>
#include <QGraphicsItemGroup>
#include <QGraphicsLineItem>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsTextItem>
#include <QGraphicsView>
class CustomRectItem : public QGraphicsRectItem {
public:
  explicit CustomRectItem(const QRectF &rect) : QGraphicsRectItem(rect) {
    this->setFlag(QGraphicsItem::ItemIsMovable);
    this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
  }

  void addLine(QGraphicsLineItem *line, bool is_first_point_p) {
    this->line = line;
    first_point_p = is_first_point_p;
  }

  void addLabel() {
    text = new QGraphicsTextItem();
    text->setPos(this->pos());
    text->setPlainText("Barev");
    this->scene()->addItem(text);
  }

protected:
  QVariant itemChange(GraphicsItemChange change, const QVariant &value) {
    if (((ItemPositionChange == change) && scene())) {
      // value is the same as pos();
      moveLineToCenter(value.toPointF());
      if (text) {
        {
          QString s;
          QTextStream st(&s);

          st.setFieldWidth(4);
          st.setFieldAlignment(QTextStream::AlignCenter);
          st.setPadChar('_');
          (st << value.toPointF().x() << value.toPointF().y());
          text->setPlainText(s);
          moveTextToCenter(value.toPointF());
        }
      }
    }

    return QGraphicsItem::itemChange(change, value);
  }

private:
  void moveLineToCenter(QPointF newPos) {
    {
      auto p1 = (first_point_p) ? (newPos) : (line->line().p1());
      auto p2 = (first_point_p) ? (line->line().p2()) : (newPos);

      line->setLine(QLineF(p1, p2));
    }
  }

  void moveTextToCenter(QPointF newPos) {
    if (text) {
      text->setPos(newPos);
    }
  }

  QGraphicsLineItem *line = nullptr;
  QGraphicsTextItem *text = nullptr;
  bool first_point_p = false;
};

int main(int argc, char **argv) {
  if ((0 == argc)) {
    return 0;
  }

  if ((nullptr == argv)) {
    return 0;
  }

  {
    QApplication a(argc, argv);
    QGraphicsView w;

    w.setAttribute(Qt::WA_TranslucentBackground, false);
    {
      auto scene = new QGraphicsScene(0, 0, 300, 300, &w);

      scene->setBackgroundBrush(Qt::white);
      w.setScene(scene);
      // draw grid
      {
        auto dx = 20;
        auto dy = dx;
        auto nx = 10;
        auto ny = nx;

        for (unsigned int i = 0; (i < ny); i += 1) {
          {
            auto x1 = (dx * (1 + i));
            auto y1 = (dy * 1);
            auto x2 = x1;
            auto y2 = (dy * ny);

            scene->addLine(QLineF(x1, y1, x2, y2));
          }
        }

        for (unsigned int i = 0; (i < nx); i += 1) {
          {
            auto y1 = (dy * (1 + i));
            auto x1 = (dx * 1);
            auto y2 = y1;
            auto x2 = (dx * nx);

            scene->addLine(QLineF(x1, y1, x2, y2));
          }
        }

        // highlight one rectangle;
        {
          auto i = 4;
          auto j = 3;
          auto eps = 1;

          {
            auto y1 = ((dy * j) - eps);
            auto x1 = ((dx * i) - eps);
            auto y2 = ((dy * (1 + j)) + eps);
            auto x2 = ((dx * (1 + i)) + eps);

            scene->addRect(QRectF(x1, y1, (x2 - x1), (y2 - y1)));
          }
        }
      }

      // two handles to define the line
      {
        auto w = (1.7e+1f);
        auto c = (w / (-2.e+0f));
        auto handle_center = new CustomRectItem(QRectF(c, c, w, w));
        auto handle_periph = new CustomRectItem(QRectF(c, c, w, w));

        {
          auto line = scene->addLine(QLineF(40, 40, 80, 80));

          // initiate the line to some random ;
          handle_center->addLine(line, true);
          handle_periph->addLine(line, false);
        }

        scene->addItem(handle_center);
        scene->addItem(handle_periph);
        // change position of handles now, so that the line is redrawn by
        // CustomRect::itemChange;
        handle_center->setPos(150, 150);
        handle_periph->setPos(130, 280);
        handle_center->addLabel();
        handle_periph->addLabel();
        {
          auto tr = QTransform();

          tr.rotate(45, Qt::ZAxis);
          handle_center->setTransform(tr);
        }

        scene->addText("hello");
      }
    }

    w.show();
    return a.exec();
  }
}
