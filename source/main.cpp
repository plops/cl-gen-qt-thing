#include <QApplication>
#include <QDebug>
#include <QGraphicsItemGroup>
#include <QGraphicsLineItem>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
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

protected:
  QVariant itemChange(GraphicsItemChange change, const QVariant &value) {
    if (((ItemPositionChange == change) && scene())) {
      // value is the same as pos();
      moveLineToCenter(value.toPointF());
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

  QGraphicsLineItem *line;
  bool first_point_p;
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
      auto tr = QTransform();

      tr.rotate(45, Qt::ZAxis);
      w.setTransform(tr);
    }

    {
      auto scene = new QGraphicsScene(0, 0, 300, 300, &w);

      scene->setBackgroundBrush(Qt::lightGray);
      w.setScene(scene);
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
