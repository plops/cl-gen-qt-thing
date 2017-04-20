#include <QApplication>
#include <QDebug>
#include <QGraphicsItem>
#include <QGraphicsItemGroup>
#include <QGraphicsLineItem>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsTextItem>
#include <QGraphicsView>
//! This program displays a line on a canvas. The parameters of the line can be
//! adjusted with two control points. The canvas also displays a grid of square
//! pixels and highlights the pixels that are intersected by the line.

//! Movable square. Two of these are use to define a line.

//! The two control points are distinguished by the boolean member
//! first_point_p.
class CustomRectItem : public QGraphicsRectItem {
public:
  explicit CustomRectItem(const QRectF &rect) : QGraphicsRectItem(rect) {
    this->setFlag(QGraphicsItem::ItemIsMovable);
    this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
  }

  //! Attach a line to the control point.

  //! For each line this should be called twice for two different instances of
  //! CustomRectItem and different second parameter. Call addLine before adding
  //! the CustomRectItem to the scene. This ensures that the line coordinates
  //! are in a consistent state.
  void addLine(QGraphicsLineItem *line, bool is_first_point_p) {
    this->line = line;
    first_point_p = is_first_point_p;
  }

  //! Display a label below the control point.

  //! Call addLabel before setPos. This ensures that the point coordinates are
  //! displayed correctly.
  void addLabel() {
    text = new QGraphicsTextItem();
    text->setPos(this->pos());
    text->setPlainText("Barev");
    this->scene()->addItem(text);
  }

protected:
  //! Update line parameters when the control point is moved with the mouse.
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
  //! Update one of the two points of the line. The bool first_point_p chooses
  //! the point.
  void moveLineToCenter(QPointF newPos) {
    {
      auto p1 = (first_point_p) ? (newPos) : (line->line().p1());
      auto p2 = (first_point_p) ? (line->line().p2()) : (newPos);

      line->setLine(QLineF(p1, p2));
    }
  }

  //! Update the text label position.
  void moveTextToCenter(QPointF newPos) {
    if (text) {
      text->setPos(newPos);
    }
  }

  QGraphicsLineItem *line = nullptr;
  QGraphicsTextItem *text = nullptr;
  bool first_point_p = false;
};

class CustomItemGridGroup : public QGraphicsItemGroup {
public:
  explicit CustomItemGridGroup(int dx, int dy, int nx, int ny)
      : m_dx(dx), m_dy(dy), m_nx(nx), m_ny(ny) {
    // draw grid
    {
      auto dx = m_dx;
      auto dy = m_dy;
      auto nx = m_nx;
      auto ny = m_ny;

      for (unsigned int i = 0; (i < ny); i += 1) {
        {
          auto x1 = (dx * (1 + i));
          auto y1 = (dy * 1);
          auto x2 = x1;
          auto y2 = (dy * ny);

          this->addToGroup(new QGraphicsLineItem(QLineF(x1, y1, x2, y2)));
        }
      }

      // highlight one rectangle;
    }
  }

private:
  unsigned int m_dx;
  unsigned int m_dy;
  unsigned int m_nx;
  unsigned int m_ny;
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
      // two handles to define the line
      {
        auto w = (1.7e+1f);
        auto c = (w / (-2.e+0f));
        auto grid = new CustomItemGridGroup(20, 20, 10, 10);
        auto handle_center = new CustomRectItem(QRectF(c, c, w, w));
        auto handle_periph = new CustomRectItem(QRectF(c, c, w, w));

        {
          auto line = scene->addLine(QLineF(40, 40, 80, 80));

          // initiate the line to some random ;
          handle_center->addLine(line, true);
          handle_periph->addLine(line, false);
        }

        scene->addItem(grid);
        scene->addItem(handle_center);
        scene->addItem(handle_periph);
        handle_center->addLabel();
        handle_periph->addLabel();
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
