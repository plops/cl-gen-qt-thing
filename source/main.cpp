#include <QApplication>
#include <QDebug>
#include <QGraphicsItem>
#include <QGraphicsItemGroup>
#include <QGraphicsLineItem>
#include <QGraphicsRectItem>
#include <QGraphicsScene>
#include <QGraphicsTextItem>
#include <QGraphicsView>
#include <utility>
#include <vector>
//! This program displays a line on a canvas. The parameters of the line can be
//! adjusted with two control points. The canvas also displays a grid of square
//! pixels and highlights the pixels that are intersected by the line.

class CustomItemPixelsGroup : public QGraphicsItemGroup {
public:
  explicit CustomItemPixelsGroup(int dx, int dy, int nx, int ny,
                                 std::vector<std::pair<int, int>> vecs)
      : m_dx(dx), m_dy(dy), m_nx(nx), m_ny(ny) {
    {
      auto dx = m_dx;
      auto dy = m_dy;
      auto nx = m_nx;
      auto ny = m_ny;

      for (auto v : vecs) {
        {
          auto i = v.first;
          auto j = v.second;
          auto eps = -2;

          {
            auto y1 = ((dy * j) - eps);
            auto x1 = ((dx * i) - eps);
            auto y2 = ((dy * (1 + j)) + eps);
            auto x2 = ((dx * (1 + i)) + eps);

            this->addToGroup(
                new QGraphicsRectItem(QRectF(x1, y1, (x2 - x1), (y2 - y1))));
          }
        }
      }
    }
  }

private:
  unsigned int m_dx;
  unsigned int m_dy;
  unsigned int m_nx;
  unsigned int m_ny;
};

class CustomLineItem : public QGraphicsLineItem {
public:
  explicit CustomLineItem(const QLineF &line) : QGraphicsLineItem(line) {
    {
      auto w = 17;
      auto h = w;

      m_p1 = new CustomRectItem(
          QRectF((line.p1() - ((5.e-1f) * QPointF(w, h))), QSizeF(w, h)), this,
          this, true);
      m_p2 = new CustomRectItem(
          QRectF((line.p2() - ((5.e-1f) * QPointF(w, h))), QSizeF(w, h)), this,
          this, false);
    }
  }

private:
  CustomRectItem *m_p1 = nullptr;
  CustomRectItem *m_p2 = nullptr;
};

//! Movable square. Two of these are use to define a line.

//! The two control points are distinguished by the boolean member
//! first_point_p.
class CustomRectItem : public QGraphicsRectItem {
public:
  explicit CustomRectItem(const QRectF &rect, QGraphicsItem *parent,
                          CustomLineItem *line, bool first_point_p)
      : m_line(line), m_first_point_p(first_point_p),
        QGraphicsRectItem(rect, parent) {
    this->setFlag(QGraphicsItem::ItemIsMovable);
    this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
  }

private:
  QVariant itemChange(GraphicsItemChange change, const QVariant &value) {
    if (((ItemPositionChange == change) && scene())) {
      moveLineToCenter(value.toPointF());
    }

    return QGraphicsItem::itemChange(change, value);
  }

  void moveLineToCenter(QPointF newPos) {
    {
      auto p1 = (m_first_point_p) ? (newPos) : (m_line->line().p1());
      auto p2 = (m_first_point_p) ? (m_line->line().p2()) : (newPos);

      m_line->setLine(QLineF(p1, p2));
    }
  }

  CustomLineItem *m_line = nullptr;
  bool m_first_point_p = false;
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

      for (unsigned int i = 0; (i < nx); i += 1) {
        {
          auto y1 = (dy * (1 + i));
          auto x1 = (dx * 1);
          auto y2 = y1;
          auto x2 = (dx * nx);

          this->addToGroup(new QGraphicsLineItem(QLineF(x1, y1, x2, y2)));
        }
      }
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

        {
          auto line = new CustomLineItem(QLineF(40, 40, 80, 80));

          scene->addItem(line);
          // initiate the line to some random ;
        }

        scene->addItem(grid);
        // change position of handles now, so that the line is redrawn by
        // CustomRect::itemChange;
        scene->addText("hello");
      }
    }

    w.show();
    return a.exec();
  }
}
