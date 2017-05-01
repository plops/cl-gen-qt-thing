#include <CustomLineItem.h>
#include <QApplication>
#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QtCore>
#include <utility>
#include <vector>
//! This program displays a line on a canvas. The parameters of the line can be
//! adjusted with two control points. The canvas also displays a grid of square
//! pixels and highlights the pixels that are intersected by the line.

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
          auto x1 = (dx * i);
          auto y1 = (dy * 0);
          auto x2 = x1;
          auto y2 = (dy * (ny - 1));

          this->addToGroup(new QGraphicsLineItem(QLineF(x1, y1, x2, y2)));
        }
      }

      for (unsigned int i = 0; (i < nx); i += 1) {
        {
          auto y1 = (dy * i);
          auto x1 = (dx * 0);
          auto y2 = y1;
          auto x2 = (dx * (nx - 1));

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

          // the following reasignment of parents is required for the line to be
          // drawn on top of the pixmap;
          line->getImageItem()->setParentItem(nullptr);
          line->setParentItem(line->getImageItem());
          scene->addItem(line->getImageItem());
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
