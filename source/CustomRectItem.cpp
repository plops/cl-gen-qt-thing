#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QGraphicsScene>
CustomRectItem::CustomRectItem(const QRectF &rect, QGraphicsItem *parent,
                               CustomLineItem *line, bool first_point_p)
    : m_line(line), m_first_point_p(first_point_p),
      QGraphicsRectItem(rect, parent) {
  this->setFlag(QGraphicsItem::ItemIsMovable);
  this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
}

QVariant CustomRectItem::itemChange(GraphicsItemChange change,
                                    const QVariant &value) {
  if (((ItemPositionChange == change) && scene())) {
    moveLineToCenter(value.toPointF());
    m_line->scene()->removeItem(m_line->getPixels());
    {
      std::vector<std::pair<int, int>> pos;
      auto line = m_line->line();
      int dx = m_line->getPixels()->dx();
      int dy = m_line->getPixels()->dy();
      int nx = m_line->getPixels()->nx();
      int ny = m_line->getPixels()->ny();

      for (unsigned int j = 0; (j < (ny - 1)); j += 1) {
        for (unsigned int i = 0; (i < (nx - 1)); i += 1) {
          if ((fabsf(m_line->getDistanceFromPoint(
                   QPointF((dx * (i + (5.e-1f))), (dy * (j + (5.e-1f)))))) <
               ((1.e+0f) * sqrtf((dx * dy))))) {
            pos.push_back(std::make_pair(i, j));
          }
        }
      }

      m_line->setPixels(pos);
      {
        static std::array<std::array<std::array<unsigned char, IMG_H>, IMG_W>,
                          IMG_C>
            img;

        for (unsigned int j = 0; (j < IMG_H); j += 1) {
          for (unsigned int i = 0; (i < IMG_W); i += 1) {
            {
              auto v = m_line->getDistanceFromPoint(
                  QPointF((i + (5.e-1f)), (j + (5.e-1f))));
              auto v2 = (v * v);
              auto vu =
                  static_cast<unsigned char>(((v2 < (1.e+2f))) ? (v2) : (100));

              img[0][i][j] = 255 - vu;
              img[1][i][j] = 255 - vu;
              img[2][i][j] = 255 - vu;
            }
          }
        }

        m_line->updatePixmapFromImage(img);
      }
    }
  }

  return QGraphicsItem::itemChange(change, value);
}

void CustomRectItem::moveLineToCenter(QPointF newPos) {
  {
    auto p1 = (m_first_point_p) ? (newPos) : (m_line->line().p1());
    auto p2 = (m_first_point_p) ? (m_line->line().p2()) : (newPos);

    m_line->setLine(QLineF(p1, p2));
  }
}
