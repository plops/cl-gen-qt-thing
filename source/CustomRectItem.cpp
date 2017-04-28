#include <CustomRectItem.h>
#include <QGraphicsScene>
#include <QVector2D>
#include <utility>
#include <vector>
CustomRectItem::CustomRectItem(const QRectF &rect, QGraphicsItem *parent,
                               CustomLineItem *line, bool first_point_p)
    : m_line(line), m_first_point_p(first_point_p),
      QGraphicsRectItem(rect, parent) {
  this->setFlag(QGraphicsItem::ItemIsMovable);
  this->setFlag(QGraphicsItem::ItemSendsScenePositionChanges);
}

inline float lineDistance(QLineF line, QPointF p0) {
  // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line vector
  // formulation;
  {
    auto p1(line.p1());
    auto p2(line.p2());
    auto n_len = QVector2D((p1 - p2));
    auto n(n_len.normalized());
    auto a_p(QVector2D((p1 - p0)));
    auto a_p_dot_n(QVector2D::dotProduct(a_p, n));

    return (a_p - (a_p_dot_n * n)).length();
  }
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
          if ((fabsf(lineDistance(line, QPointF((dx * (i + (5.e-1f))),
                                                (dy * (j + (5.e-1f)))))) <
               ((5.e-1f) * sqrtf((dx * dy))))) {
            pos.push_back(std::make_pair(i, j));
          }
        }
      }

      m_line->setPixels(pos);
      {
        auto imgp = m_line->getImage();
        auto img = *imgp;

        for (unsigned int i = 0; (i < (DX * (NX - 1))); i += 1) {
          for (unsigned int j = 0; (j < (DY * (NY - 1))); j += 1) {
            {
              auto v =
                  lineDistance(line, QPointF((i + (5.e-1f)), (j + (5.e-1f))));
              auto vu = static_cast<unsigned char>(v);

              img[(0 + (3 * (j + (i * (DX * (NX - 1))))))] = j;
              img[(1 + (3 * (j + (i * (DX * (NX - 1))))))] = i;
              img[(2 + (3 * (j + (i * (DX * (NX - 1))))))] = vu;
            }
          }
        }

        m_line->updatePixmapFromImage();
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
