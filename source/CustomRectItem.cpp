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

      for (unsigned int j = 0; (j < ny); j += 1) {
        for (unsigned int i = 0; (i < nx); i += 1) {
          if ((lineDistance(line, QPointF((dx * (i + 1)), (dy * (j + 1)))) <
               sqrtf((dx * dy)))) {
            (qDebug() << "close " << i << " " << j << " "
                      << lineDistance(line, QPointF((i + 1), (j + 1))));
            pos.push_back(std::make_pair(i, j));

          } else {
            (qDebug() << "far  " << i << " " << j << " "
                      << lineDistance(line,
                                      QPointF((dx * (i + 1)), (dy * (j + 1)))));
          }
        }
      }

      m_line->setPixels(pos);
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
