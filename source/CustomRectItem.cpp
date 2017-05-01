#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QGraphicsScene>
#include <iostream>
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
               ((5.e-1f) * sqrtf((dx * dy))))) {
            pos.push_back(std::make_pair(i, j));
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
