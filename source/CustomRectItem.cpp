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
      std::vector<std::pair<int, int>> pos = {{1, 3}, {4, 5}, {9, 4}};

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
