#pragma once
#include <CustomLineItem.h>
#include <QGraphicsItem>
#include <QtCore>
class CustomRectItem : public QGraphicsRectItem {
public:
  explicit CustomRectItem(const QRectF &rect, QGraphicsItem *parent,
                          CustomLineItem *line, bool first_point_p);

private:
  QVariant itemChange(GraphicsItemChange change, const QVariant &value);
  void moveLineToCenter(QPointF newPos);
  CustomLineItem *m_line = nullptr;
  bool m_first_point_p = false;
};
