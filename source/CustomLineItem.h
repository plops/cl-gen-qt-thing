#pragma once
#include <CustomItemPixelsGroup.h>
#include <QGraphicsItem>
#include <QtCore>
class CustomRectItem;
class CustomLineItem : public QGraphicsLineItem {
public:
  explicit CustomLineItem(const QLineF &line);
  QVariant itemChange(GraphicsItemChange change, const QVariant &value);

private:
  CustomRectItem *m_p1 = nullptr;
  CustomRectItem *m_p2 = nullptr;
  CustomItemPixelsGroup *m_pixels = nullptr;
};
