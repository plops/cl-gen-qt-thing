#pragma once
#include <CustomItemPixelsGroup.h>
#include <QGraphicsItem>
#include <QtCore>
class CustomRectItem;
class CustomLineItem : public QGraphicsLineItem {
public:
  explicit CustomLineItem(const QLineF &line);
  CustomItemPixelsGroup *getPixels();
  void setPixels(std::vector<std::pair<int, int>> vecs);

private:
  CustomRectItem *m_p1 = nullptr;
  CustomRectItem *m_p2 = nullptr;
  CustomItemPixelsGroup *m_pixels = nullptr;
};
