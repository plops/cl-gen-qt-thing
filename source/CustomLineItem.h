#pragma once
#include <CustomItemPixelsGroup.h>
#include <QGraphicsItem>
#include <QtCore>
class CustomRectItem;
enum Coord {
  DX = 20,
  DY = 20,
  NX = 10,
  NY = 10,
  PPM_IMAGE_BYTES = (3 * (DX * (NX - 1)) * (DY * (NY - 1))),
  PPM_HEADER_LENGTH = 17
};

class CustomLineItem : public QGraphicsLineItem {
public:
  explicit CustomLineItem(const QLineF &line);
  CustomItemPixelsGroup *getPixels();
  std::array<unsigned char, PPM_IMAGE_BYTES> *getImage();
  void setPixels(std::vector<std::pair<int, int>> vecs);
  void updatePixmapFromImage();
  float getDistanceFromPoint(QPointF p0);

private:
  void createPPMHeader(int w, int h);
  CustomRectItem *m_p1 = nullptr;
  CustomRectItem *m_p2 = nullptr;
  CustomItemPixelsGroup *m_pixels = nullptr;
  QGraphicsPixmapItem *m_pixmap_item = nullptr;
  QPixmap *m_pixmap = nullptr;
  std::array<unsigned char, (PPM_HEADER_LENGTH + PPM_IMAGE_BYTES)> m_ppm_data;
  std::array<unsigned char, PPM_IMAGE_BYTES> m_image;
};
