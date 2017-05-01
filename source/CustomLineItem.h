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
  IMG_C = 3,
  IMG_W = (DX * (NX - 1)),
  IMG_H = (DY * (NY - 1)),
  PPM_IMAGE_BYTES = (IMG_C * IMG_W * IMG_H),
  PPM_HEADER_LENGTH = 17
};

class CustomLineItem : public QGraphicsLineItem {
public:
  explicit CustomLineItem(const QLineF &line);
  CustomItemPixelsGroup *getPixels();
  std::array<std::array<std::array<unsigned char, IMG_C>, IMG_W>, IMG_H> *
  getImage();
  QGraphicsPixmapItem *getImageItem();
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
  std::array<std::array<std::array<unsigned char, IMG_C>, IMG_W>, IMG_H>
      m_image;
};
