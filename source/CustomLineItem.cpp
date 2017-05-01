#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QDebug>
#include <QGraphicsScene>
#include <QVector2D>
#include <assert.h>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <utility>
#include <vector>
void CustomLineItem::createPPMHeader(int w, int h) {
  m_ppm_data.fill(0);
  assert((w <= (DX * (NX - 1))));
  assert((h <= (DY * (NY - 1))));
  assert((w <= 9999));
  assert((h <= 9999));
  {
    std::ostringstream oss;

    (oss << "P6 " << std::setw(4) << w << " " << std::setw(4) << h << " "
         << std::setw(3) << 255 << " ");
    {
      auto i = 0;

      for (const auto c : oss.str()) {
        m_ppm_data[i] = static_cast<unsigned char>(c);
        i += 1;
      }

      for (unsigned int j = 0; (j < (w * h * 3)); j += 1) {
        m_ppm_data[(i + j)] = m_image[j];
      }
    }
  }
}

void CustomLineItem::updatePixmapFromImage() {
  createPPMHeader((DX * (NX - 1)), (DY * (NY - 1)));
  assert(m_pixmap->loadFromData(m_ppm_data.data(), m_ppm_data.size(), "PPM"));
  (std::cout << "update" << std::endl);
}

CustomLineItem::CustomLineItem(const QLineF &line) : QGraphicsLineItem(line) {
  m_pixmap_item = new QGraphicsPixmapItem(this);
  m_pixmap = new QPixmap((DX * (NX - 1)), (DY * (NY - 1)));

  {
    auto w = 17;
    auto h = w;

    m_p1 = new CustomRectItem(QRectF(((-5.e-1f) * QPointF(w, h)), QSizeF(w, h)),
                              this, this, true);
    m_p1->setPos(line.p1());
    m_p2 = new CustomRectItem(QRectF(((-5.e-1f) * QPointF(w, h)), QSizeF(w, h)),
                              this, this, false);
    m_p2->setPos(line.p2());
  }

  {
    auto img = m_image;

    for (unsigned int i = 0; (i < (DX * (NX - 1))); i += 1) {
      for (unsigned int j = 0; (j < (DY * (NY - 1))); j += 1) {
        {
          auto v =
              getDistanceFromPoint(QPointF((i + (5.e-1f)), (j + (5.e-1f))));
          auto vu = static_cast<unsigned char>(v);

          m_image[(0 + (3 * (j + (i * (DX * (NX - 1))))))] = j;
          m_image[(1 + (3 * (j + (i * (DX * (NX - 1))))))] = i;
          m_image[(2 + (3 * (j + (i * (DX * (NX - 1))))))] = 100;
        }
      }
    }

    updatePixmapFromImage();
  }

  m_pixmap_item->setPixmap(*m_pixmap);

  {
    std::vector<std::pair<int, int>> pos = {{1, 1}, {2, 2}, {2, 3}};

    m_pixels = new CustomItemPixelsGroup(DX, DY, NX, NX, pos, this);
  }
}

CustomItemPixelsGroup *CustomLineItem::getPixels() { return m_pixels; }

std::array<unsigned char, PPM_IMAGE_BYTES> *CustomLineItem::getImage() {
  return &m_image;
}

void CustomLineItem::setPixels(std::vector<std::pair<int, int>> vecs) {
  {
    auto dx = m_pixels->dx();
    auto dy = m_pixels->dy();
    auto nx = m_pixels->nx();
    auto ny = m_pixels->ny();

    delete m_pixels;
    m_pixels = new CustomItemPixelsGroup(dx, dy, nx, ny, vecs, this);
  }
}

//! Computes distance from a point p0 to the line through the points m_p1 and
//! m_p2. Make sure m_p1->m_line.{p1,p2} are initialized correctly before
//! calling this function.
float CustomLineItem::getDistanceFromPoint(QPointF p0) {
  // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line vector
  // formulation;
  {
    auto p1(m_p1->pos());
    auto p2(m_p2->pos());
    auto n_len = QVector2D((p1 - p2));
    auto n(n_len.normalized());
    auto a_p(QVector2D((p1 - p0)));
    auto a_p_dot_n(QVector2D::dotProduct(a_p, n));

    return (a_p - (a_p_dot_n * n)).length();
  }
}
