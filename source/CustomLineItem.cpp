#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QDebug>
#include <QGraphicsScene>
#include <QVector2D>
#include <assert.h>
#include <iomanip>
#include <sstream>
#include <utility>
#include <vector>
int CustomLineItem::createPPMHeader(
    std::array<std::array<std::array<unsigned char, IMG_H>, IMG_W>, IMG_C>
        &image) {
  m_ppm_data.fill(0);
  {
    auto colors = image.size();
    auto w = image[0].size();
    auto h = image[0][0].size();

    assert((w <= (DX * (NX - 1))));
    assert((h <= (DY * (NY - 1))));
    assert((w <= 9999));
    assert((h <= 9999));
    assert((3 == colors));
    {
      std::ostringstream oss;

      (oss << "P6 " << std::setw(4) << w << " " << std::setw(4) << h << " "
           << std::setw(3) << 255 << " ");
      {
        auto i0 = 0;

        for (const auto c : oss.str()) {
          m_ppm_data[i0] = static_cast<unsigned char>(c);
          i0 += 1;
        }

        {
          auto sum = i0;

          for (unsigned int j = 0; (j < h); j += 1) {
            for (unsigned int i = 0; (i < w); i += 1) {
              for (unsigned int k = 0; (k < colors); k += 1) {
                m_ppm_data[(i0 + k + (3 * ((w * j) + i)))] = image[k][i][j];
                sum += 1;
              }
            }
          }

          return sum;
        }
      }
    }
  }
}

void CustomLineItem::updatePixmapFromImage(
    std::array<std::array<std::array<unsigned char, IMG_H>, IMG_W>, IMG_C>
        &image) {
  {
    auto colors = image.size();
    auto w = image[0].size();
    auto h = image[0][0].size();

    {
      auto n(createPPMHeader(image));

      assert(m_pixmap->loadFromData(m_ppm_data.data(), n, "PPM"));
    }

    m_pixmap_item->setPixmap(*m_pixmap);
  }
}

CustomLineItem::CustomLineItem(const QLineF &line) : QGraphicsLineItem(line) {
  // the order of initialization is important so that items are layered
  // correctly. Unfortunately it is not possible to bring the line  (this
  // object) above the pixmap from within this constructor. Instead I have to
  // change the parents after object creation;
  m_pixmap_item = new QGraphicsPixmapItem(this);
  m_pixmap = new QPixmap(IMG_W, IMG_H);

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
    std::vector<std::pair<int, int>> pos = {{1, 1}, {2, 2}, {2, 3}};

    m_pixels = new CustomItemPixelsGroup(DX, DY, NX, NX, pos, this);
  }
}

CustomItemPixelsGroup *CustomLineItem::getPixels() { return m_pixels; }

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
//! m_p2. Make sure m_p{1,2}->pos are initialized correctly before calling this
//! function.
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

QGraphicsPixmapItem *CustomLineItem::getImageItem() { return m_pixmap_item; }
