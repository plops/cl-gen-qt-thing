#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QDebug>
#include <QGraphicsScene>
#include <assert.h>
#include <iomanip>
#include <sstream>
enum Coord { DX = 20, DY = 20, NX = 30, NY = 30, PPM_HEADER_LENGTH = 17 };

std::array<unsigned char,
           (PPM_HEADER_LENGTH + (3 * (DX * (NX - 1)) * (DY * (NY - 1))))>
    g_ppm_data;

void toPPM(int w, int h) {
  g_ppm_data.fill(0);
  assert((w <= (DX * (NX - 1))));
  assert((h <= (DY * (NY - 1))));
  assert((w <= 9999));
  assert((h <= 9999));
  {
    std::ostringstream oss;

    (oss << "P6 " << std::setw(4) << w << " " << std::setw(4) << h << " "
         << std::setw(3) << 255 << " ");
    oss.seekg(0, ios::end);
    {
      auto size = oss.tellg();
      auto i = 0;

      oss.seekg(0, ios::beg);
      for (const auto c : oss.str()) {
        g_ppm_data[i] = static_cast<unsigned char>(c);
        i += 1;
      }
    }
  }
}

CustomLineItem::CustomLineItem(const QLineF &line) : QGraphicsLineItem(line) {
  m_pixmap_item = new QGraphicsPixmapItem(this);
  m_pixmap = new QPixmap((DX * (NX - 1)), (DY * (NY - 1)));

  m_pixmap->fill(Qt::green);
  toPPM((DX * (NX - 1)), (DY * (NY - 1)));
  m_pixmap_item->setPixmap(*m_pixmap);

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
