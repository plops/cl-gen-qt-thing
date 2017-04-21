#include <CustomItemPixelsGroup.h>
#include <QGraphicsRectItem>
CustomItemPixelsGroup::CustomItemPixelsGroup(
    int dx, int dy, int nx, int ny, std::vector<std::pair<int, int>> vecs,
    QGraphicsItem *parent)
    : m_dx(dx), m_dy(dy), m_nx(nx), m_ny(ny), QGraphicsItemGroup(parent) {
  {
    auto dx = m_dx;
    auto dy = m_dy;
    auto nx = m_nx;
    auto ny = m_ny;

    for (auto v : vecs) {
      {
        auto i = v.first;
        auto j = v.second;
        auto eps = -2;

        {
          auto y1 = ((dy * j) - eps);
          auto x1 = ((dx * i) - eps);
          auto y2 = ((dy * (1 + j)) + eps);
          auto x2 = ((dx * (1 + i)) + eps);

          this->addToGroup(
              new QGraphicsRectItem(QRectF(x1, y1, (x2 - x1), (y2 - y1))));
        }
      }
    }
  }
}
