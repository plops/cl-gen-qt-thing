#pragma once
#include <QGraphicsItemGroup>
#include <utility>
#include <vector>
class CustomItemPixelsGroup : public QGraphicsItemGroup {
public:
  explicit CustomItemPixelsGroup(int dx, int dy, int nx, int ny,
                                 std::vector<std::pair<int, int>> vecs);

private:
  unsigned int m_dx;
  unsigned int m_dy;
  unsigned int m_nx;
  unsigned int m_ny;
};
