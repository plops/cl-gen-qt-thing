#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QDebug>
#include <QGraphicsScene>
CustomLineItem::CustomLineItem(const QLineF &line) : QGraphicsLineItem(line) {
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

    m_pixels = new CustomItemPixelsGroup(20, 20, 10, 10, pos, this);
  }
}

QVariant CustomLineItem::itemChange(GraphicsItemChange change,
                                    const QVariant &value) {
  if (((ItemPositionChange == change) && scene())) {
    // value is the same as pos();
    (qDebug() << "change pos customLine " << this->pos() << " " << value);
  }

  return QGraphicsItem::itemChange(change, value);
}

CustomItemPixelsGroup *CustomLineItem::getPixels() { return m_pixels; }

void CustomLineItem::setPixels(std::vector<std::pair<int, int>> vecs) {
  m_pixels = new CustomItemPixelsGroup(20, 20, 10, 10, vecs, this);
}
