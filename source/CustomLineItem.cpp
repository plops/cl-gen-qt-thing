#include <CustomLineItem.h>
#include <CustomRectItem.h>
#include <QDebug>
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
}

QVariant CustomLineItem::itemChange(GraphicsItemChange change,
                                    const QVariant &value) {
  (qDebug() << "change customLine " << this->pos() << " " << value);
  if (((ItemPositionChange == change) && scene())) {
    // value is the same as pos();
    (qDebug() << "change pos customLine " << this->pos() << " " << value);
  }

  return QGraphicsItem::itemChange(change, value);
}
