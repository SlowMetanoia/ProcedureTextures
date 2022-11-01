import pkg._

import java.awt._
import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import javax.swing.JComponent

object FormsMain{
//  val jFrame = JFrameBasics.jFrame
//
//  var (r,g,b) = (0,0,0)
//  val jPanel = new JPanel()
//  val colorSliders = Seq(
//    new SliderInit(0,255,0,"red",{value=>   r = value;jPanel.setBackground(new Color(r,g,b))},true),
//    new SliderInit(0,255,0,"green",{value=> g = value;jPanel.setBackground(new Color(r,g,b))},true),
//    new SliderInit(0,255,0,"blue",{value=>  b = value;jPanel.setBackground(new Color(r,g,b))},true)
//    )
//  jPanel.setBackground(new Color(0,0,0))
//  jFrame.add(jPanel, BorderLayout.NORTH)
//  jFrame.add(slidersPanel(colorSliders, "Choose color"), BorderLayout.CENTER)
//  //jFrame.add(new ColorChoosePanel( (x,y,z)=>{jPanel.setBackground(new Color(x,y,z))},"choose color"), BorderLayout.CENTER)
//  jFrame.revalidate()
  
  class ATChooseComponent(baseLength:Double) extends JComponent{
    import math._
    def getAT(baseLine:Line2D,imageLine:Line2D):AffineTransform = {
      val xT = new AffineTransform()

      val (x0, y0, x1, y1) = (
        baseLine.getX1 - baseLine.getX2,
        baseLine.getY1 - baseLine.getY2,
        imageLine.getX1 - imageLine.getX2,
        imageLine.getX1 - imageLine.getX2
      )
      val (baseLength, imgLength) = (sqrt(x0 * x0 + y0 * y0), sqrt(x1 * x1 + y1 * y1))

      //вычислим синус и косинус угла поворота
      val cosFi = (x0*x1 + y0*y1)/(baseLength*imgLength)
      val sinFi = sqrt(1 - cosFi*cosFi)
      //аффинное преобразование поворота
      val rotationT = new AffineTransform(cosFi,-sinFi,sinFi,cosFi,0,0)
      //аффинное преобразование сжатия
      val suppressionT = new AffineTransform(baseLength/imgLength,0,0,baseLength/imgLength,0,0)
      //применим преобразования
      xT.concatenate(rotationT)
      xT.concatenate(suppressionT)
      val newP1 = xT.transform(baseLine.getP1,null)
      //вычислим оставшийся сдвиг
      val (dx,dy) = (imageLine.getX1 - newP1.getX,imageLine.getY1 - newP1.getY)
      //аффинное преобразование сдвига
      val shiftT = new AffineTransform(0,0,0,0,dx,dy)
      xT.concatenate(shiftT)
      xT
    }

    val baseCut = new Line2D.Double(0,0,baseLength,0)
    
    var pointOne:Option[Point2D] = None
    var Cut:Option[Line2D] = None
    var resultAT:Option[AffineTransform] = None
    
    setMinimumSize(new Dimension(100,100))
    setMaximumSize(new Dimension(700,700))
    setPreferredSize(new Dimension(400,400))
    
    def ownDefaultAT: AffineTransform = {
      //начальные границы
      //делаем вид, что область квадратная
      val bounds: Int = if (getBounds().width > getBounds().height) getBounds().height else getBounds().width
      //в компонент единичный отрезок должен поместиться 2*baseLength раз + ещё чуть-чуть
      val unitSize = bounds / (2 * baseLength + 1)
      //сдвиг в центр
      val xt = new AffineTransform(1,0,0,-1,getBounds().width/2,getBounds().height/2)
      //растягивание до нужного размера
      xt.concatenate(new AffineTransform(unitSize,0,0,unitSize,0,0))
      xt
    }
    
    override def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setTransform(ownDefaultAT)
    }
  }
  
  //atChooseComponent
def main( args: Array[ String ] ): Unit = {
  val frame = JFrameBasics.jFrame
  //относительный размер единицы
  var unitRelativeSize = 400
  
  val componentCenter = (
    frame.getBounds.width/2,
    frame.getBounds.height/2
  )
  var gridT = new AffineTransform(1,0,0,-1,componentCenter._1,componentCenter._2)
  gridT.concatenate(new AffineTransform(unitRelativeSize,0,0,unitRelativeSize,0,0))
  val atcc = new ATChooseComponent(1)
  //println(s"atcc.getAT(new Line2D.Double(0,0,1,0)) = ${atcc.getAT(new Line2D.Double(0,0,1,0),new Line2D.Double(3,4,5,6)).transform(new Point2D.Double(0,0))}")
}
  
  def grid(size:Int):Seq[Shape] = {
    (-size to size).flatMap{ i=>
      Seq(
        new Line2D.Double(-size,i,size,i),
        new Line2D.Double(i,-size,i,size)
        )
    }
  }
}
