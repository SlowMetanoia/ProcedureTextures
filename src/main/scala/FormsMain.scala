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
      val bounds = getBounds.width max getBounds.height
      //в компонент единичный отрезок должен поместиться 2*baseLength раз + ещё чуть-чуть
      val unitSize = bounds / (2 * baseLength + 1)
      //сдвиг в центр
      val xt = new AffineTransform(1,0,0,-1,getBounds.width/2,getBounds.height/2)
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
