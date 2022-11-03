import FormsMain.ATChooseComponent.ATSub
import pkg._

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent, MouseMotionAdapter}
import java.awt.geom.{AffineTransform, Line2D, Point2D}
import javax.swing.JComponent

object FormsMain{
  class ATChooseComponent(baseLength:Double) extends JComponent{
    
    var atSubs:Seq[ATSub] = Seq.empty
    
    val baseCut = new Line2D.Double(0,0,baseLength,0)
    
    var pointOne:Option[Point2D] = None
    var pointTwo:Option[Point2D] = None
    var resultAT:Option[AffineTransform] = None
    
    object mousePoint extends Iterator[Point2D]{
      var pt:Point2D = new Point2D.Double(0,0)
      def set(e:MouseEvent):Unit = {
        pt = ownDefaultAT.inverseTransform(e.getPoint,null)
      }
      override def hasNext: Boolean = true
      override def next( ): Point2D = pt
    }
    
    setMinimumSize(new Dimension(100,100))
    setMaximumSize(new Dimension(700,700))
    setPreferredSize(new Dimension(400,400))
    
    addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved( e: MouseEvent ): Unit = {
        super.mouseMoved(e)
        mousePoint.set(e)
        repaint()
      }
    })
    
    def updateAT(xT:AffineTransform):Unit = {
      resultAT = Some(xT)
      atSubs.foreach(_.atChanged(xT))
    }
    
    addMouseListener(new MouseAdapter{
      override def mouseClicked( e: MouseEvent ): Unit = {
        super.mouseClicked(e)
        e.getButton match {
          case MouseEvent.BUTTON1 =>
            pointOne = Some(mousePoint.next())
          case MouseEvent.BUTTON3 =>
            pointTwo = Some(mousePoint.next())
          case _ =>
        }
        (pointOne,pointTwo) match {
          case (Some(pt1),Some(pt2)) => updateAT(ATChooseComponent.getAT(baseCut,new Line2D.Double(pt1,pt2)))
          case _ =>
        }
        repaint()
      }
    })
    
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
      g2d.setStroke(new BasicStroke(0))
      grid(100).foreach{ line =>
        if(line.getX1==0 || line.getY1==0) {
          val stroke = g2d.getStroke
          g2d.setStroke(new BasicStroke((2.0/ownDefaultAT.getScaleX).toFloat))
          g2d.draw(line)
          g2d.setStroke(stroke)
        }
        g2d.draw(line)
      }
      g2d.setColor(Color.RED)
      g2d.setStroke(new BasicStroke((2.0/ownDefaultAT.getScaleX).toFloat))
      g2d.draw(baseCut)
      g2d.setColor(Color.BLUE)
      
      resultAT match {
        case Some(value) =>
          g2d.transform(value)
          g2d.draw(baseCut)
        case None =>
          pointTwo match {
            case None =>
              pointOne.foreach(pt=> g2d.draw(new Line2D.Double(pt,mousePoint.next())))
            case Some(ptTwo) =>
              pointOne.foreach(ptOne=> g2d.draw(new Line2D.Double(ptOne,ptTwo)))
          }
      }
      
      g2d.setTransform(ownDefaultAT)
      g2d.setTransform(new AffineTransform(2,0,0,2,0,0))
      g2d.setColor(Color.DARK_GRAY)
      g2d.drawString("Левая кнопка мыши - поставить одну точку, правая - другую",10,10)
    }
  }
  object ATChooseComponent{
    import math._
    trait ATSub{
      def atChanged(xT:AffineTransform):Unit
    }
    
    
    def concatenate(xsT:AffineTransform*):AffineTransform = {
      val result = new AffineTransform()
      xsT.foreach(result.concatenate)
      result
    }
    def ->(line:Line2D,xT:AffineTransform):Line2D = {
      new Line2D.Double(
        xT.transform(line.getP1,null),
        xT.transform(line.getP2,null)
      )
    }
    def rotation(fi:Double):AffineTransform = {
      new AffineTransform(cos(fi),sin(fi),-sin(fi),cos(fi),0,0)
    }

    def rotation(cos: Double, sin: Double): AffineTransform = {
      new AffineTransform(cos, -sin, sin, cos, 0, 0)
    }
    def scale(nx:Double,my:Double): AffineTransform = {
      new AffineTransform(nx,0,0,my,0,0)
    }
    def shift(dx:Double,dy:Double): AffineTransform = {
      new AffineTransform(1,0,0,1,dx,dy)
    }
    def length(line:Line2D):Double = {
      val lx = line.getX2 - line.getX1
      val ly = line.getY2 - line.getY1
      sqrt(lx*lx + ly*ly)
    }
    def getAT(baseLine:Line2D,imageLine:Line2D):AffineTransform = {
      val (x0, y0, x1, y1) = (
        baseLine.getX1 - baseLine.getX2,
        baseLine.getY1 - baseLine.getY2,
        imageLine.getX1 - imageLine.getX2,
        imageLine.getX1 - imageLine.getX2
      )
      val fi = {
        val cosFi = (x0*x1 + y0*y1)/(length(baseLine)*length(imageLine))
        //через косинус
        val fi0 = acos(cosFi)
        //через синус
        val fi1 = acos(cosFi) + Pi/4
        println(fi1)
        if(cos(fi1) < 0) fi0 else -fi0
      }
      val(rx0,ry0) = (y0,-x0)
      val xT = concatenate(
        shift(imageLine.getX1,imageLine.getY1),
        scale(length(imageLine)/length(baseLine),length(imageLine)/length(baseLine)),
        rotation(
          fi
          //(x0*x1 + y0*y1)/(length(baseLine)*length(imageLine)),
          //(rx0*x1 + ry0*y1)/(length(baseLine)*length(imageLine))
        )
        )
      xT
    }
  }
  
  //atChooseComponent
def main( args: Array[ String ] ): Unit = {
  val frame = JFrameBasics.jFrame
  //относительный размер единицы
  var unitRelativeSize = 50
  
  val componentCenter = (
    frame.getBounds.width/2,
    frame.getBounds.height/2
  )
  val gridT = new AffineTransform(1,0,0,-1,componentCenter._1,componentCenter._2)
  gridT.concatenate(new AffineTransform(unitRelativeSize,0,0,unitRelativeSize,0,0))
  val line0 = new Line2D.Double(0,0,1,0)
  val line1 = new Line2D.Double(3,4,5,6)
  val resultTransform = ATChooseComponent.getAT(line0,line1)
  
//  val component = new JComponent {
//    import ATChooseComponent.->
//    override def paintComponent( g: Graphics ): Unit = {
//      super.paintComponent(g)
//      val g2d = g.asInstanceOf[Graphics2D]
//      g2d.setStroke(new BasicStroke(0))
//      g2d.setTransform(gridT)
//
//      g2d.setColor(Color.GRAY)
//      grid(10).foreach { line =>
//        if(line.getX1==0 || line.getY1==0) {
//          val stroke = g2d.getStroke
//          g2d.setStroke(new BasicStroke((3.0/unitRelativeSize).toFloat))
//          g2d.draw(line)
//          g2d.setStroke(stroke)
//        }
//        g2d.draw(line)
//      }
//      g2d.setColor(Color.BLUE)
//
//      g2d.draw(line0)
//      //g2d.draw(line1)
//      g2d.setColor(Color.RED)
//      g2d.draw(->(line0, resultTransform))
//    }
//  }
  val component = new ATChooseComponent(4)
  frame.add(
    component
  )
  component.repaint()
  frame.revalidate()
}
  

}
