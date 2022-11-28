import FuncDrawer.Cut
import breeze.numerics.{ cos, sin }
import pkg.paramFunc2Lines

import java.awt.geom.AffineTransform
import java.awt.{ BasicStroke, Color, Graphics }
import javax.swing.{ JComponent, JPanel }
import scala.swing.{ Dimension, Graphics2D }

class FuncDrawer(cuts:Seq[Cut]) extends JPanel{
  setMaximumSize(new Dimension(1000, 1000))
  setMinimumSize(new Dimension(400,400))
  setPreferredSize(new Dimension(700,700))
  object Drawer extends JComponent{
    setMaximumSize(new Dimension(3000, 3000))
    setMinimumSize(new Dimension(400,400))
    setPreferredSize(new Dimension(1300,700))
    
    override def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setColor(Color.BLACK)
      g2d.fillRect(-3000,-3000,6000,6000)
      g2d.setTransform(new AffineTransform(1,0,0,1,350,350))
      g2d.setColor(Color.YELLOW)
      g2d.setStroke(new BasicStroke(0))
      cuts.flatMap{ case Cut(t1, t2, fx, fy, parts)=> paramFunc2Lines(t1, t2, fx, fy, parts)}
          .foreach(g2d.draw)
    }
  }
  //setLayout(null)
  add(Drawer)
  revalidate()
}
object FuncDrawer{
  case class Cut(t1:Double,t2:Double,fx:Double=>Double,fy:Double=>Double,parts:Either[Int,Double])
  
  def main( args: Array[ String ] ): Unit = {
    val jFrame = JFrameBasics.jFrame
    val fCut = Cut(0,400,t=>40*sin(t/50),t=>40*cos(t/50),Left(600))
    val drawer = new FuncDrawer(Seq(fCut))
    jFrame.add(drawer)
    jFrame.revalidate()
  }
}
