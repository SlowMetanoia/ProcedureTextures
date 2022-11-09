import PaintFrame._

import java.awt._
import java.awt.event._
import java.awt.geom.AffineTransform
import javax.swing.{ JComponent, JFrame }
import scala.swing._

class PaintFrame(
                  backgroundColor:Color = Color.BLACK,
                  linesColor:Color = Color.YELLOW,
                  shapes:Seq[Shape],
                  var transformCalc:Seq[Shape] => AffineTransform = null
                ) extends JFrame{
  if(transformCalc == null) transformCalc = defaultCalc(_,this)
  setTitle("Painting")
  setVisible(true)
  setDefaultCloseOperation(1)
  setBounds(dimension.width/2 - windowSize._1/2,
                   dimension.height/2 - windowSize._2/2,
                   windowSize._1,
                   windowSize._2)
  object component extends JComponent{
    def transform: AffineTransform = transformCalc(shapes)
    override def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.fillRect(-3000,-3000,6000,6000)
      g2d.setTransform(transform)
      g2d.setColor(linesColor)
      g2d.setStroke(new BasicStroke(0))
      shapes.foreach(g2d.draw)
    }
  }
  add(component)
  revalidate()
  addMouseListener(new MouseAdapter {
    override def mouseClicked( e: MouseEvent ): Unit = {
      super.mouseClicked(e)
      component.repaint()
      revalidate()
    }
  })
}
object PaintFrame{
  val dimension: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  val windowSize: (Int, Int) = (1300,700)
  def biggestVector(shape: Shape): Double = {
    val rect = shape.getBounds2D
    val maxX = rect.getMaxX
    val maxY = rect.getMaxY
    if(maxX> maxY) maxX else maxY
  }
  def defaultCalc(shapes:Seq[Shape],frame:JFrame):AffineTransform = {
    if(shapes.nonEmpty) {
      val maximum = shapes.map(biggestVector).max
      val fsize = if(frame.getWidth<frame.getHeight) frame.getWidth else frame.getHeight
      val c =  fsize / ( 2 * maximum + 1 )
      val xT = new AffineTransform(1, 0, 0, 1, windowSize._1 / 2, windowSize._2 / 2)
      xT.concatenate(new AffineTransform(c, 0, 0, -c, 0, 0))
      xT
    }
    else new AffineTransform()
  }
}
