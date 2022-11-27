import breeze.numerics.sqrt
import pkg.getConstraints

import java.awt.event.{MouseAdapter, MouseEvent, MouseMotionAdapter}
import java.awt.geom.{Line2D, Point2D}
import java.awt.{Graphics, GridBagConstraints, GridBagLayout}
import javax.swing.{JButton, JComponent, JPanel}
import scala.swing.{Color, Dimension, Graphics2D}

class TriangleDrawer extends JPanel {
  //-------------------------------------параметры отрисовки----------------------------
  val bgColor = new Color(0,0,0)
  val sourceColor = new Color(0,255,0)
  val nonExistingColor = new Color(255,0,255)
  val color = new Color(255,255,255)


  setMinimumSize(new Dimension(400, 400))
  setMaximumSize(new Dimension(800, 800))
  setPreferredSize(new Dimension(700, 700))

  val gridBagLayout = new GridBagLayout
  setLayout(gridBagLayout)
  val bindingC: GridBagConstraints = getConstraints(0, 0, 1, 18)
  val clearC: GridBagConstraints = getConstraints(18, 0, 1, 17)
  val drawC: GridBagConstraints = getConstraints(0, 1, 34, 35)


  val clearButton = new JButton("Clear")
  clearButton.addActionListener(_=>{
    drawablePart.points = Seq.empty
    drawablePart.repaint()
  })
  
  object drawablePart extends JComponent {
    setMinimumSize(new Dimension(400, 400))
    setMaximumSize(new Dimension(700, 700))
    setPreferredSize(new Dimension(600, 600))

    var mousePoint: Point2D = new Point2D.Double(0, 0)

    var points = Seq.empty[Point2D]

    addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        super.mouseClicked(e)
        points = points.appended(mousePoint)
        repaint()
      }
    })

    addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved(e: MouseEvent): Unit = {
        super.mouseMoved(e)
        val point = e.getPoint
        mousePoint = {
          val matchingPoint = points.find(pt => sqrt(
            (point.getX - pt.getX) * (point.getX - pt.getX) + (point.getY - pt.getY) * (point.getY - pt.getY)
          ) < 5)
          if(matchingPoint.isEmpty) point
          else matchingPoint.get
        }
        repaint()
      }})
    def getTriangles:Seq[Seq[Point2D]] =
      points
        .grouped(3)
        .collect{case Seq(a,b,c) => Seq(a,b,c)}
        .toSeq

    def drawTriangle(points:Seq[Point2D])(implicit g2d:Graphics2D): Unit = points match {
      case Seq(a,b,c) => Seq(
        new Line2D.Double(a,b),
        new Line2D.Double(b,c),
        new Line2D.Double(c,a)
      ).foreach(g2d.draw)
    }

    def getRestPoints:Seq[Point2D] = points.takeRight(points.length%3)

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      implicit val g2d: Graphics2D = g.asInstanceOf[Graphics2D]
      g2d.setColor(bgColor)
      g2d.fillRect(-3000,-3000,6000,6000)
      //рисование треугольников
      getTriangles match {
        case Seq(h, t@ _*)=>
          g2d.setColor(sourceColor)
          drawTriangle(h)
          g2d.setColor(color)
          t.foreach(drawTriangle)
        case Seq()=>
      }
      //рисование по оставшимся точкам
      getRestPoints match {
        case Seq(a,b) =>
          g2d.setColor(nonExistingColor)
          g2d.draw(new Line2D.Double(a,b))
          g2d.draw(new Line2D.Double(b,mousePoint))
          g2d.draw(new Line2D.Double(mousePoint,a))
        case Seq(a)=>
          g2d.setColor(nonExistingColor)
          g2d.draw(new Line2D.Double(a,mousePoint))
        case Seq()=>
      }
    }
  }

  add(clearButton, clearC)
  add(drawablePart, drawC)
  revalidate()
}
object TriangleDrawer{
  def main(args: Array[String]): Unit = {
    val jFrame = JFrameBasics.jFrame
    val td = new TriangleDrawer
    jFrame.add(td)
    jFrame.revalidate()
  }
}
