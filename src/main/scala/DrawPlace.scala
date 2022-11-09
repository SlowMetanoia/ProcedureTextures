import DrawPlace.shapesProducer
import pkg.{ getConstraints, grid }

import java.awt.event.{ MouseAdapter, MouseEvent, MouseMotionAdapter }
import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import java.awt._
import javax.swing.{ JButton, JComponent, JPanel }
import scala.swing.{ Dimension, Graphics2D }

class DrawPlace(var transform:AffineTransform = null) extends shapesProducer{
  
  var isAuto: Boolean = transform == null
  
  setMinimumSize(new Dimension(400,400))
  setMaximumSize(new Dimension(800,800))
  setPreferredSize(new Dimension(500,500))
  def recountTransform( ): Unit =
  if(isAuto){
    val transform = new AffineTransform(1,0,0,1,drawablePart.getWidth/2,drawablePart.getHeight/2)
    transform.concatenate(new AffineTransform(40,0,0,-40,0,0))
    setTransform(transform)
  }
  
  def setTransform(affineTransform: AffineTransform):Unit = transform = affineTransform
  
  val gridBagLayout = new GridBagLayout
  setLayout(gridBagLayout)
  val bindingC: GridBagConstraints = getConstraints(0, 0, 1, 18)
  val clearC: GridBagConstraints = getConstraints(18, 0, 1, 17)
  val drawC: GridBagConstraints = getConstraints(0, 1, 34, 35)
  
  
  
  val bindingButton = new JButton("Switch grid binding mode")
  val clearButton = new JButton("Clear")
  var isBinding = true
  
  bindingButton.addActionListener( _=> { isBinding = !isBinding; drawablePart.repaint() })
  clearButton.addActionListener( _ => {
    drawablePart.pt1 = None
    drawablePart.lines = Seq.empty
    drawablePart.repaint()}
                                 )
  
  object drawablePart extends JComponent{
    setMinimumSize(new Dimension(400,400))
    setMaximumSize(new Dimension(700,700))
    setPreferredSize(new Dimension(500,500))
    
    var mousePoint: Point2D = new Point2D.Double(0, 0)
    
    var pt1: Option[ Point2D ] = None
    
    var lines: Seq[ Line2D ] = Seq.empty
    
    addMouseListener(new MouseAdapter {
      override def mouseClicked( e: MouseEvent ): Unit = {
        super.mouseClicked(e)
        if(pt1.isDefined) {
          lines = lines.appended(new Line2D.Double(pt1.get, mousePoint))
          pt1 = None
        }
        else pt1 = Some(mousePoint)
        repaint()
      }
    })
    
    addMouseMotionListener(new MouseMotionAdapter {
      override
      def mouseMoved( e: MouseEvent ): Unit = {
        super.mouseMoved(e)
        mousePoint = if(isBinding) {
          val point = transform.inverseTransform(e.getPoint, null)
          new Point2D.Double(point.getX.round,point.getY.round)
        } else transform.inverseTransform(e.getPoint, null)
        repaint()
      }
    })
    
    override def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      recountTransform()
      val g2d = g.asInstanceOf[ Graphics2D ]
      g2d.drawString(s"$mousePoint",20,20)
      g2d.setTransform(transform)
      g2d.setStroke(JFrameBasics.gridStroke)
      grid(40).foreach {
        line =>
          if(line.intersects(-0.5, -0.5, 0.5, 0.5)) {
            g2d.setStroke(new BasicStroke((3.0/transform.getScaleX).toFloat))
            g2d.draw(line)
            g2d.setStroke(JFrameBasics.gridStroke)
          }
          else
            g2d.draw(line)
      }
      g2d.setColor(Color.RED)
      lines.foreach(g2d.draw)
      g2d.setColor(Color.MAGENTA)
      pt1.foreach(pt => g2d.draw(new Line2D.Double(pt, mousePoint)))
    }
  }
  add(bindingButton,bindingC)
  add(clearButton,clearC)
  add(drawablePart,drawC)
  
  override def getShapes: Seq[ Line2D ] = drawablePart.lines
}
object DrawPlace{
  sealed trait shapesProducer extends JPanel{
    def getShapes:Seq[Line2D]
  }
}
