import java.awt.event.{ MouseAdapter, MouseEvent }
import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import java.awt.{ BorderLayout, Color, Graphics, Shape }
import javax.swing.{ JButton, JComponent, JPanel, JToggleButton }
import scala.swing.Graphics2D
import pkg._

object SIFMain extends App{
  //Всяческий сетап и рисование формочек
  val jFrame = JFrameBasics.jFrame
  val setupTransformation = JFrameBasics.startTransposition
  val stroke = JFrameBasics.stroke
  //предварительные настройки
  var isBindingToGrid = true
  //панельки
  val workPanel = new JPanel()
  //workPanel.setBounds(0,0,700,700)
  //workPanel.setBackground(new Color(0,0,0,255))
  val workSettingsPanel = new JPanel()
  //workSettingsPanel.setBounds(700,0,600,460)
  //workSettingsPanel.setBackground(Color.GRAY)
  val otherSettingsPanel = new JPanel()
  otherSettingsPanel.add(new JButton("one"))
  otherSettingsPanel.add(new JButton("one"))
  otherSettingsPanel.add(new JButton("one"))
  otherSettingsPanel.add(new JButton("one"))
  //otherSettingsPanel.setBackground(Color.DARK_GRAY)
  //otherSettingsPanel.setBounds(700,460,240,600)

  val transformations = Seq(
    new AffineTransform(1.0 / 3, 0, 0, 1, 0, 0),
    new AffineTransform(1.0 / 3, 0, 0, 1, 2.0 / 3, 0)
    )

  val transformationList = transformationsList(transformations)
  var lastPoint:Option[Point2D] = None
  var cuts:Seq[Line2D] = Seq.empty

  val all =
    generateComponentByTransformations(
      transformationList(4),
      Seq(new Line2D.Double(0, 0, 1, 0))
      )
  
  jFrame.add(all, BorderLayout.CENTER)

  jFrame.add(workSettingsPanel, BorderLayout.WEST)
  jFrame.add(otherSettingsPanel, BorderLayout.WEST)
  workSettingsPanel.revalidate()
  otherSettingsPanel.revalidate()

  val gridBindingButton = new JToggleButton("Включить привязку к сетке")

  gridBindingButton.addActionListener(_ => isBindingToGrid = !isBindingToGrid)

  var mousePoint:Point2D = new Point2D.Double(0,0)

  jFrame.revalidate()

  all.addMouseMotionListener(
    new MouseAdapter {
      override def mouseMoved(e: MouseEvent): Unit = {
        super.mouseMoved(e)
        mousePoint = JFrameBasics.invertedStartTransposition.transform(new Point2D.Double(e.getX,e.getY),null)
        if(isBindingToGrid) mousePoint = new Point2D.Double(mousePoint.getX.round, mousePoint.getY.round)
        all.repaint()
      }
    }
    )
  
  all.addMouseListener(
    new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        super.mouseClicked(e)
        lastPoint match {
          case None =>
            lastPoint = Some(mousePoint)
          case Some(point) =>
            lastPoint = None
            cuts = cuts.appended( new Line2D.Double(point,mousePoint))
        }
      }
    }
    )
  
  
  
  def transformationsList(affineTransforms:Seq[AffineTransform]):LazyList[Seq[AffineTransform]] = {
    series[Seq[AffineTransform]](Seq(new AffineTransform())) { transforms =>
      transforms.flatMap { t0 =>
        affineTransforms.map { t1 =>
          val result = new AffineTransform(t0)
          result.concatenate(t1)
          result
        }
      }
    }
  }

  def generateComponentByTransformations(transforms:Seq[AffineTransform],shapes:Seq[Shape]):JComponent = {
    new JComponent {
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        val g2d = g.asInstanceOf[Graphics2D]
        //дебаг
        g2d.drawString(s"${mousePoint.getX} \n ${mousePoint.getY}",10,10)
        //отрисовка сетки
        g2d.setStroke(JFrameBasics.gridStroke)
        g2d.setColor(JFrameBasics.gridColor)
        g2d.setTransform(setupTransformation)
        grid(10).foreach(
          line=> if(line.intersects(-0.5,-0.5,0.5,0.5)) {
            g2d.setStroke(JFrameBasics.mainGridLinesStroke)
            g2d.draw(line)
            g2d.setStroke(JFrameBasics.gridStroke)
          }
          else
            g2d.draw(line)
        )
        //отрисовка фрактала
        g2d.setColor(Color.BLACK)
        g2d.setStroke(stroke)
        transforms.foreach{ xT=>
          g2d.setTransform(setupTransformation)
          g2d.transform(xT)
          shapes.foreach(g2d.draw)
        }
        //отрисовка отрезков
        g2d.setTransform(setupTransformation)
        cuts.foreach(g2d.draw)
        lastPoint.foreach(point=> g2d.draw(new Line2D.Double(point,mousePoint)))
      }
    }
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