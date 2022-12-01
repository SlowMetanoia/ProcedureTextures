import JFrameBasics.{dimension, windowSize}

import java.awt.{BasicStroke, Color, Graphics, Shape}
import javax.swing.{JComponent, JFrame}
import scala.swing.{Dimension, Graphics2D}

class FramePainting(
                      backgroundColor: Color = Color.BLACK,
                      linesColor: Color = Color.YELLOW,
                      var shapes: Seq[Shape],
                    )extends JFrame{
  setTitle("Painting")
  setVisible(true)
  setDefaultCloseOperation(1)
  setBounds(
    dimension.width / 2 - windowSize._1 / 2,
    dimension.height / 2 - windowSize._2 / 2,
    windowSize._1,
    windowSize._2
  )
  val drawComponent: JComponent = new JComponent {
    setMinimumSize(new Dimension(600,600))
    setMaximumSize(new Dimension(1000,1000))
    setPreferredSize(new Dimension(600,600))
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setStroke(new BasicStroke(0))
      g2d.setColor(backgroundColor)
      g2d.fillRect(-3000,-3000,6000,6000)
      g2d.setColor(linesColor)
      shapes.foreach(g2d.draw)
    }
  }
  add(drawComponent)
}
