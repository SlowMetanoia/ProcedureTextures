import ParticleSworn.{Particle, Sworn}
import pkg.SliderInit

import java.awt.{BasicStroke, Color, Graphics}
import java.awt.geom.{Ellipse2D, Point2D}
import javax.swing.{JButton, JComponent, JPanel}
import scala.swing.{Color, Dimension, Graphics2D}

class SwornDrawer(
                   sworn: Sworn,
                   var delay:Int = 1000,
                   backgroundColor: Color = Color.BLACK,
                   particleColor: Particle=>Color = _=>Color.YELLOW,
                   var particlePosition: Particle=>Point2D,
                   var particles: Seq[Particle]
                 ) extends JPanel{
    val drawComponent: JComponent = new JComponent {
    setMinimumSize(new Dimension(600,600))
    setMaximumSize(new Dimension(1000,1000))
    setPreferredSize(new Dimension(600,600))
    //рисовашки
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      g2d.setStroke(new BasicStroke(0))
      //фон
      g2d.setColor(backgroundColor)
      g2d.fillRect(-3000,-3000,6000,6000)
      //personal Best's
      g2d.setColor(Color.BLUE)
      particles
        .foreach{ p=>
          val selfOptimalPoint = particlePosition(Particle(
            p.personalBestPosition, Seq.empty, Seq.empty, 0))
          g2d.fill(new Ellipse2D.Double(selfOptimalPoint.getX,selfOptimalPoint.getY,5,5))
        }
      //particles
      particles
        .foreach{ p=>
          g2d.setColor(particleColor(p))
          val pp = particlePosition(p)
          g2d.fill(new Ellipse2D.Double(pp.getX,pp.getY,5,5))
        }
      g2d.setColor(Color.RED)
      val globalOptimumPoint = particlePosition(Particle(sworn.generalBest._2,Seq.empty,Seq.empty,0))
      g2d.fill(new Ellipse2D.Double(globalOptimumPoint.getX,globalOptimumPoint.getY,3,3))
    }
  }
  val sliders = Seq(
    new SliderInit(0,1000,delay,"Delay",n=>delay=n),
    new SliderInit(-1000,1000,(sworn.momentum*100).toInt,"momentum*100",n=>sworn.momentum = n /100.0),
    new SliderInit(-1000,1000,(sworn.collectivism*100).toInt,"collectivism*100",n=>sworn.collectivism = n /100.0),
    new SliderInit(-1000,1000,(sworn.individualism*100).toInt,"individualism*100",n=>sworn.individualism = n /100.0),
  )
  val slidersPanel: JPanel = pkg.slidersPanel(sliders,"params")

  val resetButton = new JButton("Reset")
  resetButton.addActionListener(_ => {
    sworn.reset()
  })
  val minimizingButton = new JButton(s"isMinimizing: ${sworn.isMinimizing}")
  minimizingButton.addActionListener(_ => {
    sworn.isMinimizing = !sworn.isMinimizing
    minimizingButton.setText(s"isMinimizing: ${sworn.isMinimizing}")
    sworn.reset()
  })


  add(minimizingButton)
  add(resetButton)
  add(slidersPanel)
  add(drawComponent)
  repaint()
  def makeStep(): Unit ={
    Thread.sleep(delay)
    particles = sworn.makeStep()
    drawComponent.repaint()
  }
}