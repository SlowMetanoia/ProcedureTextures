
import pkg._

import java.awt.Shape
import java.awt.geom.{ AffineTransform, Line2D }
import javax.swing.{ JButton, JPanel }
import scala.swing.Dimension

class SIFPanel extends JPanel{
  setMaximumSize(new Dimension(2000, 2000))
  setPreferredSize(new Dimension(1300,700))
  setMinimumSize(new Dimension(400, 400))
  //число итераций
  var iterationsNumber:Int = 5
  
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
  def applyTransformations(transformations:Seq[AffineTransform],shapes:Seq[Line2D]):Seq[Shape] = {
    shapes.flatMap{ shape=>
      transformations.map{transformation=>
        new Line2D.Double(
          transformation.transform(shape.getP1,null),
          transformation.transform(shape.getP2,null)
        )
      }
    }
  }
  
  val itSlider: JPanel =
    new SliderInit(0,100,iterationsNumber,"Iterations number", x => {iterationsNumber = x}).getSliderPanel
    
  itSlider.setMinimumSize(new Dimension(250,40))
  val atChooser: ATChooseComponent.ATProvider = ATChooseComponent.getResultPanel()
  val paintingPanel = new DrawPlace()
  val paintingButton = new JButton("Paint!")
  paintingButton.addActionListener(_=> {
    val frame = new PaintFrame(shapes = {
      val transformations = transformationsList(atChooser.getAT)(iterationsNumber)
      val resultShapes = applyTransformations(transformations,paintingPanel.getShapes)
      resultShapes
    }
                               )
  })
  
  add(atChooser)
  add(paintingPanel)
  paintingPanel.add(itSlider,getConstraints(0,35,1,25))
  paintingPanel.add(paintingButton,getConstraints(25,35,1,10))
  //add(itSlider)
  //add(paintingButton)
  revalidate()
}
object SIFPanel{
  def main( args: Array[ String ] ): Unit = {
    val jFrame = JFrameBasics.jFrame
    val panel = new SIFPanel
    jFrame.add(panel)
    jFrame.revalidate()
  }
}
