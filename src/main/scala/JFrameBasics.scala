import java.awt.geom.AffineTransform
import java.awt.{ BasicStroke, Color, Stroke, Toolkit }
import javax.swing.JFrame
import scala.swing.Dimension
import pkg._

object JFrameBasics {
  val jFrame: JFrame = new JFrame()
 
  val dimension: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  
  val relativeScreenSize: (Double, Double)= (0.5, 0.5)
  

  val windowSize: (Int, Int) = (
    1300,
    700
  )
  
  val windowCenter: (Int, Int) = (350, 350)
  
  //Из центра экрана
  val startTransposition = new AffineTransform(1,0,0,-1,windowCenter._1,windowCenter._2)

  //Относительный размер единицы
  val unitRelativeSize = 100
  
  startTransposition.concatenate(new AffineTransform(unitRelativeSize,0,0,unitRelativeSize,0,0))

  //обратное преобразование
  val invertedStartTransposition: AffineTransform = startTransposition.createInverse()

  val stroke: Stroke = new BasicStroke((3.0/unitRelativeSize).toFloat)
  val gridStroke = new BasicStroke(0)
  val mainGridLinesStroke = new BasicStroke((3.0/unitRelativeSize).toFloat)

  val gridColor = new Color(0,200,100,100)

  jFrame.setTitle("MyApp")
  jFrame.setBackground(Color.BLACK)
  jFrame.setVisible(true)
  jFrame.setDefaultCloseOperation(1)
  jFrame.setBounds(dimension.width/2 - windowSize._1/2,
                   dimension.height/2 - windowSize._2/2,
                   windowSize._1,
                   windowSize._2)

}

