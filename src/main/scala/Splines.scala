import java.awt.event.ActionEvent
import java.awt.geom.Point2D
import javax.swing.{ JButton, JFrame, JPanel }
import scala.swing.Dimension


object Splines {
  //-------------------------------------------Режимы-------------------------------------------------------------------
  sealed trait MODE
  final object INTERPOLATING extends MODE
  final object ERMIT extends MODE
  final object CYCLIC extends MODE
  final object ACYCLIC extends MODE
  //-------------------------------------------Параметры----------------------------------------------------------------
  var points = Seq.empty[Point2D]
  
  
  var mode:MODE = INTERPOLATING
  val buttonsPanel: JPanel = new JPanel{
    def chmod(mode:MODE):ActionEvent=>Unit = _=>{
      Splines.mode = mode
      drawer.repaint()
    }
    val interpolationButton = new JButton("Interpolating spline")
    interpolationButton.addActionListener(chmod(INTERPOLATING)(_))
    val erimtButton = new JButton("Ermit spline")
    erimtButton.addActionListener(chmod(ERMIT)(_))
    val cyclicButton = new JButton("Cyclic spline")
    cyclicButton.addActionListener(chmod(CYCLIC)(_))
    val acyclicButton = new JButton("Acyclic spline")
    acyclicButton.addActionListener(chmod(ACYCLIC)(_))
  }
  object drawer extends JPanel{
    setMaximumSize(new Dimension(2000,2000))
    setMinimumSize(new Dimension(600,600))
    setPreferredSize(new Dimension(1300,650))
    
  }
  val frame: JFrame = JFrameBasics.jFrame
  def main( args: Array[ String ] ): Unit = {
  
  }
}
