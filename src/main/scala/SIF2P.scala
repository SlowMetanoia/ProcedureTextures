import javax.swing.JPanel
import scala.swing.Dimension

class SIF2P extends JPanel{
  object SettingsPanel extends JPanel{
    setBounds(700,0,600,700)
    //setBackground(Color.BLACK)
  }
  
  val drawer = new TriangleDrawer
  setLayout(null)
  drawer.setBounds(0,0,700,700)
  drawer.setPreferredSize(new Dimension(700,700))
  add(drawer)
  add(SettingsPanel)
  revalidate()
  
}
object SIF2P{
  def main( args: Array[ String ] ): Unit = {
    val jFrame = JFrameBasics.jFrame
    val sif = new SIF2P
    jFrame.add(sif)
    jFrame.revalidate()
  }
}
