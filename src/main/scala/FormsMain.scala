import java.awt._
import javax.swing.JPanel
import pkg._

object FormsMain extends App {
  val jFrame = JFrameBasics.jFrame
  
  var (r,g,b) = (0,0,0)
  val jPanel = new JPanel()
  val colorSliders = Seq(
    new SliderInit(0,255,0,"red",{value=>   r = value;jPanel.setBackground(new Color(r,g,b))},true),
    new SliderInit(0,255,0,"green",{value=> g = value;jPanel.setBackground(new Color(r,g,b))},true),
    new SliderInit(0,255,0,"blue",{value=>  b = value;jPanel.setBackground(new Color(r,g,b))},true)
    )
  jPanel.setBackground(new Color(0,0,0))
  jFrame.add(jPanel, BorderLayout.NORTH)
  jFrame.add(slidersPanel(colorSliders, "Choose color"), BorderLayout.CENTER)
  //jFrame.add(new ColorChoosePanel( (x,y,z)=>{jPanel.setBackground(new Color(x,y,z))},"choose color"), BorderLayout.CENTER)
  jFrame.revalidate()
}
