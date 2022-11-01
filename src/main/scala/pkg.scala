import java.awt.geom.Line2D
import java.awt.{ GridBagConstraints, GridBagLayout, Insets, Label }
import javax.swing.{ JLabel, JPanel, JSlider }

object pkg {
  //ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  //сетка
  def grid(size:Int):Seq[Line2D] = {
    (-size to size).flatMap{ i=>
      Seq(
        new Line2D.Double(-size,i,size,i),
        new Line2D.Double(i,-size,i,size)
        )
    }
  }
  //Фабричный метод для констреинта
  def getConstraints(
                      gridx:Int,
                      gridy:Int,
                      gridheight:Int,
                      gridwidth:Int,
                      gridweightx:Int = 0,
                      gridweighty:Int = 0,
                      anchor:Int = 10,
                      fill:Int = 0,
                      insets:Insets = new Insets(0,0,0,0),
                      ipadx:Int = 0,
                      ipady:Int = 0,
                    ): GridBagConstraints = {
    new GridBagConstraints(gridx,gridy,gridwidth,gridheight,gridweightx,gridweighty,anchor,fill,insets,ipadx,ipady)
  }
  
  class SliderInit( min:Int, max:Int, defVal:Int, label:String, se:Int=>Unit, isValueDisplaying:Boolean){
    def getSliderPanel: JPanel = {
      
      val panel = new JPanel(new GridBagLayout)
      
      val sliderC = getConstraints(0,1,1,2)
      val labelC = getConstraints(0,0,1,3)
      val outputC = getConstraints(2,1,1,1)
      
      val slider = new JSlider(min,max,defVal)
      val outLabel = new JLabel(("00" + slider.getValue.toString).takeRight(3))
      
      slider.addChangeListener(_ => se(slider.getValue))
      slider.addChangeListener(_ => outLabel.setText(("00" + slider.getValue.toString).takeRight(3)))
      
      panel.add(new Label(label),labelC)
      panel.add(slider,sliderC)
      if(isValueDisplaying) panel.add(outLabel,outputC)
      
      panel
    }
  }
  
  def slidersPanel(sliders:Seq[SliderInit],label: String):JPanel = {
    val panel = new JPanel(new GridBagLayout)
    sliders
      .indices
      .map(_+1)
      .map(i=> getConstraints(0,i,1,2))
      .zip(sliders)
      .foreach{case (constraints,slider) => panel.add(slider.getSliderPanel,constraints)}
    panel.add(new JLabel(label),getConstraints(0,0,1,2))
    panel
  }
}
