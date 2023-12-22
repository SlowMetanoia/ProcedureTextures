import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import java.awt.{ Color, Dimension, GridBagConstraints, GridBagLayout, Insets, Label }
import javax.swing.{ JLabel, JPanel, JSlider, JTextField }
import scala.math.{ cos, sin, sqrt }

object pkg {
  //ряды
  def series[T](prev: T)(next: T => T): LazyList[T] = prev #:: series(next(prev))(next)
  def iteratorSeries[T](prev: T)(nxt:T=>T):Iterator[T] = new Iterator[T]{
    private var current:T = prev
    override def hasNext: Boolean = true

    override def next(): T = {
      val old = current
      current = nxt(current)
      old
    }
  }
  object Color{
    def unapply( arg: Color ): Option[(Int,Int,Int,Int)] = {
      Some((arg.getRed,arg.getGreen,arg.getBlue,arg.getAlpha))
    }
  }
  def rotation(fi: Double): AffineTransform = {
    new AffineTransform(cos(fi), sin(fi), -sin(fi), cos(fi), 0, 0)
  }

  def rotation(cos: Double, sin: Double): AffineTransform = {
    new AffineTransform(cos, -sin, sin, cos, 0, 0)
  }

  def scale(nx: Double, my: Double): AffineTransform = {
    new AffineTransform(nx, 0, 0, my, 0, 0)
  }

  def shift(dx: Double, dy: Double): AffineTransform = {
    new AffineTransform(1, 0, 0, 1, dx, dy)
  }

  def length(line: Line2D): Double = {
    val lx = line.getX2 - line.getX1
    val ly = line.getY2 - line.getY1
    sqrt(lx * lx + ly * ly)
  }


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
  
  class NumberTextField(var initialText:Double,label:String,se:Double=>Unit) extends JPanel{
    val gridBagLayout = new GridBagLayout
    setLayout(gridBagLayout)
    val lc: GridBagConstraints = getConstraints(0, 0, 1, 2)
    val l = new JLabel(label)
    val tc: GridBagConstraints = getConstraints(0, 1, 1, 2)
    val textField = new JTextField()
    
    textField.setText(("0"*21 + initialText.toString).takeRight(21))
    textField.setMinimumSize(new Dimension(40,10))
    add(l,lc)
    add(textField, tc)
    textField.addPropertyChangeListener(_=>{ tfValue.foreach(se)})
    def tfValue:Option[Double] = textField.getText.toDoubleOption
  }
  
  def paramFunc2Lines(t1:Double,t2:Double,fx:Double=>Double,fy:Double=>Double,parts:Either[Int,Double]):Seq[Line2D] = {
    val points = parts match {
      case Left(n)=> series[Double](t1)((t2-t1)/n + _).takeWhile(_<=t2).map(t=>new Point2D.Double(fx(t),fy(t)))
      case Right(d)=> series[Double](t1)(d + _).takeWhile(_<t2).appended(t2).map(t=>new Point2D.Double(fx(t),fy(t)))
    }
    points
      .sliding(2)
      .map{ case Seq(p1,p2) => new Line2D.Double(p1,p2) }
      .toSeq
  }
  def polynomial(as:Double*):Double=>Double = {
    as
      .zipWithIndex
      .map { case (a, i) => ( x: Double ) => a * math.pow(x, i) }
      .reduce((f1,f2)=>(x:Double)=>f1(x) + f2(x))
  }
  
  class SliderInit( min:Int, max:Int, defVal:Int, label:String, se:Int=>Unit, isValueDisplaying:Boolean = true){
    def getSliderPanel: JPanel = {
      
      val panel = new JPanel(new GridBagLayout)
      
      val sliderC = getConstraints(0,1,1,2)
      val labelC = getConstraints(0,0,1,3)
      val outputC = getConstraints(2,1,1,1)
      
      val slider = new JSlider(min,max,defVal)
      val outLabel = new JLabel(("     " + slider.getValue.toString).takeRight(3))
      
      slider.addChangeListener(_ => se(slider.getValue))
      slider.addChangeListener(_ => outLabel.setText(("     " + slider.getValue.toString).takeRight(5)))
      
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
