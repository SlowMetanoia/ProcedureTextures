import ATChooseComponent._
import pkg._

import java.awt.event.{ MouseAdapter, MouseEvent, MouseMotionAdapter }
import java.awt.geom.{ AffineTransform, Line2D, Point2D }
import java.awt._
import javax.swing.{ JButton, JComponent, JPanel }

class ATChooseComponent(var baseLength:Double) extends JComponent{
  
  var isBindingToGrid:Boolean = true
  var atSubs:Seq[ATSub] = Seq.empty
  def baseCut = new Line2D.Double(0,0,baseLength,0)
  
  var pointOne:Option[Point2D] = None
  var pointTwo:Option[Point2D] = None
  var resultAT:Seq[AffineTransform] = Seq.empty
  
  object mousePoint extends Iterator[Point2D]{
    var pt:Point2D = new Point2D.Double(0,0)
    def set(e:MouseEvent):Unit = {
      pt = if(isBindingToGrid) {
        val p = ownDefaultAT.inverseTransform(e.getPoint,null)
        new Point2D.Double(p.getX.round,p.getY.round)
      }
           else ownDefaultAT.inverseTransform(e.getPoint,null)
    }
    override def hasNext: Boolean = true
    override def next( ): Point2D = pt
  }
  
  def setResultAT(scaleX:Double,shareX:Double,shiftX:Double,scaleY:Double,shareY:Double,shiftY:Double): Unit = {
    if(resultAT.isEmpty)
      resultAT.appended(new AffineTransform(scaleX,shareX,scaleY,shareY,shiftX,shiftY))
    else {
      resultAT = resultAT.updated(resultAT.length - 1,
                                  new AffineTransform(scaleX, shareX, scaleY, shareY, shiftX, shiftY))
    }
    repaint()
  }
  
  def addATSub(sub:ATSub): Unit = atSubs = atSubs.appended(sub)
  
  setMinimumSize(new Dimension(400,400))
  setMaximumSize(new Dimension(900,900))
  setPreferredSize(new Dimension(700,700))
  
  addMouseMotionListener(new MouseMotionAdapter {
    override def mouseMoved( e: MouseEvent ): Unit = {
      super.mouseMoved(e)
      mousePoint.set(e)
      repaint()
    }
  })
  
  def addAT( xT:AffineTransform):Unit = {
    resultAT = resultAT.appended(xT)
    atSubs.foreach(_.atChanged(xT))
  }
  
  addMouseListener(new MouseAdapter{
    override def mouseClicked( e: MouseEvent ): Unit = {
      super.mouseClicked(e)
      e.getButton match {
        case MouseEvent.BUTTON1 =>
          if(pointOne.isEmpty) pointOne = Some(mousePoint.next())
          else if(pointTwo.isEmpty) pointTwo = Some(mousePoint.next())
        case MouseEvent.BUTTON3 =>
        //pointTwo = Some(mousePoint.next())
        case _ =>
      }
      (pointOne,pointTwo) match {
        case (Some(pt1),Some(pt2)) =>
          addAT(ATChooseComponent.getAT(baseCut, new Line2D.Double(pt1, pt2)))
          pointOne = None
          pointTwo = None
        case _ =>
      }
      repaint()
    }
  })
  
  def ownDefaultAT: AffineTransform = {
    //начальные границы
    //делаем вид, что область квадратная
    val bounds: Int = if (getBounds().width > getBounds().height) getBounds().height else getBounds().width
    //в компонент единичный отрезок должен поместиться 2*baseLength раз + ещё чуть-чуть
    val unitSize = bounds / (2 * baseLength + 1)
    //сдвиг в центр
    val xt = new AffineTransform(1,0,0,-1,getBounds().width/2,getBounds().height/2)
    //растягивание до нужного размера
    xt.concatenate(new AffineTransform(unitSize,0,0,unitSize,0,0))
    xt
  }
  
  override def paintComponent( g: Graphics ): Unit = {
    super.paintComponent(g)
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.setTransform(ownDefaultAT)
    g2d.setStroke(new BasicStroke(0))
    grid(100).foreach{ line =>
      if(line.getX1==0 || line.getY1==0) {
        val stroke = g2d.getStroke
        g2d.setStroke(new BasicStroke((2.0/ownDefaultAT.getScaleX).toFloat))
        g2d.draw(line)
        g2d.setStroke(stroke)
      }
      g2d.draw(line)
    }
    g2d.setColor(Color.RED)
    g2d.setStroke(new BasicStroke((2.0/ownDefaultAT.getScaleX).toFloat))
    g2d.draw(baseCut)
    g2d.setColor(Color.BLUE)
    
    resultAT.foreach{at=>
      val oldTransform = g2d.getTransform
      g2d.transform(at)
      g2d.draw(baseCut)
      g2d.setTransform(oldTransform)
    }
    
    (pointOne,pointTwo) match {
      case (Some(pt1),None) =>
        g2d.draw(new Line2D.Double(pt1, mousePoint.next()))
      case (None,Some(pt2)) =>
        g2d.draw(new Line2D.Double(mousePoint.next(),pt2))
      case (None,None) =>
    }
    
    g2d.setTransform(ownDefaultAT)
    g2d.setTransform(new AffineTransform(2,0,0,2,0,0))
    g2d.setColor(Color.DARK_GRAY)
//    g2d.drawString("ЛКМ - первая точка",10,10)
//    g2d.drawString("ПКМ - вторая",10,20)
  }
}
object ATChooseComponent{
  import math._
  trait ATSub{
    def atChanged(xT:AffineTransform):Unit
  }
  
  
  def concatenate(xsT:AffineTransform*):AffineTransform = {
    val result = new AffineTransform()
    xsT.foreach(result.concatenate)
    result
  }
  def ->(line:Line2D,xT:AffineTransform):Line2D = {
    new Line2D.Double(
      xT.transform(line.getP1,null),
      xT.transform(line.getP2,null)
      )
  }
  def rotation(fi:Double):AffineTransform = {
    new AffineTransform(cos(fi),sin(fi),-sin(fi),cos(fi),0,0)
  }
  
  def rotation(cos: Double, sin: Double): AffineTransform = {
    new AffineTransform(cos, -sin, sin, cos, 0, 0)
  }
  def scale(nx:Double,my:Double): AffineTransform = {
    new AffineTransform(nx,0,0,my,0,0)
  }
  def shift(dx:Double,dy:Double): AffineTransform = {
    new AffineTransform(1,0,0,1,dx,dy)
  }
  def length(line:Line2D):Double = {
    val lx = line.getX2 - line.getX1
    val ly = line.getY2 - line.getY1
    sqrt(lx*lx + ly*ly)
  }
  def getAT(baseLine:Line2D,imageLine:Line2D):AffineTransform = {
    val (x0, y0, x1, y1) = (
      baseLine.getX2 - baseLine.getX1,
      baseLine.getY2 - baseLine.getY1,
      imageLine.getX2 - imageLine.getX1,
      imageLine.getY2 - imageLine.getY1
    )
    val fi = {
      (if(x0*y1 - y0*x1 >= 0) 1 else -1)*acos((x0*x1 + y0*y1)/(length(baseLine)*length(imageLine)))
    }
    val xT = concatenate(
      shift(imageLine.getX1,imageLine.getY1),
      scale(length(imageLine)/length(baseLine),length(imageLine)/length(baseLine)),
      rotation(
        fi
        )
      )
    xT
  }
  trait ATProvider extends JPanel{
    def getAT:Seq[AffineTransform]
  }
  def getResultPanel(baseLineLength:Int = 3):ATProvider = {
    //Основная панель
    object choosePanel extends ATProvider{
      
      //setMinimumSize(new Dimension(500,500))
      //setMaximumSize(new Dimension(700,700))
      //setPreferredSize(new Dimension(600,600))
      
      val gridBagLayout = new GridBagLayout
      setLayout(gridBagLayout)
      
      val chooseComponent = new ATChooseComponent(baseLineLength)
      val displayPanel = new ATDisplayPanel(chooseComponent.setResultAT)
      
      
      
      val paintingConstraints: GridBagConstraints = getConstraints(0, 2, 7, 7)
      val settingsPanelConstraints: GridBagConstraints = getConstraints(0, 0, 2, 7)
      
      val settingsPanel: JPanel = new JPanel(){
        
//        setMinimumSize(new Dimension(250,400))
//        setPreferredSize(new Dimension(300,700))
//        setMaximumSize(new Dimension(900,900))
        
        val baseLineSlider: JPanel = new SliderInit(1, 10, baseLineLength, "Base line length",
          {baseLineLength =>
            chooseComponent.baseLength = baseLineLength
            chooseComponent.repaint()
          }
                                                    ).getSliderPanel
        val gridBindingSwitch = new JButton("Switch grid binding mode")
        gridBindingSwitch.addActionListener(_=>{
          chooseComponent.isBindingToGrid = !chooseComponent.isBindingToGrid
        })
        val clearButton = new JButton("Clear")
        clearButton.addActionListener(_=>{
          chooseComponent.resultAT = Seq.empty
          chooseComponent.repaint()
        })
        
        add(gridBindingSwitch)
        add(baseLineSlider)
        add(clearButton)
        //add(displayPanel)
      }
      add(settingsPanel,settingsPanelConstraints)
      add(chooseComponent,paintingConstraints)
      
      
      chooseComponent.addATSub(displayPanel)
  
      override def getAT: Seq[AffineTransform] = chooseComponent.resultAT
    }
    choosePanel
  }
  //Панель, отражающая настройки
  class ATDisplayPanel(se:(Double,Double,Double,Double,Double,Double)=>Unit) extends JPanel with ATSub {
    val gridBagLayout = new GridBagLayout
    setLayout(gridBagLayout)
    
    val xConstraints: Seq[GridBagConstraints] = for(i<-0 to 2) yield
      getConstraints(0,i*2,2,4)
    val yConstraints: Seq[GridBagConstraints] = for(i<-0 to 2) yield
      getConstraints(4,i*2,2,4)
    
    val fields: Seq[ NumberTextField ] = {
      //имена и начальные значения
      val names = Seq("scaleX", "shareX", "shiftX", "scaleY", "shareY", "shiftY")
      val inits = Seq(1,0,0,1,0,0)
      var fields = Seq.empty[NumberTextField]
      //Создание полей и их связывание
      fields = names
        .zip(inits)
        .map{
          case (name,init) =>
            new NumberTextField(init,name, _=>{
              val fieldValues = fields
                .map(_.tfValue)
              //проверяем, что с ними всё ок и если действительно ок - вызываем se
              if(fieldValues.forall(_.isDefined)) fieldValues.map(_.get) match {
                case Seq(scaleX,shareX,shiftX,scaleY,shareY,shiftY) =>
                  se(scaleX,shareX,shiftX,scaleY,shareY,shiftY)
              }}
                                )
        }
      fields
    }
    fields
      .zip(xConstraints.appendedAll(yConstraints))
      .foreach{ case (field,constraints) => add(field,constraints) }
    
    override def atChanged( xT: AffineTransform ): Unit ={
      fields.zip(
        Seq(xT.getScaleX,xT.getShearX,xT.getTranslateX,xT.getScaleY,xT.getShearY,xT.getTranslateY)
        ).foreach{
        case (field,value) => field.textField.setText(value.toString)
      }
    }
  }
}
