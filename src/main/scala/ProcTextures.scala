import pkg._

import java.awt.geom.{ Ellipse2D, Line2D, Point2D, Rectangle2D }
import java.awt.{ BasicStroke, Color, Graphics }
import java.lang.Thread.sleep
import javax.swing.{ JButton, JComponent, JLabel, JPanel }
import scala.concurrent.ExecutionContext._
import scala.concurrent.Future
import scala.math.{ Pi, cos, sin, sqrt }
import scala.swing.{ Dimension, Graphics2D }
import scala.util.Random

object ProcTextures {
  //--------------------------------------------------Режимы рисования------------------------------------------------
  sealed trait TextureMode
  final object GRID extends TextureMode
  final object BRICKS extends TextureMode
  final object MULTICOLOR_GRID extends TextureMode {
    var cellColors: Seq[Color] = Seq(
      Color.RED, Color.BLUE, Color.WHITE, Color.CYAN, Color.GREEN, Color.GRAY, Color.MAGENTA, Color.DARK_GRAY
    )
  }
  final object STAR_SKY extends TextureMode
  final object GRADIENT extends TextureMode
  final object WAVES extends TextureMode
  final object LINES extends TextureMode
  final object GRADIENT_WAVES extends TextureMode
  final object FILLING_PULSE extends TextureMode
  final object DRAWING_PULSE extends TextureMode
 


  //-------------------------------------------Рисовальщик текстур----------------------------------------------------
  object textureDrawer extends JComponent {
    val r = new Random
    var paintMe: Graphics2D => Unit = _ => ()
    setBounds(0, 5, 700, 670)

    def stars(g2d: Graphics2D, n: Int): Unit = {
      for (_ <- 1 to n) {
        g2d.fillOval((r.nextDouble() * 700).intValue(), (r.nextDouble() * 670).intValue(), 3, 3)
      }
    }

    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      paintMe(g2d)
    }
  }


  def main(args:Array[String]):Unit ={
    //------------------------------------------Параметры рисования-----------------------------------------------------
    var gridColor = Color.BLACK
    var cellColor = Color.WHITE
    var starsColor = Color.LIGHT_GRAY
    var outlineWidth = 3
    var cellSize = 80
    var brickHeight = 40
    var brickShift = 50
    var starNumber = 400
    var rotation:Double = 0
    var scaleX:Double = 1
    var scaleY:Double = 1
    var shift:Double = 0
    var mode: TextureMode = FILLING_PULSE
    var functionDistance = 150
    
    
    //-----------------------------------------------Дополнительные фишки для пульсации---------------------------------
  
    var tickTime = 10
    val pulse = Future[Unit]{
      while(true){
        sleep(tickTime)
        if(mode == FILLING_PULSE || mode == DRAWING_PULSE){
          textureDrawer.repaint()
          //println("piu")
        }
      }
    }(global)
    
    var pulseDiff = 1
    var colorDiff = 5
    val pulsingValues = {
      val initial = new Ellipse2D.Double(350, 350, 1, 1)
      def initialColor = cellColor
      val initColorItr: Iterator[Color ] = new Iterator[Color]{
        var alfa = 255
        override def hasNext: Boolean = true
        override def next( ): Color = {
          alfa = (alfa + 2*colorDiff)%255
          initialColor match {
            case pkg.Color(r,g,b,_)=>
              new Color(r,g,b, alfa)
          }
        }
      }
      iteratorSeries[ Seq[ (Ellipse2D,Color) ] ](Seq((initial,initialColor))){ shapes=>
        shapes.map{
          case(ellipse,color) =>
            (
              new Ellipse2D.Double(ellipse.getX-pulseDiff.toFloat/2,
                                   ellipse.getY-pulseDiff.toFloat/2,
                                   ellipse.getWidth + pulseDiff,
                                   ellipse.getHeight + pulseDiff),
              color match { case pkg.Color(r,g,b,a) => new Color(r,g,b,(a+colorDiff)%255) }
            )
        }.appended((initial,initColorItr.next())).filter{case(ellipse,_)=>ellipse.getWidth<1050}
      }
    }
    //-----------------------------------------Вспомогательные функции--------------------------------------------------
    def functionLines(f: Double => Double, step: Double): Iterator[Line2D] = {
      series[Point2D](new Point2D.Double(0, f(0)))(pt => new Point2D.Double(pt.getX + step, f(pt.getX + step)))
        .sliding(2).map { case LazyList(pt1, pt2) => new Line2D.Double(pt1, pt2) }
    }
    def multiFunctionalLines(
                              f: Double => Double,
                              step: Double,
                              distance: Double,
                            ):Iterator[Line2D] = {
      val zeroF = functionLines(f, step).takeWhile(_.getX1 < 3000)
      //сдвиги
      val shifts = series[Double](0)(_ + distance)
      //линии со сдвигами
      val shifted = zeroF.flatMap { ln =>
        shifts.takeWhile(_ < 3000).map { y =>
          new Line2D.Double(
            ln.getX1,
            ln.getY1 + y,
            ln.getX2,
            ln.getY2 + y
            )
        }
      }
      shifted
    }
    def gradLines(
                   f: Double => Double,
                   step: Double,
                   distance: Double,
                   parts: Int
                 ) = {
      val zeroF = functionLines(f, step).takeWhile(_.getX1*scaleX < 1000)
      val gradLines = zeroF.map{ line=>
        val d = distance/parts
        for (i<-0 to parts) yield
          new Line2D.Double(
            line.getX1,
            line.getY1 + d*i,
            line.getX2,
            line.getY2 + d*i
            )
      }
      gradLines
    }
    def drawMultiFuncLines(g2d: Graphics2D,
                           f: Double => Double,
                           step: Double,
                           fColor: Color,
                           rotationAng: Double,
                           distance: Double,
                           stroke: Double = 0): Unit = {
      val shifted = multiFunctionalLines(f,step, distance)
      val xT = pkg.rotation(rotationAng)
      xT.concatenate(pkg.scale(scaleX,scaleY))
      g2d.setStroke(new BasicStroke(stroke.toFloat))
      g2d.setColor(fColor)
      g2d.setTransform(xT)
      shifted.foreach(g2d.draw)
    }
    
    def drawGradient(g2d: Graphics2D,
                     f: Double => Double,
                     step: Double,
                     fColor: Color,
                     rotationAng: Double,
                     distance: Double,
                     stroke: Double = 0,
                     parts: Int = 40
                    ): Unit = {
      val lines = gradLines(f,step,distance,parts * 2)
      val shifts = series[Double](0)(_ + distance).takeWhile(_*scaleY<2000)
      val palette = {
        val palette = for (i <- 0 until parts) yield new Color(
          fColor.getRed,
          fColor.getGreen,
          fColor.getBlue,
          ((i.toFloat / parts) * 255).intValue()
          )
        palette.reverse ++ palette
      }
      
      val xT = pkg.rotation(rotationAng)
      xT.concatenate(pkg.scale(scaleX,scaleY))
      g2d.setStroke(new BasicStroke(stroke.toFloat))
      g2d.setTransform(xT)
      lines.foreach{gradLns=>
        gradLns
          .zip(palette)
          .foreach{ case (ln,color)=>
            g2d.setColor(color)
            shifts.foreach{shift=>
              g2d.draw(
                new Line2D.Double(ln.getX1,
                                  ln.getY1 + shift,
                                  ln.getX2,
                                  ln.getY2 + shift))}}}
    }
    //--------------------------------------------Кирпичный заводик-----------------------------------------------------
    def sqr(d: Double):Double = d*d
    def ptDistance(pt1:(Double,Double),pt2:(Double,Double)) = sqrt(sqr(pt2._1 - pt1._1) + sqr(pt2._2-pt1._2))
    def rectByCenter(x:Double,y:Double,w:Double,h:Double):Rectangle2D = {
      new Rectangle2D.Double(
        x-w/2,
        y-h/2,
        w,
        h
      )
    }
    def MOE(x:Int,lim:Int):Int = if(x>lim) x else lim
    def LOE(x:Int,lim:Int):Int = if(x<lim) x else lim
    def drawSpots(
               x:Double,
               y:Double,
               w:Double,
               h:Double,
               trys:Int,
               minDistance:Double
             ):Seq[Ellipse2D] = {
      ???
    }
  
    def drawCement(
                    x:Double,
                    y:Double,
                    w:Double,
                    h:Double,
                    g2d: Graphics2D,
                    Color:Color,
                    bubblesSize:Double,
                    bubblesRarity:Double,
                    orientation:Boolean,
                    bubblesColorRule:Color=>Color,
                  ):Unit = {
      val r = new Random()
      
      //Закрасим всё.
      g2d.setColor(Color)
      g2d.fill(new Rectangle2D.Double(x,y,w,h))
      
      //добавим пузырьков
      var bubbles = Seq.empty[(Double,Double)]
      for(_<-0 to 1000){
        val (x,y) = (r.nextDouble()*w,r.nextDouble()*h)
        if(bubbles.forall(ptDistance(_,(x,y))>bubblesSize*bubblesRarity))
          bubbles = bubbles.appended((x,y))
      }
      g2d.setColor(bubblesColorRule(Color))
      bubbles.foreach { case ( (x, y) ) =>
        g2d.draw(new Ellipse2D.Double(x,y,bubblesSize,bubblesSize))
      }
      //"скруглим" цемент
      g2d.setColor(new Color(255,255,255,3))
      val d = 0.1
      if(orientation){
        var i:Double = 0
        while (i < Pi/2) {
          if(orientation)
            g2d.draw(rectByCenter(x + w / 2, y + w / 2, w*cos(i), h))
          else
            g2d.draw(rectByCenter(x + w / 2, y + w / 2, w, h*cos(i)))
          i+=d
        }
      }
      
    }
    def drawBrick(
                   x:Double,
                   y:Double,
                   w:Double,
                   h:Double,
                   g2d: Graphics2D,
                   brickColor:Color,
                   cementColor:Color,
                   irregularityColorRule:Color=>Color,
                   crackColorRule:Color=>Color,
                   crackChance:Double,
                   brickColorDeviation:Int,
                   cementWidth:Double,
                   cementWidthDeviation:Double
                 ):Unit = {
      val r = new Random()
      //рисуем цемент
      def cementDistortion:Color=>Color =
      {case pkg.Color(r,g,b,_) => new Color(
        LOE(0,r-50),
        LOE(0,g-50),
        LOE(0,b-50),
        100
        )}
      //правый слой
      val wd = cementWidth + r.nextDouble() * 2 * cementWidthDeviation - cementWidthDeviation
      drawCement(x+w-wd,y,wd,h,g2d,cementColor, 2, 1, orientation = true, cementDistortion)
      //нижний слой
      val hd = cementWidth + r.nextDouble() * 2 * cementWidthDeviation - cementWidthDeviation
      drawCement(x,y+h-hd,w,hd,g2d,cementColor, 2, 1, orientation = false, cementDistortion)
      //рисуем кирпич
      g2d.setColor(brickColor)
      g2d.fill(new Rectangle2D.Double(x,y,w,h))
      //рисуем вкрапления
      g2d.setColor(crackColorRule(brickColor))
      //...Если успею
    }
    
    //--------------------------------------Раздутая функция отрисовки--------------------------------------------------


    def drawTextures(): Unit = {
      val square: (Double, Double) => Rectangle2D = new Rectangle2D.Double(_, _, cellSize, cellSize)
      mode match {
        case STAR_SKY =>
          textureDrawer.paintMe = g2d => {
            g2d.setColor(gridColor)
            g2d.fill(textureDrawer.getBounds)
            g2d.setColor(starsColor)
            textureDrawer.stars(g2d, starNumber)
          }
        case GRID => textureDrawer.paintMe = g2d => {

          val xs = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getWidth)
          val ys = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getHeight)
          g2d.setColor(cellColor)
          xs.foreach { x =>
            ys.foreach { y =>
              g2d.fill(square(x, y))
            }
          }
          g2d.setStroke(new BasicStroke(outlineWidth))
          g2d.setColor(gridColor)
          xs.foreach { x =>
            ys.foreach { y =>
              g2d.draw(square(x, y))
            }
          }
        }
        case MULTICOLOR_GRID => textureDrawer.paintMe = g2d => {
          val xs = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getWidth)
          val ys = series[Int](0)(_ + cellSize).takeWhile(_ < textureDrawer.getHeight)

          for {
            i <- xs.indices
            j <- ys.indices
          } {
            g2d.setColor(MULTICOLOR_GRID.cellColors((i + j) % MULTICOLOR_GRID.cellColors.length))
            g2d.fill(square(xs(i), ys(j)))
          }
          g2d.setColor(gridColor)
          g2d.setStroke(new BasicStroke(outlineWidth))
          for {
            i <- xs.indices
            j <- ys.indices
          } g2d.draw(square(xs(i), ys(j)))
        }
        case BRICKS => textureDrawer.paintMe = g2d => {
          val rectangle: (Double, Double) => Rectangle2D = new Rectangle2D.Double(_, _, cellSize, brickHeight)
          val xs = series[Int](0)(_ + cellSize).takeWhile(_ < (textureDrawer.getWidth + cellSize))
          val ys = series[Int](0)(_ + brickHeight).takeWhile(_ < textureDrawer.getHeight)
          g2d.setColor(cellColor)
          for {i <- xs.indices; j <- ys.indices}
            if (j % 2 == 0) {
              g2d.fill(rectangle(xs(i), ys(j)))
            }else {
              g2d.fill(rectangle(xs(i) - cellSize * brickShift / 100, ys(j)))
            }

          g2d.setColor(starsColor)
          textureDrawer.stars(g2d, starNumber)

          g2d.setColor(gridColor)
          g2d.setStroke(new BasicStroke(outlineWidth))
          for {
            i <- xs.indices
            j <- ys.indices
          } if (j % 2 == 0) g2d.draw(rectangle(xs(i), ys(j))) else g2d.draw(rectangle(xs(i) - cellSize * brickShift / 100, ys(j)))
        }
        case GRADIENT => textureDrawer.paintMe = g2d => {
          
          //g2d.setTransform(pkg.rotation(rotation))
          
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)

          for {
            x <- -3000 to 6000
          } {
            g2d.setColor(new Color(cellColor.getRed, cellColor.getGreen, cellColor.getBlue, math.abs(x) % 255))
            g2d.drawLine(x, -3000, x, 6000)
          }
        }
        case WAVES => textureDrawer.paintMe = g2d=> {
          val color = cellColor
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          drawMultiFuncLines(g2d,x=>100*sin(x / 50 - 100)-1000,1,cellColor,rotation,functionDistance,outlineWidth)
        }
        case LINES => textureDrawer.paintMe = g2d=>{
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          drawMultiFuncLines(g2d,x=> x - 1500,1,cellColor,rotation,functionDistance,outlineWidth)
        }
        case GRADIENT_WAVES => textureDrawer.paintMe = g2d=>{
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          drawGradient(g2d,x=>100*sin(x / 50 - 100)-1000,1,cellColor,rotation,functionDistance,outlineWidth)
        }
        case FILLING_PULSE => textureDrawer.paintMe = g2d=>{
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          pulsingValues.next()
                       .foreach{
                         case(shape,color)=>
                           g2d.setColor(color)
                           g2d.fill(shape)
                       }
        }
        case DRAWING_PULSE => textureDrawer.paintMe = g2d=>{
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          pulsingValues.next()
                       .foreach{
                         case(shape,color)=>
                           g2d.setColor(color)
                           g2d.draw(shape)
                       }
        }
      }
      textureDrawer.repaint()
    }

    val jFrame = JFrameBasics.jFrame
    val jPanel = new JPanel()
    jPanel.setBounds(700, 5, 600, 700)
    jPanel.add(new JLabel("Texture mode"))

    //----------------------------------------------------Кнопки--------------------------------------------------------
    val brickButton = new JButton("Bricks")
    brickButton.addActionListener(_ => {
      mode = BRICKS
      drawTextures()
    })
    val plainColorButton = new JButton("Star sky")
    plainColorButton.addActionListener(_ => {
      mode = STAR_SKY
      drawTextures()
    })
    val gridButton = new JButton("Grid")
    gridButton.addActionListener(_ => {
      mode = GRID
      drawTextures()
    })
    val multicolorGridButton = new JButton("Multicolor grid")
    multicolorGridButton.addActionListener(_ => {
      mode = MULTICOLOR_GRID
      drawTextures()
    })
    val gradientButton = new JButton("Gradients")
    gradientButton.addActionListener(_=>{
        mode = GRADIENT
        drawTextures()
      })
    val waveButton = new JButton("Waves")
    waveButton.addActionListener(_=>{
      mode = WAVES
      drawTextures()
    })
    val linesButton = new JButton("Lines")
    linesButton.addActionListener(_=>{
      mode = LINES
      drawTextures()
    })
    val gradientWavesButton = new JButton("Gradient Waves")
    gradientWavesButton.addActionListener(_=>{
      mode = GRADIENT_WAVES
      drawTextures()
    })
    val fillingPulseButton = new JButton("Filling Pulse")
    fillingPulseButton.addActionListener(_=>{
      mode = FILLING_PULSE
      drawTextures()
    })
    val drawingPulseButton = new JButton("Countering Pulse")
    drawingPulseButton.addActionListener(_=>{
      mode = DRAWING_PULSE
      drawTextures()
    })


    val buttonsPanel = new JPanel() {
      setPreferredSize(new Dimension(600,70))
      //setMinimumSize(new Dimension())
      add(brickButton)
      add(plainColorButton)
      //add(gridButton)
      //add(multicolorGridButton)
      //add(gradientButton)
      add(waveButton)
      //add(linesButton)
      add(gradientWavesButton)
      add(fillingPulseButton)
      add(drawingPulseButton)
    }

    jPanel.add(buttonsPanel)
    //-----------------------------------------------------Слайдеры-----------------------------------------------------
    val gridColorSliders = Seq(
      new SliderInit(0, 255, 0, "Red", { value => gridColor = new Color(value, gridColor.getGreen, gridColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, 0, "Green", { value => gridColor = new Color(gridColor.getRed, value, gridColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, 0, "Blue", { value => gridColor = new Color(gridColor.getRed, gridColor.getGreen, value); textureDrawer.repaint() }, true)
    )
    val cellColorSliders = Seq(
      new SliderInit(0, 255, cellColor.getRed, "Red", { value => cellColor = new Color(value, cellColor.getGreen, cellColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, cellColor.getGreen, "Green", { value => cellColor = new Color(cellColor.getRed, value, cellColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, cellColor.getBlue, "Blue", { value => cellColor = new Color(cellColor.getRed, cellColor.getGreen, value); textureDrawer.repaint() }, true)
    )
    val starsColorSliders = Seq(
      new SliderInit(0,255,starsColor.getRed, "Red", { value=> starsColor = new Color(value,starsColor.getGreen, starsColor.getBlue); textureDrawer.repaint()}),
      new SliderInit(0,255,starsColor.getGreen, "Green", { value=> starsColor = new Color(starsColor.getRed,value, starsColor.getBlue); textureDrawer.repaint()}),
      new SliderInit(0,255,starsColor.getBlue, "Red", { value=> starsColor = new Color(starsColor.getRed,starsColor.getGreen, value); textureDrawer.repaint()}),
      new SliderInit(0,999,starNumber, "Number", { value=> starNumber = value; textureDrawer.repaint()})
    )
    val cellSettingsSliders = Seq(
      new SliderInit(5, 100, cellSize, "Cell size", { value => cellSize = value; textureDrawer.repaint() }, true),
      new SliderInit(1, 10, outlineWidth, "Outline width", { value => outlineWidth = value; textureDrawer.repaint() }, true),
      new SliderInit(5, 100, brickHeight, "Brick height", { value => brickHeight = value; textureDrawer.repaint() }, true),
      new SliderInit(0, 100, 50, "Percent of bricks shift", { value => brickShift = value; textureDrawer.repaint() }, true)
    )
    val functionPrintingSliders = Seq(
      new SliderInit(0,90,rotation.intValue(), "Rotation angle",{ value => rotation = Pi/180*value;textureDrawer.repaint()}),
      new SliderInit(100,999,(scaleX*100).intValue(), "Scale X", {value => scaleX = value/100;textureDrawer.repaint()}),
      new SliderInit(100,999,(scaleY*100).intValue(), "Scale Y", {value => scaleY = value/100;textureDrawer.repaint()}),
      //new SliderInit(0,999,shift.intValue(), "Shift", {value => shift = value;textureDrawer.repaint()})
      new SliderInit(20,500,functionDistance.intValue(),"In between distance",{value => functionDistance = value;textureDrawer.repaint()})
    )
    
    val pulseSliders = Seq(
      new SliderInit(0,300,tickTime,"Tick time",{value=> tickTime = value}),
      new SliderInit(1,50,colorDiff,"Color change speed",{value=> colorDiff = value}),
      new SliderInit(1,50,pulseDiff,"Pulse step",{value=> pulseDiff = value}),
    )

    jPanel.add(slidersPanel(gridColorSliders, "Second color"))
    jPanel.add(slidersPanel(cellColorSliders, "First color"))
    jPanel.add(slidersPanel(cellSettingsSliders, "Cell settings"))
    jPanel.add(slidersPanel(starsColorSliders,"Stars settings"))
    jPanel.add(slidersPanel(functionPrintingSliders,"Functions printing settings"))
    jPanel.add(slidersPanel(pulseSliders,"Pulse settings"))
    jPanel.revalidate()

    jFrame.setLayout(null)
    jFrame.add(textureDrawer)
    jFrame.add(jPanel)
    
    drawTextures()
    jPanel.revalidate()
    jFrame.revalidate()
    jFrame.setTitle("Procedure Textures")
  }
}