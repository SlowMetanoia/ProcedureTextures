import pkg._

import java.awt.geom.{ Ellipse2D, Line2D, Point2D, Rectangle2D }
import java.awt.{ BasicStroke, Color, Graphics }
import java.lang.Thread.sleep
import javax.swing.{ JButton, JComponent, JFrame, JLabel, JPanel }
import scala.annotation.tailrec
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
  final object BRICK extends TextureMode
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
    var gridColor = new Color(82,99,89)
    var cellColor = new Color(120,96,96)
    var starsColor = Color.LIGHT_GRAY
    var outlineWidth = 3
    var cellSize = 80
    var brickHeight = 40
    var brickShift = 50
    var starNumber = 400
    var rotation:Double = 0
    var scaleX:Double = 1
    var scaleY:Double = 1
    var mode: TextureMode = BRICK
    var functionDistance = 150
    
    
    //-----------------------------------------------Дополнительные фишки для пульсации---------------------------------
  
    var tickTime = 10
    val pulse = Future[Unit]{
      while(true){
        sleep(tickTime)
        if(mode == FILLING_PULSE || mode == DRAWING_PULSE){
          textureDrawer.repaint()
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
    var crackWidth = 1.1
    var crackDistortionAlfaValue = 193
    var crackDistortionDeltaValue = 78
    var spotsPerBrick = 400
    var maximumCrackBrakes = 5
    var crackChance = 0.44
    var brickColorDeviation = 0.3
    var cementWidth = 10
    var cementWidthDeviation = 4
    var cementSpotsNumber = 274
    
    
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
    def MOE(x:Int,lim:Int):Int = if(x<lim) x else lim
    def LOE(x:Int,lim:Int):Int = if(x>lim) x else lim
    def inLim(x:Int,lLim:Int,rLim:Int) = x match {
      case x:Int if x<lLim => lLim
      case x:Int if x>rLim => rLim
      case _ => x
    }
    def incLim = inLim(_,0,255)
    def darkerDistortion:Color=>Color =
    {case pkg.Color(r,g,b,_) =>
      val rand = new Random()
      new Color(
      LOE(0,r-rand.nextInt(200)),
      LOE(0,g-rand.nextInt(200)),
      LOE(0,b-rand.nextInt(200)),
      100
      )}
    def crackDistortion:Color=>Color =
    {case pkg.Color(r,g,b,_) =>
      val rand = new Random()
      new Color(
        LOE(0,r-crackDistortionDeltaValue),
        LOE(0,g-crackDistortionDeltaValue),
        LOE(0,b-crackDistortionDeltaValue),
        crackDistortionAlfaValue
        )}
    def shiftWithRandomRotation(x:Double,y:Double) = {
      val rand = new Random()
      val xT = pkg.shift(x,y)
      xT.concatenate(pkg.rotation(rand.nextDouble() * Pi / 4))
      xT
    }
    def zeroDistortion:Color=>Color = {
      case pkg.Color(r,g,b,_)=>
        val rand = new Random()
        new Color(
          incLim(r+rand.nextInt(100)-50),
          incLim(r+rand.nextInt(100)-50),
          incLim(r+rand.nextInt(100)-50),
          100
        )
    }

    def lineVectorHalf(line: Line2D): Line2D = {
      val centralPoint = new Point2D.Double(
        (line.getX2 - line.getX1) / 2,
        (line.getY2 - line.getY1) / 2
      )
      new Line2D.Double(new Point2D.Double(0,0), centralPoint)
    }
    def spots(
               x:Double,
               y:Double,
               w:Double,
               h:Double,
               trys:Int,
               rLimits:(Int,Int)
             ):Seq[Ellipse2D] = {
      val rand = new Random()
      val canvas = new Rectangle2D.Double(x,y,w,h)
      
      var spots = Seq.empty[Ellipse2D]
      
      for(_<-0 to trys){
        //выбираем точку где-нибудь в нашем прямоугольнике
        val (xx,yy) = (x+rLimits._1+rand.nextDouble()*(w-2*rLimits._1),y+rLimits._1+rand.nextDouble()*(h-2*rLimits._1))
        //выбираем размеры
        val (ww,hh) = (
          rand.nextDouble()*(rLimits._2-rLimits._1)+rLimits._1,
          rand.nextDouble()*(rLimits._2-rLimits._1)+rLimits._1
        )
        spots = spots.appended(
          new Ellipse2D.Double(xx,yy,ww,hh)
          )
      }
      spots
    }
    def crack(bounds:Rectangle2D,
              parts:Int,
              line:Line2D
             ):Seq[Line2D] = {
      val rand = new Random()
      @tailrec
      def crackStep( bounds:Rectangle2D,
                     line: Line2D
                   ):Seq[Line2D] = {
        val fi = Pi * (rand.nextDouble() - 1.0 / 2)
        val xT = pkg.shift(line.getX1, line.getY1)
        xT.concatenate(pkg.rotation(fi))
        val ln = ATChooseComponent.applyTransform(lineVectorHalf(line), xT)
        if (bounds.contains(ln.getBounds2D))
          Seq(ln, new Line2D.Double(ln.getP2, line.getP2))
        else
          crackStep(bounds, line)
      }
      parts match {
        case x:Int if(x<0) => throw new IllegalArgumentException("parts number must be positive")
        case 0 => Seq()
        case 1 => Seq(line)
        case 2 => crackStep(bounds, line)
        case x:Int =>
          crackStep(bounds,line) match {
            case Seq(ln1,ln2) =>
              crack(bounds,x/2,ln1).appendedAll(crack(bounds,x-x/2,ln2))
          }
      }
    }
    def randomDouble(x:Double,y:Double):Double = {
      val r = new Random()
      x + r.nextDouble()*(y-x)
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
      val place = new Rectangle2D.Double(x,y,w,h)
      g2d.setColor(Color)
      g2d.draw(place)
      g2d.fill(place)
      //добавим пузырьков
      val bubbles = spots(x,y,w,h,cementSpotsNumber,(1,2))
      bubbles.foreach({
        g2d.setColor(bubblesColorRule(Color))
        g2d.fill
      })
      //"скруглим" цемент
      g2d.setColor(new Color(0,0,0,3))
      val d = 0.05
      
      var i:Double = 0
      while (i < Pi/2) {
        if(orientation) g2d.fill(new Rectangle2D.Double(x,y,w*cos(i),h))//g2d.draw(rectByCenter(x + w / 2, y + h / 2, w*cos(i), h))
        else g2d.fill(new Rectangle2D.Double(x,y,w,h*cos(i)))//g2d.draw(rectByCenter(x + w / 2, y + h / 2, w, h*cos(i)))
        i+=d
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
                   brickColorDeviation:Double,
                   cementWidth:Double,
                   cementWidthDeviation:Double
                 ):Unit = {
      val rand = new Random()
      //рисуем цемент
      //правый слой
      val wd = cementWidth + rand.nextDouble() * 2 * cementWidthDeviation - cementWidthDeviation
      drawCement(x+w-wd,y,wd,h,g2d,cementColor, 2, 1, orientation = true, darkerDistortion)
      //нижний слой
      val hd = cementWidth + rand.nextDouble() * 2 * cementWidthDeviation - cementWidthDeviation
      drawCement(x,y+h-hd,w,hd,g2d,cementColor, 2, 1, orientation = false, darkerDistortion)

      val finalBrickColor = {
        brickColor match {
          case pkg.Color(r,g,b,_)=>
            new Color(
              incLim(r+(rand.nextDouble()*r*brickColorDeviation).intValue()),
              incLim(g+(rand.nextDouble()*g*brickColorDeviation).intValue()),
              incLim(b+(rand.nextDouble()*b*brickColorDeviation).intValue())
            )
        }
      }

      //рисуем кирпич
      val brick = new Rectangle2D.Double(x,y,w-wd,h-hd)
      g2d.setColor(finalBrickColor)
      g2d.draw(brick)
      g2d.fill(brick)
      //рисуем вкрапления
      val distortions = spots(brick.getX,brick.getY,brick.getWidth,brick.getHeight,spotsPerBrick,(2,4))
      distortions.foreach{d=>
        g2d.setColor(irregularityColorRule(finalBrickColor))
        g2d.fill(d)
      }
      //C некоторым шансом, рисуем трещину
      if(rand.nextDouble()<crackChance){
        val d = 0.0001
        val (xx,yy,ww,hh) = (brick.getX+d,brick.getY+d,brick.getWidth-2*d,brick.getHeight-2*d)
        //случайные точки на сторонах прямоугольника
        def rTop = new Point2D.Double(randomDouble(xx,xx+ww),yy)
        def rBot = new Point2D.Double(randomDouble(xx,xx+ww),yy+hh)
        def rLeft = new Point2D.Double(xx,randomDouble(yy,yy+hh))
        def rRight = new Point2D.Double(xx+ww,randomDouble(yy,yy+hh))
        //выбираем 2 случайные точки
        val rPoints = Seq(rTop,rBot,rLeft,rRight)
        val points = {
          var (x1,x2) = (rand.nextInt(4),rand.nextInt(4))
          while(x1 == x2) {
            x1 = rand.nextInt(4)
          }
          (x1,x2)
        }
        val crackLine = new Line2D.Double(rPoints(points._1),rPoints(points._2))
        g2d.setColor(crackColorRule(brickColor))
        g2d.setStroke(new BasicStroke((crackWidth).toFloat))
        
        crack(brick, rand.nextInt(maximumCrackBrakes+1), crackLine).foreach(g2d.draw)
        
      }
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
          var rect:Rectangle2D = new Rectangle2D.Double(0,0,0,0)
          g2d.setColor(cellColor)
          for {j <- ys.indices;i <- xs.indices} {
            rect = if(j % 2 == 0) rectangle(xs(i), ys(j))
                   else rectangle(xs(i) - cellSize * brickShift / 100, ys(j))
            drawBrick(
              rect.getX,
              rect.getY,
              rect.getWidth,
              rect.getHeight,
              g2d,
              cellColor,
              gridColor,
              darkerDistortion,
              crackDistortion,
              crackChance,
              brickColorDeviation,
              cementWidth,
              cementWidthDeviation
            )
          }
          
          g2d.setColor(starsColor)
          //textureDrawer.stars(g2d, starNumber)

          /*g2d.setColor(gridColor)
          g2d.setStroke(new BasicStroke(outlineWidth))
          for {
            i <- xs.indices
            j <- ys.indices
          } if (j % 2 == 0) g2d.draw(rectangle(xs(i), ys(j))) else g2d.draw(rectangle(xs(i) - cellSize * brickShift / 100, ys(j)))*/
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
        case BRICK => textureDrawer.paintMe = g2d=>{
          drawBrick(
            100,
            100,
            500,
            500,
            g2d,
            cellColor,
            gridColor,
            darkerDistortion,
            crackDistortion,
            crackChance,
            brickColorDeviation,
            cementWidth,
            cementWidthDeviation
            )
        }
      }
      textureDrawer.repaint()
    }

    val jFrame = JFrameBasics.jFrame
    val jPanel = new JPanel()
    jPanel.setBounds(700, 5, 600, 700)
    jPanel.add(new JLabel("Texture mode"))

    //----------------------------------------------------Кнопки--------------------------------------------------------
    val bricksButton = new JButton("Bricks")
    bricksButton.addActionListener(_ => {
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
    val brickButton = new JButton("Brick")
    brickButton.addActionListener(_=>{
      mode = BRICK
      drawTextures()
    })
    val brickSettingsButton = new JButton("Brick settings"){
      setForeground(Color.RED)
    }


    val buttonsPanel = new JPanel() {
      setPreferredSize(new Dimension(600,70))
      //setMinimumSize(new Dimension())
      add(bricksButton)
      add(plainColorButton)
      //add(gridButton)
      //add(multicolorGridButton)
      //add(gradientButton)
      add(waveButton)
      //add(linesButton)
      add(gradientWavesButton)
      add(fillingPulseButton)
      add(drawingPulseButton)
      add(brickButton)
      add(brickSettingsButton)
    }

    jPanel.add(buttonsPanel)
    //-----------------------------------------------------Слайдеры-----------------------------------------------------
    val gridColorSliders = Seq(
      new SliderInit(0, 255, gridColor.getRed, "Red", { value => gridColor = new Color(value, gridColor.getGreen, gridColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, gridColor.getGreen, "Green", { value => gridColor = new Color(gridColor.getRed, value, gridColor.getBlue); textureDrawer.repaint() }, true),
      new SliderInit(0, 255, gridColor.getBlue, "Blue", { value => gridColor = new Color(gridColor.getRed, gridColor.getGreen, value); textureDrawer.repaint() }, true)
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
    
    val secondColorSliders = slidersPanel(gridColorSliders, "Second color")
    val firstColorSliders = slidersPanel(cellColorSliders, "First color")
  
    brickSettingsButton.addActionListener(_=>{
      val settingsFrame = new JFrame()
      settingsFrame.setBounds(jPanel.getBounds)
      settingsFrame.setTitle("Brick settings")
      settingsFrame.setVisible(true)
      settingsFrame.setDefaultCloseOperation(1)
      val panel: JPanel = new JPanel(){
        val settingsSliders = Seq(
          new SliderInit(0,100,(crackWidth*10).intValue(),"Crack width",x=>{crackWidth = x.toDouble/10}),
          new SliderInit(0,255,crackDistortionAlfaValue,"Crack alfa",x=>{crackDistortionAlfaValue = x}),
          new SliderInit(0,255,crackDistortionDeltaValue,"Crack color delta",x=>{crackDistortionDeltaValue = x}),
          new SliderInit(0,1000,spotsPerBrick,"Spots per brick",x=>{spotsPerBrick = x}),
          new SliderInit(1,7,maximumCrackBrakes,"Maximum crack brakes",x=>{maximumCrackBrakes = x}),
          new SliderInit(0,100,(crackChance*100).intValue(),"Crack chance",x=>{crackChance = x.toDouble/100}),
          new SliderInit(0,100,(brickColorDeviation*100).intValue(),"Brick color deviation",x=>{brickColorDeviation = x.toDouble/100}),
          new SliderInit(5,20,cementWidth,"Cement width",x=>{cementWidth = x}),
          new SliderInit(0,5,cementWidthDeviation,"Cement width deviation",x=>{cementWidthDeviation = x}),
          new SliderInit(0,999,cementSpotsNumber,"Cement spots number",x=>{cementSpotsNumber = x}),
          )
        val repaintButton = new JButton("Repaint")
        repaintButton.addActionListener(_=>{
          drawTextures()
        })
        add(firstColorSliders)
        add(secondColorSliders)
        add(slidersPanel(settingsSliders,"Brick settings"))
        add(repaintButton)
        
      }
      settingsFrame.add(panel)
    })
    
    
    
    jPanel.add(firstColorSliders)
    jPanel.add(secondColorSliders)
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