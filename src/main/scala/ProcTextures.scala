import java.awt.geom.{Line2D, Point2D, Rectangle2D}
import java.awt.{BasicStroke, Color, Graphics}
import javax.swing.{JButton, JComponent, JLabel, JPanel}
import scala.swing.Graphics2D
import pkg._

import scala.math.{Pi, exp, sin}
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
    var mode: TextureMode = GRADIENT

    //-----------------------------------------Вспомогательные функции--------------------------------------------------
    def functionLines(f: Double => Double, step: Double): Iterator[Line2D] = {
      series[Point2D](new Point2D.Double(0, f(0)))(pt => new Point2D.Double(pt.getX + step, f(pt.getX + step)))
        .sliding(2).map { case LazyList(pt1, pt2) => new Line2D.Double(pt1, pt2) }
    }

    def drawMultiFuncLines(g2d: Graphics2D,
                           f: Double => Double,
                           step: Double,
                           fColor: Color,
                           rotationAng: Double,
                           distance: Double,
                           stroke: Double = 0): Unit = {
      val shiftedF = (x: Double) => f(x)
      //линии функции от x = -1300 до x >= 2600
      val zeroF = functionLines(shiftedF, step).takeWhile(_.getX1 < 3000)
      //сдвиги
      val shifts = series[Double](-3000)(_ + distance)
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
      val xT = pkg.rotation(rotationAng)
      xT.concatenate(pkg.scale(scaleX,scaleY))
      g2d.setStroke(new BasicStroke(stroke.toFloat))
      g2d.setColor(fColor)
      g2d.setTransform(xT)
      shifted.foreach(g2d.draw)
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
          for {
            i <- xs.indices
            j <- ys.indices
          } if (j % 2 == 0) g2d.fill(rectangle(xs(i), ys(j))) else g2d.fill(rectangle(xs(i) - cellSize * brickShift / 100, ys(j)))

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
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          drawMultiFuncLines(g2d,x=>100*sin(x / 50),1,cellColor,rotation,150,outlineWidth)
        }
        case LINES => textureDrawer.paintMe = g2d=>{
          g2d.setColor(gridColor)
          g2d.fillRect(-3000, -3000, 6000, 6000)
          drawMultiFuncLines(g2d,x=> x,1,cellColor,rotation,150,outlineWidth)
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


    val buttonsPanel = new JPanel() {
      add(brickButton)
      add(plainColorButton)
      add(gridButton)
      add(multicolorGridButton)
      add(gradientButton)
      add(waveButton)
      add(linesButton)
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
    val atSettingsSliders = Seq(
      new SliderInit(0,90,rotation.intValue(), "Rotation angle",{ value => rotation = Pi/180*value;textureDrawer.repaint()}),
      new SliderInit(1,999,(scaleX*100).intValue(), "Scale X", {value => scaleX = value/100;textureDrawer.repaint()}),
      new SliderInit(1,999,(scaleY*100).intValue(), "Scale Y", {value => scaleY = value/100;textureDrawer.repaint()}),
      new SliderInit(-999,999,shift.intValue(), "Shift", {value => shift = value;textureDrawer.repaint()})
    )

    jPanel.add(slidersPanel(gridColorSliders, "Grid color settings"))
    jPanel.add(slidersPanel(cellColorSliders, "Cell color settings"))
    jPanel.add(slidersPanel(cellSettingsSliders, "Cell settings"))
    jPanel.add(slidersPanel(starsColorSliders,"Stars settings"))
    jPanel.add(slidersPanel(atSettingsSliders,"Some AT Settings"))
    jPanel.revalidate()

    jFrame.setLayout(null)
    jFrame.add(textureDrawer)
    jFrame.add(jPanel)

    drawTextures()
    jFrame.revalidate()
    jFrame.setTitle("Procedure Textures")
  }
}