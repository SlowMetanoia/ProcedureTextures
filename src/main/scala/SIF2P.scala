import SIFPanel.{applyTransformations, transformationsList}
import breeze.linalg.{DenseMatrix, Matrix, inv}
import pkg.SliderInit

import java.awt.Color
import java.awt.geom.{AffineTransform, Line2D, Point2D}
import javax.swing.{JButton, JPanel}
import scala.swing.Dimension

class SIF2P extends JPanel{
  var iterations = 5
  val drawer = new TriangleDrawer
  val visualizer = new FramePainting(Color.BLACK,Color.YELLOW,Seq.empty)
  visualizer.hide()
  setLayout(null)
  drawer.setBounds(0,0,700,700)
  drawer.setPreferredSize(new Dimension(700,700))
  add(drawer)
  drawer.changeListeners = Seq(drawAll)
  def drawAll(triangles:Seq[Seq[Point2D]]): Unit ={
    //var triangles = drawer.drawablePart.getTriangles
    def triangle2Matrix(trianglePoints: Seq[Point2D]): DenseMatrix[Double] = {
      DenseMatrix.tabulate[Double](3, 3)((i, j) => {
        j match {
          case 0 => trianglePoints(i).getX
          case 1 => trianglePoints(i).getY
          case 2 => 1
        }
      })
    }

    if(triangles.nonEmpty){
      val (source, images) = triangles
        .map(triangle2Matrix) match {
        case Seq(h, rest@_*) => (h, rest)
      }
      val xTs = images.map { img =>
        val mx = source \ img

        val xT = new AffineTransform(mx(0, 0), mx(0, 1), mx(1, 0), mx(1, 1), mx(2, 0), mx(2, 1))
        xT
      }

      def tLines(pSeq: Seq[Point2D]): Seq[Line2D] = pSeq match {
        case Seq(a, b, c) =>
          Seq(
            new Line2D.Double(a, b),
            new Line2D.Double(b, c),
            new Line2D.Double(c, a)
          )
      }

      val transforms = transformationsList(xTs)(iterations)
      val shapes = {
        val base = triangles.head
        transforms.map { xT =>
          base match {
            case Seq(a, b, c) =>
              Seq(
                xT.transform(a, null),
                xT.transform(b, null),
                xT.transform(c, null),
              )
          }
        }
      }
      visualizer.shapes = shapes.flatMap(tLines)
      visualizer.drawComponent.repaint()
    }
  }
  object SettingsPanel extends JPanel {
    setBounds(700, 0, 600, 700)
    val c = 5

    val paintButton = new JButton("paint!")
    paintButton.addActionListener(_ => {

      visualizer.show()
      visualizer.drawComponent.repaint()
    })
    //add(paintButton)
    add(visualizer.drawComponent)
    val itSlider = new SliderInit(0,20,iterations,"Iterations number",value=>{iterations = value;drawer.applyChanges()}).getSliderPanel
    add(itSlider)
    //setBackground(Color.BLACK)
    revalidate()
  }
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
