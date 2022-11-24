import SIFPanel.{applyTransformations, transformationsList}
import breeze.linalg.{DenseMatrix, Matrix, inv}

import java.awt.Color
import java.awt.geom.{AffineTransform, Line2D, Point2D}
import javax.swing.{JButton, JPanel}
import scala.swing.Dimension

class SIF2P extends JPanel{
  val iterations = 5
  val drawer = new TriangleDrawer
  setLayout(null)
  drawer.setBounds(0,0,700,700)
  drawer.setPreferredSize(new Dimension(700,700))
  add(drawer)

  object SettingsPanel extends JPanel {
    setBounds(700, 0, 600, 700)

    val paintButton = new JButton("paint!")
    paintButton.addActionListener(_ => {
      var triangles = drawer.drawablePart.getTriangles
      if(triangles.isEmpty) triangles = Seq(
        Seq(
          new Point2D.Double(4.14,7),
          new Point2D.Double(0.1,1.9),
          new Point2D.Double(7.8,1.26),
        ),
        Seq(
        new Point2D.Double(4.14,7),
        new Point2D.Double(2.2,3.54),
        new Point2D.Double(5.7,3.06),
      )
      )
      def triangle2Matrix(trianglePoints:Seq[Point2D]):DenseMatrix[Double] = {
        DenseMatrix.tabulate[Double](3,3)((i,j)=>{
          j match {
            case 0 => trianglePoints(i).getX
            case 1 => trianglePoints(i).getY
            case 2 => 1
          }
        })
      }
      val (source,images) = triangles
        .map(triangle2Matrix) match {case Seq(h, rest @ _*)=> (h,rest)}
      val xTs = images.map{img=>
        val mx = source \ img
        val xT = new AffineTransform(mx(0,0),mx(0,1),mx(1,0),mx(1,1),mx(0,2),mx(1,2))
        xT
      }
      def tLines(pSeq:Seq[Point2D]):Seq[Line2D] = pSeq match {
        case Seq(a,b,c) =>
          Seq(
            new Line2D.Double(a,b),
            new Line2D.Double(b,c),
            new Line2D.Double(c,a)
          )
      }
      val transforms = transformationsList(xTs)
      val shapes = applyTransformations(transforms(iterations),tLines(triangles.head))
      val visualize = new PaintFrame(shapes = shapes,transformCalc = _=>new AffineTransform(2,0,0,2,0,0))
    })
    add(paintButton)

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
