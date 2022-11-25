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
  setLayout(null)
  drawer.setBounds(0,0,700,700)
  drawer.setPreferredSize(new Dimension(700,700))
  add(drawer)

  object SettingsPanel extends JPanel {
    setBounds(700, 0, 600, 700)
    val c = 5

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
        println(mx)
        /*println(s"mx(0,0) = ${mx(0,0)}")
        println(s"mx(1,0) = ${mx(1,0)}")
        println(s"mx(0,1) = ${mx(0, 1)}")
        println(s"mx(1,1) = ${mx(1, 1)}")
        println(s"mx(0,2) = ${mx(0,2)}")
        println(s"mx(1,2) = ${mx(1,2)}")*/
        val xT = new AffineTransform(mx(0,0),mx(0,1),mx(1,0),mx(1,1),mx(2,0),mx(2,1))
        println(s"xT = ${xT}")
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
      val transforms = transformationsList(xTs)(iterations)
      val shapes = {
        val base = triangles.head
        transforms.map{xT=>
          base match {
            case Seq(a,b,c) =>
              Seq(
                xT.transform(a,null),
                xT.transform(b,null),
                xT.transform(c,null),
              )}}}
      //applyTransformations(transforms(iterations),tLines(triangles.head))
      val visualize = new FramePainting(Color.BLACK,Color.YELLOW,shapes.flatMap(tLines))
    })
    add(paintButton)
    val itSlider = new SliderInit(0,20,iterations,"Iterations number",value=>{iterations = value}).getSliderPanel
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
