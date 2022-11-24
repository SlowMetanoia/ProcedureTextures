import breeze.linalg._
import breeze.numerics._

import java.awt.geom.Point2D


object Breeze1 {
  def main(args:Array[String]): Unit = {
    val xs = Seq(
      (0,0,0),
      (1,1,1),
      (2,2,2),
      (3,3,3)
    )
    val mx = Matrix.tabulate[Int](xs.length,3)((i,j)=> {
      j match {
        case 0 => xs(i)._1
        case 1 => xs(i)._2
        case 2 => xs(i)._3
      }})
    println(mx)

    def triangle2Matrix(trianglePoints: Seq[Point2D]): DenseMatrix[Double] = {
      DenseMatrix.tabulate[Double](3, 3)((i, j) => {
        j match {
          case 0 => trianglePoints(i).getX
          case 1 => trianglePoints(i).getY
          case 2 => 1
        }
      })
    }

    val m1 = triangle2Matrix(Seq(
      new Point2D.Double(4.14, 7),
      new Point2D.Double(2.2, 3.54),
      new Point2D.Double(5.7, 3.06),
    ))
    val m2 = triangle2Matrix(Seq(
      new Point2D.Double(4.14, 7),
      new Point2D.Double(0.1, 1.9),
      new Point2D.Double(7.8, 1.26),
    ))
    println(s"m1\\m2 = ${m1 \ m2}")
    println(s"m1\\m2 = ${m2/m1}")
    println(s"m1\\m2 = ${inv(m1) * m2}")
  }
}