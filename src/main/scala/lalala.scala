import java.awt.geom.Line2D
import scala.annotation.unused

object lalala extends App{
  import FormsMain.ATChooseComponent._
  def calcNPrint(
                  x0:Double,
                  x1:Double,
                  y0:Double,
                  y1:Double
                ) = {
    val baseLine = new Line2D.Double(0,0,1,0)
    val imgLine =  new Line2D.Double(x0,y0,x1,y1)
    println(s"source: [(0,0),(1,0)]")
    println(s"to:     [($x0,$y0),($x1,$y1)]")
    val result = ->(baseLine,getAT(baseLine,imgLine))
    println(s"result: [(${result.getX1},${result.getY1}),(${result.getX2},${result.getY2})]")
  }
  Seq(
    (0,0,1,1),
    (0,0,-1,1),
    (0,0,-1,-1),
    (0,0,1,-1)
  ).foreach{case (x0,y0,x1,y1) => calcNPrint(x0,x1,y0,y1) }

}
