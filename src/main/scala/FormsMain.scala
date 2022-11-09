
import pkg._

import java.awt._
import java.awt.event._

object FormsMain{
  
  def main( args: Array[ String ] ): Unit = {
    val frame = JFrameBasics.jFrame
    val drawPlace = new DrawPlace()
    frame.add(drawPlace)
    frame.revalidate()
  }
}
