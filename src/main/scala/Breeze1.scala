import breeze.linalg.Matrix


object Breeze1 {
  def main(args:Array[String]): Unit = {
    val x = Matrix.zeros[Double](3,3)
    println(x)
    println
    x(0,2) = 2
    println(x)
  }
}