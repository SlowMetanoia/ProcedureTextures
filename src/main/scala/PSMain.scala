import JFrameBasics._
import ParticleSworn.{Particle, Sworn, itSeries}
import pkg.SliderInit

import java.awt.Color
import java.awt.geom.Point2D
import scala.math.{abs, pow, sin}

object PSMain {
  def main(args: Array[String]): Unit = {

    //то, что мы будем оптимизировать
    val f1: Seq[Double] => Double = {
      case Seq(x, y) => x * sin(sin(4 * x)) + 1.1 * y * sin(2 * y)
    }
    val f3:Seq[Double] => Double = {
      case Seq(x, y) => x*x - 16 + y*y*sin(x*y)*sin(x*y)
    }
    //    факториал
    def fact:Int=>Int = n=>(1 to n).product

    //функция бесселя первого рода, чтобы жизнь мёдом не казалась
    //order-порядок функции
    def bessel(order:Int):Double=>Double = x=>
      itSeries[Int](0,_ + 1)
        //разложение в ряд тейлора
        .map(m=>
          pow(-1,m) * pow(x/2,2*m+order)/
            (fact(m)*fact(m+order+1))
        )
        .take(10)
        .sum
    //медленно, и не факт, что точно, но нам ведь нужна *какая-то* функция =)

    val J0 = bessel(0)
    def f2:Seq[Double]=>Double = {
      case Seq(x,y) => -J0(20*x*x+y+y) - 0.1*abs(1-x) - 0.1*abs(1-y)
    }

    val sworn:Sworn = new Sworn(
      60,
      f3,
      Seq.empty,
      start = Seq(10,-0.0005),
      false,
      0.4,
      0.3,
      0.3
    )
    var scale = 10.0
    def scaledPoint(scale:Double):Particle=>Point2D = p=> new Point2D.Double(p.position(0)*scale+300,p.position(1)*scale+300)
    val jFrame = JFrameBasics.jFrame
    val sd = new SwornDrawer(
      sworn,
      1000,
      Color.BLACK,
      _=>Color.YELLOW,
      scaledPoint(scale),
      Seq.empty
    )
    jFrame.add(sd)
    sd.add(new SliderInit(1,1000,(scale*100).toInt,"scale*100",n=>{
      scale = n /100.0
      sd.particlePosition = scaledPoint(scale)
      sd.drawComponent.repaint()
    }).getSliderPanel)

    jFrame.revalidate()

    while (true)
      sd.makeStep()
  }
}
