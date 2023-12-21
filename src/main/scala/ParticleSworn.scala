import scala.math._
import scala.util.Random


object ParticleSworn {

  def itSeries[T](start: T, f: T => T): Iterator[T] = {
    new Iterator[T] {
      var cur: T = start

      override def hasNext = true

      override def next(): T = {
        cur = f(cur)
        cur
      }
    }
  }

  /**
   * частица
   *
   * @param position             - позция частицы
   * @param speed                - скорость частицы
   * @param personalBestPosition - позиция с лучшим результатом
   * @param personalBestValue    - величина лучшего результата
   */
  case class Particle(
                       position: Seq[Double],
                       speed: Seq[Double],
                       personalBestPosition: Seq[Double],
                       personalBestValue: Double,
                     )


  /**
   * Собственно, рой!
   *
   * @param swornSize         - размер роя
   * @param optimizedFunction - оптимизируемая функция
   * @param limits            - функции ограничения
   * @param start             - точка начала
   * @param isMinimizing      - минимизирующая ли функция
   * @param momentum          - инерционный вес
   * @param individualism     - масштабный коэффицент притяжения к лучшему персональному результату
   * @param collectivism      - масштабный коэффициент притяжения к лучшему глобальному результату
   */
  class Sworn(
               swornSize: Int,
               //Dimension:Int,
               optimizedFunction: Seq[Double] => Double,
               limits: Seq[Seq[Double] => Double],
               start: Seq[Double],
               var isMinimizing: Boolean,
               var momentum: Double,
               var individualism: Double,
               var collectivism: Double
             ) {
    //лучше полученное значение
    private var generalBestPosition: Seq[Double] = start
    //положение лучшего значения
    private var generalBestValue: Double = optimizedFunction(start)

    //итераторы перемещений частиц...
    //ну то есть, ряды их позиций...
    //тут не очень понятно, как это документировать, если честно.
    var particleSeries: Seq[Iterator[Particle]] = Seq.empty
    reset()

    //возвращает точки в n-мерном пространстве, случайно распределённые по n-мерной сфере.
    def generateRandomSpreadPattern(dim:Int):Seq[Double] = {
        val rDoubles = for(_<-1 to dim) yield Random.nextDouble()*2-1
        val length = sqrt(rDoubles.map(x=>x*x).sum)
        rDoubles.map(_/length)
    }

    //для получения текущих значений. Без блокировки, ибо один шаг ничего не решает.
    def generalBest: (Double, Seq[Double]) = (generalBestValue, generalBestPosition)

    //для более удобного обновления значений
    def updated(best: (Double, Seq[Double]), pretending: (Double, Seq[Double])): (Double, Seq[Double]) = {
      if(isMinimizing)
        if(pretending._1<best._1) pretending else best
      else
        if(pretending._1>best._1) pretending else best
    }

    //попытка обновления глобального оптимума с блокировкой, чтобы избежать гонок
    def tryUpdateGeneralBest(position: Seq[Double], result: Double): Unit = synchronized {
      val (gbv, gbp) = updated(generalBest, (result, position))
      generalBestValue = gbv
      generalBestPosition = gbp
    }

    //сдвиг частицы (ну или порождение сдвинутой частицы, поэтому, собственно не move)
    def moved(p: Particle): Particle = {
      //нужные для вычислений значения
      val (position, speed, pbp, pbv) = Particle.unapply(p).get
      val (gbv, gbp) = generalBest

      //случайные величины r<-(0,1)
      def r1 = Random.nextDouble()
      def r2 = Random.nextDouble()

      //новая позиция
      val newPosition = position.indices.map(i => position(i) + speed(i))

      //новое значение
      val value = optimizedFunction(newPosition)

      //обновление локального оптимума
      val (newPBV, newPBP) = updated((pbv, pbp), (value, position))

      //обновление глобального оптимума, но только если считанное значение должно быть заменено текущим
      //таким образом избегаем блокировок
      val (newGBV, newGBP) = updated((newPBV, newPBP),(gbv,gbp))
      if(newGBV != gbv) {
        tryUpdateGeneralBest(newPosition, value)
        println(s"new general best found:$gbv\t at:${newGBP.mkString("(",",",")")}")
      }

      //новая скорость
      val newSpeed = speed.indices
        .map { i =>
          speed(i) * momentum + //momentum
            (newPBP(i) - position(i)) * individualism * r1 + //individual acceleration
            (gbp(i) - position(i)) * collectivism * r2 //collective acceleration
        }
      Particle(newPosition, newSpeed, newPBP, newPBV)
    }

    def makeStep(): Seq[Particle] = {
      particleSeries.map(_.next())
    }
    def reset():Unit = {
      generalBestPosition = start
      generalBestValue = optimizedFunction(start)
      particleSeries = (1 to swornSize).map(_=>
        itSeries[Particle](
          {
            val pos = generateRandomSpreadPattern(start.length)
            val speed = generateRandomSpreadPattern(start.length)
            val pbp = generateRandomSpreadPattern(start.length)
            val pbv = optimizedFunction(pbp)
            Particle(pos, speed, pbp, pbv)
          }, moved))
    }
  }
}