object SplineCalculationsProcessor{
  //cubic f by coefficient
  case class Cf(c0:Double,c1:Double,c2:Double,c3:Double){
    def f:Double=>Double = t=>c0 + c1*t + c2 * t * t + c3 * t * t * t
    def df:Double=>Double = t=> c1 + 2*c2*t + 3*c3*t*t
    def ddf:Double=>Double = t=> 2*c2 + 6*c3*t
  }
}