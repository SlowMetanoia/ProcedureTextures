import breeze.linalg.{ DenseMatrix, DenseVector, inv, min }
import breeze.numerics.sqrt

import java.awt.event.{ MouseAdapter, MouseEvent }
import java.awt.geom.{ Ellipse2D, Line2D, Point2D }
import java.awt.{ BasicStroke, Color, Graphics, GridBagLayout }
import javax.swing.{ JButton, JComponent, JPanel, JTextArea }
import scala.swing.{ Dimension, Graphics2D }
object testMask {
  def main( args: Array[ String ] ): Unit = {
    println(s"matMask(4) = ${ (new SplinePlotter).SplineFunctions.matMask(4) }")
  }
}
object testMap{
  def main( args: Array[ String ] ): Unit = {
    val func = (new SplinePlotter).SplineFunctions
    println(s"maped = ${ func.matMap(func.matMask(4),Map[Int,DenseMatrix[Double]](
      0->DenseMatrix.zeros(4,4),
      1->DenseMatrix.fill(4,4)(1),
      2->DenseMatrix.fill(4,4)(2),
      3->DenseMatrix.fill(4,4)(3),
      4->DenseMatrix.fill(4,4)(4)
      ))
    }")
  }
}
object funcPointTest{
  def main( args: Array[ String ] ): Unit = {
    (new SplinePlotter).SplineFunctions.splineFunc2PointsFunc(Seq(DenseMatrix((1.0,2.0,3.0,4.0), (5.0,6.0,7.0,8.0))))
  }
}
class SplinePlotter extends JPanel{
  val textField = new JTextArea()
  textField.setMinimumSize(new Dimension(150,500))
  textField.setMaximumSize(new Dimension(150,500))
  textField.setPreferredSize(new Dimension(150,500))
  type vec2 = (Double,Double)
  
  sealed trait SPLINE
  
  sealed trait SPLINE_MODE
  final object HERMIT extends SPLINE_MODE
  final object NATURAL extends SPLINE_MODE
  
  var mode:SPLINE_MODE = NATURAL
  val bgColor: Color = Color.DARK_GRAY
  val pointColor: Color = Color.YELLOW
  val vectorColor: Color = Color.GREEN
  val splineColor: Color = Color.LIGHT_GRAY
  val inBetweenColor: Color = Color.CYAN
  
  val dLinesColor: Color = Color.BLACK
  
  val pointRadius = 10
  
  
  //отрисовываемые точки
  var points:Seq[Point2D] = Seq.empty
  
  var movingPoint:Option[Int] = None
  
  def interpolateErmitLike(points:Seq[Point2D],g2d:Graphics2D):Unit = {
    g2d.setColor(splineColor)
    points.sliding(2).foreach {
      case Seq(p1, p2) => g2d.draw(new Line2D.Double(p1, p2))
      case _=>
    }
    
    if(points.length>4) {
      g2d.setColor(inBetweenColor)
      val v0 = DenseVector(points.head.getX - points.tail.head.getX, points.head.getY - points.tail.head.getY) * 15.0
      val vn = DenseVector(points.last.getX - points.init.last.getX, points.last.getY - points.init.last.getY) * 15.0
      SplineFunctions.hermitSplineLines(points.init.tail,v0,vn,100).foreach(g2d.draw)
      val (xn,yn) = (points.init.last.getX,points.init.last.getY)
      val (x0,y0) = (points.tail.head.getX,points.tail.head.getY)
      g2d.draw(new Line2D.Double(x0,y0,x0+v0(0)*100,y0+v0(1)*100))
      g2d.draw(new Line2D.Double(xn,yn,xn+vn(0)*100,yn+vn(1)*100))
    }
  }
  
  def interpolate(points:Seq[Point2D],g2d:Graphics2D):Unit = {
    g2d.setColor(splineColor)
    points.sliding(2).foreach {
      case Seq(p1, p2) => g2d.draw(new Line2D.Double(p1, p2))
      case _=>
    }
    
    if(points.length>2) {
      g2d.setColor(inBetweenColor)
      //cardinalSplineCurve(points,scale = .5).sliding(2).foreach { case Seq(p1, p2) => g2d.draw(new Line2D.Double(p1, p2)) }
      SplineFunctions.naturalSplineLines(points,100).foreach(g2d.draw)
      
    }
  }
  
  def hermitSpline( p0:vec2, p1:vec2, v0:vec2, v1:vec2, t:Double):vec2 ={
    val points = DenseMatrix(p0, v0, p1, v1)
    /*
    * vec4(1,0,0,0),
      vec4(-s,0,s,0),
      vec4(2.*s,s-3.,3.-2.*s,-s),
      vec4(-s,2.-s,s-2.,s)
    * */
    val cardinalMatrix = DenseMatrix(
      (1.0,0.0,0.0,0.0),
      (0.0,1.0,0.0,0.0),
      (-3.0,-2.0,3.0,-1.0),
      (2.0,1.0,-2.0,1.0)
    )
      /*DenseMatrix(
      (1.0,0.0,0.0,0.0),
      (-scale,0.0, scale,0.0),
      (2.0* scale, scale -3.0, 3.0 - 2.0 * scale, -scale),
      (-scale,2.0 - scale, scale -2.0, scale)
      )*/
    val powers = DenseMatrix((1.0,t,t*t,t*t*t))
    
    val result =  powers * cardinalMatrix * points
    
    (result.apply(0,0),result.apply(0,1))
  }
  def mirrorPoint(mirror:vec2, mirrored:vec2, scale:Double):vec2={
    val v = (mirror._1 - mirrored._1, mirror._2 - mirrored._2)
    val l = sqrt(v._1*v._1 + v._2*v._2)
    val nv = (v._1/l, v._2/l)
    (mirror._1 + nv._1*(l*scale),mirror._2 + nv._2*(l*scale))
  }
  
  def S(p0:vec2,p1:vec2):vec2 = (p1._1-p0._1,p1._2-p0._2)
  
  def length(p0:vec2,p1:vec2):Double = {
    val s = S(p0:vec2,p1:vec2)
    sqrt(s._1*s._1+s._2*s._2)
  }
  
  def direction(p0:vec2,p1:vec2):vec2 = {
    val s = S(p0, p1)
    val l = length(p0, p1)
    (s._1/l,s._2/l)
  }
  
  def outPoint(p0:vec2,p1:vec2):vec2 = {
    val d = direction(p1,p0)
    (p1._1+d._1*1200,p1._2+d._2*1200)
  }
  
  object SplineFunctions{
    
    val zero4: DenseVector[ Double ] = DenseVector.zeros[Double](4)
    val zero2: DenseVector[ Double ] = DenseVector.zeros[Double](2)
    
    def T(t:Double): DenseVector[ Double ] = DenseVector(1, t, t * t, t * t * t)
    def dT(t:Double): DenseVector[ Double ] = DenseVector(0, 1, 2 * t, 3 * t * t)
    def ddT(t:Double): DenseVector[ Double ] = DenseVector(0, 0, 2, 6 * t)
    
    def A(ai:Double,bi:Double): DenseMatrix[Double]    = DenseMatrix(T(ai), T(bi), dT(bi),    ddT(bi) ).t
    def B(ai:Double,bi:Double): DenseMatrix[Double]    = DenseMatrix(zero4, zero4, dT(ai),    ddT(ai) ).t
    def fC(an:Double,bn:Double): DenseMatrix[ Double ] = DenseMatrix(T(an), T(bn), zero4,     ddT(bn) ).t
    def hC(an:Double,bn:Double): DenseMatrix[ Double ] = DenseMatrix(T(an), T(bn), zero4,     dT(bn)  ).t
    def fD(a1:Double,b1:Double): DenseMatrix[ Double ] = DenseMatrix(zero4, zero4, -ddT(a1),  zero4   ).t
    def hD(a1:Double,b1:Double): DenseMatrix[ Double ] = DenseMatrix(zero4, zero4, -dT(a1),   zero4   ).t
    
    def U(p0:DenseVector[Double],p1:DenseVector[Double]): DenseMatrix[ Double ] = DenseMatrix(p0, p1, zero2, zero2).t
    def hnU( pn1:DenseVector[Double], pn:DenseVector[Double], v0:DenseVector[Double], vn:DenseVector[Double]): DenseMatrix[ Double ] = DenseMatrix(pn1, pn, v0, vn).t
    
    def matMask(n:Int): DenseMatrix[Int] ={
      val zeros = DenseVector.zeros[Int](n).toDenseMatrix
      val ae = DenseMatrix.eye[Int](n)
      val be = DenseMatrix.vertcat(zeros,2*ae)
      val result = ae + be(0 until n, ::)
      result(0,-1)=4
      result(-1,-1)=3
      result
    }
    def matMap(mask:DenseMatrix[Int],map:Map[Int,DenseMatrix[Double]]):DenseMatrix[Double] = {
      val (n,m) = (map.values.head.rows,map.values.head.cols)
      val (r,c) = (mask.rows,mask.cols)
      val result = DenseMatrix.zeros[Double](n*r,m*c)
      for{
        i<-0 until r
        j<-0 until c
        k<-0 until n
        l<-0 until m
          }{
        result(i*n+k,j*m+l) = map(mask(i,j))(k,l)
      }
      result
    }
    
    def naturalSpline(points:Seq[DenseVector[Double]]):Seq[DenseMatrix[Double]] = {
      val segmentNumber = points.length-1
      
      //a,b,c,d
      val a = A(0,1)
      val b = B(0,1)
      val c = fC(0,1)
      val d = fD(0,1)
      
      //u
      val u = points.sliding(2).map{ case Seq(pt0,pt1) => U(pt0,pt1) }.reduce(DenseMatrix.horzcat(_,_))

      //q
      val mask = matMask(segmentNumber)
      val map = Map(
        0->DenseMatrix.zeros[Double](4,4),
        1->  a,
        2-> -b,
        3->  c,
        4-> -d
        )
      val q = matMap(mask, map)
      //result
      val result = u*inv(q)
      //println(s"result = \n${ result }")
      val out = for(col<-0 until segmentNumber) yield result(::,col*4 until col*4+4)
      //println(s"out = \n${ out.mkString("\n") }")
      out
    }
    def hermitSpline(points:Seq[DenseVector[Double]],v0:DenseVector[Double],vn:DenseVector[Double]):Seq[DenseMatrix[Double]] = {
      val segmentNumber = points.length-1
  
      //a,b,c,d
      val a = A(0,1)
      val b = B(0,1)
      val c = fC(0,1)
      val d = fD(0,1)
  
      //u
      val u = points
        .init
        .sliding(2)
        .map{ case Seq(pt0,pt1) => U(pt0,pt1) }
        .toSeq
        .appended(hnU(points.init.last,points.last,v0,vn))
        .reduce(DenseMatrix.horzcat(_,_))
      
      //q
      val mask = matMask(segmentNumber)
      val map = Map(
        0->DenseMatrix.zeros[Double](4,4),
        1->  a,
        2-> -b,
        3->  c,
        4-> -d
        )
      val q = matMap(mask, map)
  
      //result
      val result = u*inv(q)
      //println(s"result = \n${ result }")
      val out = for(col<-0 until segmentNumber) yield result(::,col*4 until col*4+4)
      //println(s"out = \n${ out.mkString("\n") }")
      out
    }
    def splineFunc2PointsFunc( coefficients:Seq[DenseMatrix[Double]]):Seq[Double=>Point2D] = {
      coefficients
        .map{c=>
        (t:Double)=> ( c * T(t) ).toScalaVector match {
          case Vector(x,y) => new Point2D.Double(x,y)
        }
      }
    }
    
    def points2Vectors(points:Seq[Point2D]):Seq[DenseVector[Double]] = points.map(pt=>DenseVector(pt.getX,pt.getY))
    
    def naturalSplineLines(points:Seq[Point2D],segments:Int): Iterator[Line2D.Double ] = {
      val range = for(i<-0 to segments) yield i.toDouble / segments
      splineFunc2PointsFunc(naturalSpline(points2Vectors(points)))
        .flatMap(f=>range.map(t=>f(t)))
        .sliding(2)
        .map{case Seq(pt0,pt1)=>new Line2D.Double(pt0,pt1)}
    }
    def hermitSplineLines(points:Seq[Point2D],v0:DenseVector[Double],vn:DenseVector[Double],segments:Int): Iterator[Line2D.Double ] = {
      val range = for(i<-0 to segments) yield i.toDouble / segments
      splineFunc2PointsFunc(hermitSpline(points2Vectors(points),v0, vn))
        .flatMap(f=>range.map(t=>f(t)))
        .sliding(2)
        .map{case Seq(pt0,pt1)=>new Line2D.Double(pt0,pt1)}
    }
  }
  
  def cardinalSplineCurve( points:Seq[Point2D], scale:Double):Seq[Point2D] = {
    val n = 100
    if(points.length>2) {
      var pts = points.map(p => (p.getX, p.getY))
      var cardinalSplineVelocities =
        pts
          .sliding(3)
          .map { case Seq(h, _, t) => ((t._1 - h._1)*scale, (t._2 - h._2)*scale) }.toSeq
      cardinalSplineVelocities = cardinalSplineVelocities
        .appended((0.0,0.0))
        .prepended((0.0,0.0))
      
      val ttt = tt(n)
      val dt = 1.0/n
      val fPoints = pts.zip(cardinalSplineVelocities)
        .sliding(2)
        .flatMap {
          pair =>
            val ((p0,v0),(p1,v1)) = (pair.head,pair.last)
            val points = ttt.map(t => hermitSpline(p0, p1, v0.asInstanceOf[(Double,Double)], v1.asInstanceOf[(Double,Double)], t))
            points.map(p => new Point2D.Double(p._1, p._2))
        }.toSeq
      
      fPoints
    }else{
      Seq.empty
    }
  }
  
  def tt(n:Int):Seq[Double] = for(i<-0 to n) yield 1.0/n * i
  
  
  def drawPoint(point:Point2D,g2d:Graphics2D):Unit = {
    val (w,h) = (pointRadius,pointRadius)
    val (x,y) = (point.getX-w/2,point.getY-h/2)
    val pointEllipse = new Ellipse2D.Double(x,y,w,h)
    g2d.fill(pointEllipse)
    g2d.setColor(Color.BLACK)
    g2d.draw(pointEllipse)
  }
  
  def drawVector(p1:Point2D,p2:Point2D,g2d:Graphics2D,startAt:Point2D):Unit = {
    g2d.setColor(vectorColor)
    val x = p2.getX - p1.getX
    val y = p2.getY - p1.getY
    val length = math.sqrt(x*x+y*y)
    val (normalizedX,normalizedY) = (x/length,y/length)
    val arrowLength =  min(1.0 / 10 * length,20.0)
    val arrowWidth = min(1.0/40 *length,10.0)
    val pt1 = startAt
    val pt2 = new Point2D.Double(startAt.getX + x,startAt.getY + y)
    val line = new Line2D.Double(pt1,pt2)
    val baseP = new Point2D.Double(p2.getX - normalizedX*arrowLength,p2.getY - normalizedY*arrowLength)
    
    g2d.setStroke(new BasicStroke(0))
    
    val arrowParts:Seq[Line2D] = {
      val arrP1 = new Point2D.Double(baseP.getX - arrowWidth*normalizedY,baseP.getY + arrowWidth*normalizedX)
      val arrP2 = new Point2D.Double(baseP.getX + arrowWidth*normalizedY,baseP.getY - arrowWidth*normalizedX)
      Seq(new Line2D.Double(pt2,arrP1),new Line2D.Double(pt2,arrP2))
    }
    g2d.draw(line)
    arrowParts.foreach(g2d.draw)
  }
  def emptyDrawVector(p1:Point2D,p2:Point2D,g2d:Graphics2D,point2D: Point2D):Unit = ()
  
  def splineDrawer(
                    pointDrawer:(Point2D,Graphics2D)=>Unit,
                    vectorDrawer:(Point2D,Point2D,Graphics2D,Point2D)=>Unit,
                    splineDrawer:(Seq[Point2D],Graphics2D)=>Unit,
                    pointColor:Color,
                    vectorColor:Color,
                    splineColor:Color,
                    g2d:Graphics2D
                  ):Seq[Point2D]=>Unit = points => {
    g2d.setStroke(new BasicStroke(0))
    g2d.setColor(splineColor)
    splineDrawer(points,g2d)
    points.foreach{ point=>
      g2d.setColor(pointColor)
      pointDrawer(point,g2d)
    }
    if(points.length>3) {
      g2d.setColor(vectorColor)
      vectorDrawer(points(0), points(1), g2d,points(0))
      g2d.setColor(vectorColor)
      vectorDrawer(points.takeRight(2)(0), points.takeRight(2)(1), g2d,points.takeRight(2)(0))
    }
  }
  
  setMaximumSize(new Dimension(1300, 650))
  setMinimumSize(new Dimension(600,300))
  setPreferredSize(new Dimension(1300, 650))
  
  val panelLayout = new GridBagLayout
  val topPanelConstraints = pkg.getConstraints(0,0,1,24)
  object topPanel extends JPanel{
    val interpolationButton = new JButton("INTERPOLATING")
    val ermitButton = new JButton("HERMIT")
    interpolationButton.addActionListener(_=>{mode = NATURAL; drawer.repaint()})
    ermitButton.addActionListener(_=>{mode = HERMIT; drawer.repaint()})
    add(interpolationButton)
    add(ermitButton)
  }
  
  add(topPanel,topPanelConstraints)
  
  object drawer extends JComponent{
    setMaximumSize(new Dimension(1300, 700))
    setMinimumSize(new Dimension(600,300))
    setPreferredSize(new Dimension(1300, 650))
    override def paintComponent( g: Graphics ): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]
      
      g2d.setColor(bgColor)
      g2d.fillRect(-3000,-3000,6000,6000)
      
      val draw = mode match {
        case HERMIT => splineDrawer(drawPoint, drawVector, interpolateErmitLike, pointColor, vectorColor, splineColor, g2d)
        case NATURAL => splineDrawer(drawPoint, emptyDrawVector, interpolate, pointColor, vectorColor, splineColor, g2d)
      }
        draw(points)
    }
    
    addMouseListener(new MouseAdapter {
      override def mouseClicked( e: MouseEvent ): Unit = {
        super.mouseClicked(e)
        val closest = points
          .zipWithIndex
          .find{case (pt,_) =>
            val (x, y) = (pt.getX - e.getPoint.getX, pt.getY - e.getPoint.getY)
            sqrt(x * x + y * y) < pointRadius
          }.map(_._2)
        
        movingPoint = movingPoint match {
          case Some(i) => points = points.updated(i,e.getPoint); None
          case None => closest match {
            case Some(value) => Some(value)
            case None => e.getButton match {
              case 1 => points = points.appended(e.getPoint)
              case 3 => points = points.prepended(e.getPoint)
              case _ =>
            }
            None
          }
        }
        repaint()
        updateTextField()
      }
    })
    addMouseMotionListener(new MouseAdapter {
      override def mouseMoved( e: MouseEvent ): Unit = {
        super.mouseMoved(e)
        movingPoint.foreach(i=>{
           points = points.updated(i,e.getPoint)
          repaint()
        })
        updateTextField()
      }
    })
  }
  def updateTextField(): Unit = {
    textField.setText(
      if(mode == HERMIT){
        val v0 = DenseVector(points.head.getX - points.tail.head.getX, points.head.getY - points.tail.head.getY)
        val vn = DenseVector(points.last.getX - points.init.last.getX, points.last.getY - points.init.last.getY)
        s"Velocities:\nv0 = ${(v0(0),v0(1))} * 15.0\nvn = ${(vn(0),vn(1))} * 15.0\n" +
          s"Points:\n${points.init.tail.map(p=>(p.getX,p.getY)).mkString("\n")}"
      } else {
        s"Points:\n${points.map(p=>(p.getX,p.getY)).mkString("\n")}"
      }
      )
  }
  val drawerConstraints = pkg.getConstraints(0,1,12,24)
  add(drawer,drawerConstraints)
  add(textField)
  updateTextField()
}

object SplinePlotter extends App{
  val frame = JFrameBasics.jFrame
  frame.setSize(frame.getWidth+200,frame.getHeight)
  val plotter = new SplinePlotter
  frame.add(plotter)
  frame.revalidate()
}