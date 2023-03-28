import breeze.linalg.{ DenseMatrix, min }
import breeze.numerics.sqrt

import java.awt.event.{ MouseAdapter, MouseEvent }
import java.awt.geom.{ Ellipse2D, Line2D, Point2D }
import java.awt.{ BasicStroke, Color, Graphics, GridBagLayout }
import javax.swing.{ JButton, JComponent, JPanel }
import scala.swing.{ Dimension, Graphics2D }

class SplinePlotter extends JPanel{
  
  type vec2 = (Double,Double)
  
  sealed trait SPLINE
  
  sealed trait SPLINE_MODE
  final object ERMIT extends SPLINE_MODE
  final object NATURAL extends SPLINE_MODE
  
  var mode:SPLINE_MODE = NATURAL
  val bgColor: Color = Color.DARK_GRAY
  val pointColor: Color = Color.YELLOW
  val vectorColor: Color = Color.GREEN
  val splineColor: Color = Color.LIGHT_GRAY
  val inBetweenColor: Color = Color.CYAN
  
  val pointRadius = 15
  
  
  //отрисовываемые точки
  var points:Seq[Point2D] = Seq.empty
  
  var movingPoint:Option[Int] = None
  
  def interpolateErmitLike(points:Seq[Point2D],g2d:Graphics2D):Unit = {
    g2d.setColor(inBetweenColor)
    if(points.length>2) {
      points.tail.init.sliding(2).foreach{
        case Seq(p1,p2) => g2d.draw(new Line2D.Double(p1,p2))
        case _=>
      }
    }
    
  }
  def interpolate(points:Seq[Point2D],g2d:Graphics2D):Unit = {
    g2d.setColor(inBetweenColor)
    if(points.length>2) {
      //points.sliding(2).foreach { case Seq(p1, p2) => g2d.draw(new Line2D.Double(p1, p2)) }
      cardinalSplineCurve(points,scale = .3).sliding(2).foreach { case Seq(p1, p2) => g2d.draw(new Line2D.Double(p1, p2)) }
    }
  }
  def cardinalSpline( p0:vec2, p1:vec2, v0:vec2, v1:vec2, scale:Double, t:Double):vec2 ={
    val points = DenseMatrix(p0, v0, p1, v1)
    /*
    * vec4(1,0,0,0),
      vec4(-s,0,s,0),
      vec4(2.*s,s-3.,3.-2.*s,-s),
      vec4(-s,2.-s,s-2.,s)
    * */
    val cardinalMatrix = DenseMatrix(
      (1.0,0.0,0.0,0.0),
      (-scale,0.0, scale,0.0),
      (2.0* scale, scale -3.0,3.0 - 2.0 * scale, -scale),
      (-scale,2.0 - scale, scale -2.0, scale)
      )
    val powers = DenseMatrix((1.0,t,t*t,t*t*t))
    
    val result =  powers * cardinalMatrix * points
    
    (result.apply(0,0),result.apply(0,1))
  }
  def mirrorPoint(mirror:vec2,mirrored:vec2, scale:Double):vec2={
    val v = (mirror._1 - mirrored._1,mirror._2 - mirrored._2)
    val l = sqrt(v._1*v._1 + v._2*v._2)
    val nv = (v._1/l, v._2/l)
    (nv._1*scale,nv._2*scale)
  }
  
  def cardinalSplineCurve( points:Seq[Point2D], scale:Double):Seq[Point2D] = {
    if(points.length>2) {
      val pts = points.map(p => (p.getX, p.getY))
      var cardinalSplineVelocities =
        pts
          .sliding(3)
          .map { case Seq(t, _, h) => (t._1 - h._1, t._2 - h._2) }.toSeq
      cardinalSplineVelocities = cardinalSplineVelocities
        .appended((1.0,0.0))
        .prepended((1.0,0.0))
      val ttt = tt(100)
      val input = pts.zip(cardinalSplineVelocities)
      input
        .sliding(2)
        .flatMap {
          pair =>
            val ((p0,v0),(p1,v1)) = (pair.head,pair.last)
            val points = ttt.map(t => cardinalSpline(p0, p1, v0.asInstanceOf[(Double,Double)], v1.asInstanceOf[(Double,Double)], scale, t))
            points.map(p => new Point2D.Double(p._1, p._2))
        }.toSeq
    }else{
      Seq.empty
    }
  }
  
  def tt(n:Int):Seq[Double] = for(i<-0 to n) yield 1.0/n * i
  
  def interpolationCurve(points:Seq[Point2D]):Seq[Point2D] = {
    ???
  }
  
  def ermitCurve(points:Seq[Point2D]): Seq[Point2D] ={
    ???
  }
  
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
    val ermitButton = new JButton("ERMIT")
    interpolationButton.addActionListener(_=>{mode = NATURAL; drawer.repaint()})
    ermitButton.addActionListener(_=>{mode = ERMIT;drawer.repaint()})
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
        case ERMIT => splineDrawer(drawPoint,drawVector,interpolateErmitLike,pointColor,vectorColor, splineColor, g2d)
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
      }
    })
    addMouseMotionListener(new MouseAdapter {
      override def mouseMoved( e: MouseEvent ): Unit = {
        super.mouseMoved(e)
        movingPoint.foreach(i=>{
           points = points.updated(i,e.getPoint)
          repaint()
        })
      }
    })
  }
  val drawerConstraints = pkg.getConstraints(0,1,12,24)
  add(drawer,drawerConstraints)
}

object SplinePlotter extends App{
  val frame = JFrameBasics.jFrame
  frame.add(new SplinePlotter)
  frame.revalidate()
}
