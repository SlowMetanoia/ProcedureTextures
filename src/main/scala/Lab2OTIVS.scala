import java.awt.{ Color, GridBagConstraints, GridBagLayout, Insets, Toolkit }
import javax.swing.{ JButton, JFrame, JLabel, JPanel }
import scala.swing.Dimension
object Lab2OTIVS extends App{
  
  def getConstraints(
                      gridx:Int,
                      gridy:Int,
                      gridheight:Int,
                      gridwidth:Int,
                      gridweightx:Int = 0,
                      gridweighty:Int = 0,
                      anchor:Int = 10,
                      fill:Int = 0,
                      insets:Insets = new Insets(0,0,0,0),
                      ipadx:Int = 0,
                      ipady:Int = 0,
                    ): GridBagConstraints = {
    new GridBagConstraints(gridx,gridy,gridwidth,gridheight,gridweightx,gridweighty,anchor,fill,insets,ipadx,ipady)
  }
  
  
  
  class EventDriver(events: Map[Int,String], sequence:Map[Int,(Int,Int)]){
    val revertedEvents: Map[String, Int ] = for((k,v)<- events) yield (v, k)
    var current: String = events.getOrElse(1, throw new IllegalArgumentException("event list must contain 0 event"))
    def move(isR:Boolean): Unit = {
      val i = revertedEvents(current)
      current = if(isR) events(sequence(i)._2) else events(sequence(i)._1)
    }
    def next(isR:Boolean):String = {
      val i = revertedEvents(current)
      if(isR) events(sequence(i)._2)
      else events(sequence(i)._1)
    }
    def isLeave: Boolean = !sequence.contains(revertedEvents(current))
    def curN: Int = revertedEvents(current)
    def nextN(isR:Boolean): Int = revertedEvents(next(isR))
  }
  
  object EventDriver{
    def apply( events: Map[ Int, String ], sequence: Map[ Int, (Int, Int) ] ): EventDriver = new EventDriver(events, sequence)
    val RIGHT = true
    val LEFT = false
  }
  
  val jFrame: JFrame = new JFrame(){
    val dimension: Dimension = Toolkit.getDefaultToolkit.getScreenSize
    val windowSize: (Int, Int) = (1300, 700)
    setTitle("MyApp")
    setBackground(Color.BLACK)
    setVisible(true)
    setDefaultCloseOperation(1)
    setBounds(dimension.width/2 - windowSize._1/2,
              dimension.height/2 - windowSize._2/2,
              windowSize._1,
              windowSize._2)
  }
  
  //инит
  val events = Map(
    1 -> "Компьютер включен в розетку?",
    2 -> "Требуется включить компьютер в розетку.",
    3 -> "Розетка подключена к сети?",
    4 -> "Требуется подключить розетку к сети, или включить компьютер в работающую розетку.",
    5 -> "При нажатии кнопки включения издаёт звуки? (Шум куллера)",
    6 -> "Напряжение на материнской плате есть?",
    7 -> "Биппер издаёт звуки?",
    8 -> "Неисправен блок питания,с малой вероятностью может быть неисправна материнская плата",
    9 -> "Скорее всего неисправна материнская плата или процессор Требуется более детальный анализ, обратитесь к специалисту.",
    10 -> "Биппер встроен в материнскую плату?",
    11 -> "Требуется прослушать сигнал биппера, Найти в документации значение сигнала...- или обратиться к специалисту.",
    12 -> "Требуется подключить внешнее звуковое устройство, прослушать сигнал биппера, Найти в документации значение сигнала...- или обратиться к специалисту.",
    13 -> "Неисправна материнская плата."
  )
  
  val sequence = Map(
    1->(2,3),
    3->(4,5),
    5->(6,7),
    6->(8,9),
    7->(10,11),
    10->(12,13)
  )
  
  val eventDriver = EventDriver(events,sequence)
  
  var nextAction = false
  
  val yButton = new JButton("Yes")
  val nButton = new JButton("No")
  val aButton = new JButton("Apply event")
  
  val curEventLabel = new JLabel("")
  val nextEventLabel = new JLabel("")
  
  
  
  def renew(): Unit = {
    curEventLabel.setText(s"S${ eventDriver.curN } ${eventDriver.current}")
    if(eventDriver.isLeave)
      nextEventLabel.setText("Конечное событие")
    else
      nextEventLabel.setText(s"S${ eventDriver.nextN(nextAction) } ${eventDriver.next(nextAction)}")
  }
  renew()
  
  yButton.addActionListener( _ => {
    nextAction = true
    renew()
  })
  nButton.addActionListener( _ => {
    nextAction = false
    renew()
  })
  aButton.addActionListener( _ => {
    if(eventDriver.isLeave) {}
      else {
      eventDriver.move(nextAction)
      nextAction = false
      renew()
    }
  })
  val jPanel1 = new JPanel()
  val jPanel2 = new JPanel()
  val jPanel3 = new JPanel()
  val jPanel4 = new JPanel()
  val jPanel = new JPanel()
  jPanel.setLayout(new GridBagLayout())

  jPanel1.add(curEventLabel)
  jPanel3.add(nextEventLabel)
  jPanel2.add(yButton)
  jPanel2.add(nButton)
  jPanel4.add(aButton)
  
  jPanel.add(jPanel1,getConstraints(0,0,1,1))
  jPanel.add(jPanel2,getConstraints(0,1,1,1))
  jPanel.add(jPanel3,getConstraints(0,2,1,1))
  jPanel.add(jPanel4,getConstraints(0,3,1,1))
  jFrame.add(jPanel)
  jFrame.revalidate()
}
