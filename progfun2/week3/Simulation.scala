
trait Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]

  private var agenda: Agenda = List()

  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: Action): Unit = {
    val item = Event(curtime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case head::tail if head.time <= item.time =>
      head :: insert(ag, tail)
    case _ =>
      head :: ag
  }

  private def loop(): Unit = agenda match {
    case head::tail =>
      agenda = tail
      curtime = head.time
      head.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"simulation started, time is $curtime")
    }
    loop()
  }


}
