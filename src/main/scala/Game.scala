package ccup


sealed trait Move
case object Start extends Move
case class Turn(dir: Direction) extends Move
case class Place(x: Int, y: Int, c: Color) extends Move

case class Game(grid: Grid, turn: Int, maxScore: Int) {

  def placeColor =
    Game.placeColor(turn+1)

  lazy val toInput =
      (List((turn%2).toFloat, turn.toFloat):::grid.toInput).toArray

}

case class Log(hist: List[(Game, Move)])

object Game {

  val ALL_TURNS = Set(Turn(Up), Turn(Down), Turn(Right), Turn(Left))

  def player(turn: Int) =
    (turn+1)%2

  def placeColor(turn: Int): Option[Color] =
    (turn % 5) match {
      case 0 | 4 => None
      case 1 => Some(Blue)
      case 2 => Some(Red)
      case 3 => Some(Gray)
    }


  def newGame =
    Game(Grid(), 0, 0)

}

object Log {

  def read(s: List[Move]) = {
    val sn = new SearchNode(Some(Game.newGame))
    val l = s.foldLeft((sn, List[Game]()))((acc, pos) => {
      val nsn = acc._1.move(pos)
      (nsn, nsn.node.get::acc._2)
    })
    l._2
  }

  def readM(m: String, turn: Int) =
    m match {
      case "U" => Turn(Up)
      case "D" => Turn(Down)
      case "R" => Turn(Right)
      case "L" => Turn(Left)
      case _ =>
        val y = m(0).toString.toInt-1
        val x = m(1).toString.toInt-1
        val c = Game.placeColor(turn).get
        Place(x, y, c)

    }

}
