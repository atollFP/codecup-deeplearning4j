package ccup

class SearchNode(val node: Option[Game]) {

  val children = collection.mutable.Map[Move, SearchNode]()

  def canContinue =
    node.get.turn < 1000 &&  !availableMoveNext.isEmpty

  def searchScore(depth: Int)(eval: Game => Float): Float = {
    //    println("D"+depth)
    if (node.isEmpty)
      -1
    else {
      val g = node.get
      val aM = availableMoveNext
      if (depth == 0 || g.turn == 1000 || aM.isEmpty)
        eval(g)
      else
      {
        var max = -1f
        for (m <- aM) {
          val sn = move(m)
          max = sn.searchScore(depth-1)(eval).max(max)
        }
        max
      }
    }
  }

  def maxmax(depth: Int)(eval: Game => Float):(Move, Float) = {
    val aM = availableMoveNext
    if (aM.isEmpty)
      (Start, -1)
    else
      aM.map(x => (x, move(x).searchScore(depth-1)(eval))).maxBy(_._2)
  }

  def canTurn(dir: Direction) = {
    move(Turn(dir)).node != node
  }

  def move(mv: Move) = {
    if (node.isEmpty) {
      System.err.println("Move an empty search node")
      sys.exit(0)
    }
    /*
        else if (children.contains(mv)) {
      children(mv)
  }
  */
    else {
      val gn = node.get
      val ng = mv match {
        case Turn(dir) if gn.placeColor.isEmpty =>
          Some(gn.grid.move(dir))
//          Some(gn.grid)
        case Place(x, y, c) if gn.placeColor.exists(_ == c) =>
//          Some(gn.grid)
          gn.grid.place(x, y, new Piece(1, c))
        case _ =>
          println("NOOOZ")
          None //:<
      }
      val ogame = ng.map(g => Game(g, gn.turn+1, gn.maxScore.max(ng.get.value)))
      val sn = new SearchNode(ogame)
//      children += ((mv, sn))
      sn
    }
//     */
  }

  def availableMoveGen(lturn: Int) = {
    if (lturn == 0)
      List(Start)
    else
      (lturn % 5) match {
        case 0 | 4 => Game.ALL_TURNS filter (x => canTurn(x.dir))
        case t =>
          val color = Game.placeColor(t).get
          node.get.grid.emptySpots.map(p => Place(p._1, p._2, color))
      }
  }

  def availableMoveNext =
    availableMoveGen(node.get.turn+1)

}
