package ccup

import util.Random

sealed trait Color
case object Blue extends Color
case object Red extends Color
case object Gray extends Color

sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Right extends Direction
case object Left extends Direction

/*
grid coordinates expressed as yx:

11 12 13 13
21 22 23 24
31 32 33 34
41 42 43 44

 */
trait Grid  {

  def eval: Int
  def value: Int

  //The encoding I use is a list of all the cell and each cell encode as (ValueIfBlue, ValueIfRed, ValueIfGray) R9 is encoded as (0, 9, 0) - as (0, 0, 0)
  def toInput: List[Float]
  def place(x:Int, y: Int, piece: Piece): Option[Grid]
  def move(dir: Direction): Grid
  def emptySpots: Seq[(Int, Int)]

}

case class Piece(value: Int, color: Color) {

  def combine =
    copy(value = value*3)

  def log =
    value

  override def toString() = {
    val c = color match {
      case Blue => "B"
      case Red => "R"
      case Gray => "G"
    }
    c + log.toString
  }
}


object Grid {

  def apply(): Grid =
    GridIS(IndexedSeq.fill(4,4)(None))

  def randomPiece() =
    if (Random.nextBoolean) {
      val v = Math.pow(3, Random.nextInt(4)).toInt
      val c = Random.shuffle(Seq(Blue, Red, Gray)).head
      Some(Piece(v, c))
    }
    else
      None

  def random() =
    GridIS(IndexedSeq.fill(4,4)(randomPiece))

}


case class GridIS(grid: IndexedSeq[IndexedSeq[Option[Piece]]]) extends Grid {

  def get(x: Int, y:Int) =
    grid(y)(x)

  def row(y:Int) =
    grid(y)

  def col(x:Int) =
    (0 to 3).map(y => grid(y)(x)).toIndexedSeq

  def all =
    (0 to 15).map(i => grid(i/4)(i%4)).filter(_.isDefined).map(_.get)

  def place(x:Int, y:Int, piece: Piece) =
    if (get(x, y) == None)
      Some(copy(grid = grid.updated(y, grid(y).updated(x, Some(piece)))))
    else
      None

  def move(dir: Direction = Up) = {

    val ar = Array.fill[Option[Piece]](4, 4)(None)

    val ns =
      (dir == Up || dir == Down) //NORTHSOUTH
    val dr =
      (dir == Down || dir == Right) //DOWNRIGHT
    val range =
      if (dr)
        (3 to 0 by -1)
      else
        (0 to 3)


    for (i <- 0 to 3) {

      var bef:Option[Piece] = None
      var inc =
        if (dr)
          4
        else
          -1

      for (j <- range) {

        val c =
          if (ns)
            grid(j)(i)
          else
            grid(i)(j)

        c match {
          case Some(x) if (bef.exists(_ == x)) =>
            val comb = Some(x.combine)
            if (ns)
              ar(inc)(i) = comb
            else
              ar(i)(inc) = comb

            bef = None


          case Some(x) if (bef.exists(_.value == x.value)) =>
            if (ns)
              ar(inc)(i) = None
            else
              ar(i)(inc) = None

            if (dr)
              inc += 1
            else
              inc -= 1

            bef = None


          case sx@Some(x) =>
            if (dr)
              inc -= 1
            else
              inc += 1

            if (ns)
              ar(inc)(i) = sx
            else
              ar(i)(inc) = sx

            bef = sx

          case None => ()
        }
      }
    }

    copy(grid = ar.map(_.toIndexedSeq).toIndexedSeq)
  }


  def emptySpots =
    for {
      i <- 0 to 3
      j <- 0 to 3 if grid(j)(i) == None
    } yield (i, j)

  def value =
    grid.map(_.map(_.map(_.value).getOrElse(0)).sum).sum

  def edge =
    List(0, 3).map(row(_).map(_.map(_.value).getOrElse(0)).sum).sum +
  List(0, 3).map(col(_).map(_.map(_.value).getOrElse(0)).sum).sum


  def empty_squares = {
    grid.map(_.map(_.map(_.value.toFloat).getOrElse(0.5f)).sum).sum
  }

  def bestSum = {
    (0::all.groupBy(_.color).map { case (x, y) => y.map(_.value).sum }.toList).max
  }


  def eval =
    bestSum


  def cellToInput(opt: Option[Piece]):List[Float] =
    opt.map(x => {
      val value = x.value
      x.color match {
        case Blue => List(value, 0f, 0f)
        case Red => List(0f, value, 0f)
        case Gray => List(0f, 0f, value)
      }
    }).getOrElse(List(0f, 0f, 0f))

  lazy val toInput:List[Float] = {
    val inp = for {
      i <- 0 to 3
      j <- 0 to 3
    } yield cellToInput(grid(j)(i))

    (inp.toList).flatten

  }
  override def toString() = {
    val ESP = 3
    var str = ""
    str += "value: " + value + "\n"
    for (y <- 0 to 3) {
      str += "| "
      for (x <- 0 to 3) {
        val s = get(x, y).map(_.toString).getOrElse("_")
        val l = s.length/2.0
        str += (" "*ESP).drop(l.floor.toInt) + s + (" "*ESP).drop(l.ceil.toInt)
      }
      str += " |\n"
    }
    str
  }
}
