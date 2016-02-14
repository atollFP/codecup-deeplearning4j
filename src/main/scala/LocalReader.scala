package ccup

object LocalReader {

//  MoveMap.loadMap()

  val I = 8628

  def read(str: String, web: Boolean = false) = {
    val splitted = str.split(',').toList
    val mvs =
      if (web)
        splitted.map(_.drop(2).dropRight(1))
      else
        splitted

    val cv = mvs.zipWithIndex.map(x => Log.readM(x._1, x._2 + 1))
    Log.read(cv)
  }


  def createSet(file: scala.tools.nsc.io.File) = {
    read(file.lines.drop(1).next)
  }


}
