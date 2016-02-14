package ccup


object SoloGame extends App {

  val st = System.currentTimeMillis
//  BasicConfigurator.configure();

//  val g = SelfPlay.selfPlay(Game.newGame)
//  println(g)

  SelfPlay.train2()

  val et = (System.currentTimeMillis - st)/1000f
  println("TIME: " + et)

}
