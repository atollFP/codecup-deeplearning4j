package ccup

import org.deeplearning4j.datasets.fetchers._
import org.deeplearning4j.datasets.iterator._
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.dataset.{DataSet, SplitTestAndTrain}

class GameFetcher(nbEx: Int) extends BaseDataFetcher{

  totalExamples = nbEx

  var files = scala.tools.nsc.io.Directory("games/").files

  var gameList = List[Game]()

  override def hasMore() = {
    files.hasNext
  }

  override def reset() =
    files = scala.tools.nsc.io.Directory("games/").files

  override def fetch(nb: Int) = {
    var lgl = List[Game]()
    while (lgl.length < nb && hasMore()) {
      if (gameList.isEmpty) {
        val log = LocalReader.createSet(files.next())
        gameList :::= log
      }
      val toTake = (nb - lgl.length).min(gameList.length)
      lgl :::= gameList.take(toTake)
      gameList = gameList.drop(toTake)
    }

    val in1 = Nd4j.create(lgl.map(x => (x.toInput.toArray)).toArray)
    val out = Nd4j.create(lgl.map(x => Array(x.grid.eval.toFloat/3000f)).toArray)
    val next: DataSet = new DataSet(in1, out)
    next.normalizeZeroMeanZeroUnitVariance()
    curr = next
  }

}

class GamesIterator(batch: Int, nbEx:Int) extends BaseDatasetIterator(batch, nbEx, new GameFetcher(nbEx))

class GamesEpoch(batch: Int, nbEx:Int, epoch:Int) extends MultipleEpochsIterator(epoch, new GamesIterator(batch, nbEx))
