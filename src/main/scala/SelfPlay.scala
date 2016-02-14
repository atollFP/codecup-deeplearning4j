package ccup

import java.io._
import java.nio.file.{Files, Paths}
import java.util.Random


import org.apache.commons.io.FileUtils
import org.deeplearning4j.datasets.iterator.DataSetIterator
import org.deeplearning4j.datasets.iterator.impl.IrisDataSetIterator
import org.deeplearning4j.eval.Evaluation
import org.deeplearning4j.nn.api.{Layer, OptimizationAlgorithm}
import org.deeplearning4j.nn.conf.layers.{OutputLayer, RBM, DenseLayer}
import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.params.DefaultParamInitializer
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners._
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.{DataSet, SplitTestAndTrain}
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.indexing._
import org.nd4j.linalg.lossfunctions.LossFunctions
import org.slf4j.LoggerFactory
import org.deeplearning4j.ui._
import org.deeplearning4j.ui.weights._


object SelfPlay {

  //Depth of the minimax
  val D = 3

  def fixedeval(g: Game) =
    g.grid.eval

  lazy val log = LoggerFactory.getLogger(getClass())

  //Self play with custom evaluation function provided by the nn
  def selfPlay(node: SearchNode)(eval: Game => Float): (SearchNode, Float) = {
    val mv = node.maxmax(D)(eval)
    (node.move(mv._1), mv._2)
  }

  //Self Play with the fixed evaluation to have a good comparison with nn performance
  def selfPlay(game: Game): Game = {

    var sn: SearchNode = new SearchNode(Some(game))
    while (sn.canContinue)
      sn = selfPlay(sn)(fixedeval)._1

    sn.node.get
  }

  //Used for TD-Leaf Error (Not currently used in the training but will later)
  var err = 0
  def error(game: Game)(eval: Game => Float):Float = {
    var sn: SearchNode = new SearchNode(Some(game))
    var sum = 0f
    var lastScore = -1f
    var delta = 1f
    var i = 0
    while (sn.canContinue && i < 10) {
      val sp = selfPlay(sn)(eval)
      //     println("SP: " + sp)
      sn = sp._1
      if (i != 0) {
        sum += ((sp._2 - lastScore) * delta).toFloat
        delta *= 0.7f
      }
      lastScore = sp._2
      i += 1
    }
    err += 1

    if (err%100 == 0 )
      println("error "+err + " : " + sum + " " + lastScore)
    sum
  }


  def train2() {

    val iterations = 1
    val seed = 456
    val listenerFreq = 1
    val batchSize = 100
    val numEx = 5000

    val server = UiServer.getInstance();
    log.info("Started on port " + server.getPort());

    log.info("Load data....")
    val iter = new GamesEpoch(batchSize, numEx, 10)

    val h1 = 100
    val h2 = 40
    log.info("Build model....")
    val conf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
      .seed(seed)
      .iterations(iterations)
      .learningRate(1e-3f)
      .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
    //      .l1(1e-1).l2(2e-4)
      .list(5)
      .layer(0, new DenseLayer.Builder()
        .nIn(50)
        .nOut(h1)
        .weightInit(WeightInit.RELU)
        .activation("relu")
        .updater(Updater.NESTEROVS)
        .build()
    )
      .layer(1, new DenseLayer.Builder()
        .nIn(h1)
        .nOut(h2)
        .weightInit(WeightInit.RELU)
        .activation("relu")
        .updater(Updater.NESTEROVS)
        .build()
    )
      .layer(2, new DenseLayer.Builder()
        .nIn(h2)
        .nOut(h2)
        .weightInit(WeightInit.RELU)
        .activation("relu")
        .updater(Updater.NESTEROVS)
        .build()
    )
      .layer(3, new DenseLayer.Builder()
        .nIn(h2)
        .nOut(h2)
        .weightInit(WeightInit.RELU)
        .activation("relu")
        .updater(Updater.NESTEROVS)
        .build()
    )
    .layer(4, new OutputLayer.Builder(LossFunctions.LossFunction.MSE)
      .nIn(h2)
      .nOut(1)
      .weightInit(WeightInit.XAVIER)
      .activation("sigmoid")
      .build()
    )
      .pretrain(false)
      .backprop(true)
      .build()
    val model = new MultiLayerNetwork(conf)
    model.init()

    model.setListeners(new HistogramIterationListener(1), new ScoreIterationListener(listenerFreq))
    log.info("Train model....")
    model.fit(iter)

    /*
    log.info("Evaluate weights....")
    model.getLayers.foreach { case (layer: org.deeplearning4j.nn.api.Layer) =>
      val w: INDArray = layer.getParam(DefaultParamInitializer.WEIGHT_KEY)
      log.info("Weights: " + w)
    }*/

    log.info("Eval data....")
    val test = new GamesIterator(batchSize, numEx).next()

    //Compare Labels and NN output
    println(test.getLabels.data().asFloat().toList.take(10))
    println(model.output(test.getFeatureMatrix, Layer.TrainingMode.TEST).data().asFloat().toList.take(10))

    //Apply the Neural net for self play. With the fixed eval, you reach < 2000, and even with D = 2 you get 1000. The neural net is stuck around 100
    def eval2(g: Game) = model.output(Nd4j.create(Array(g.toInput))).data().asFloat()(0)
    var sn = new SearchNode(Some(Game.newGame))
    var i = 1
    while (sn.canContinue) {
      sn = selfPlay(sn)(eval2)._1
      i += 1
      //Show progress of play
      if (i%100 == 0)
        println(i)
    }

    val fn = sn.node.get
    println(fn)
  }


}
