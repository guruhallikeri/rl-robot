package edu.torax.reinforcement.framework

abstract class NeuralNetwork (
  val dimensions: List[Int], 
  actFunctions: List[Double => Double],
  derFunctions: List[Double => Double])
{
  val inputDimension = dimensions.head
  val outputDimension = dimensions.last
  require(outputDimension == 1)
  
  val layersCount = dimensions.size - 1
  
  def gamma: Double
  def lambda: Double
  def alpha: Double
  def initializer(): Double
  
  val network: Array[Array[Array[Double]]] = createNetwork(initializer)
  val eTraces: Array[Array[Array[Double]]] = createNetwork(() => 0.0)
  
  protected def createNetwork(initFunc: () => Double): Array[Array[Array[Double]]] = {
   
    def createLayer(dim: Int, prevDim: Int): Array[Array[Double]] = {
      var result = new Array[Array[Double]](dim)
      for (val i <- 0 until dim) {
        result(i) = new Array[Double](prevDim + 1)
        for (val j <- 0 to prevDim) {
          result(i)(j) = initFunc()
        }
      }
      result
    }
    
    var result = new Array[Array[Array[Double]]](layersCount)
    var prevDimension = inputDimension
    var i = 0
    for (val dim <- dimensions.tail) {
      result(i) = createLayer(dim, prevDimension)
      i = i + 1
      prevDimension = dim
    }
    result
  } 
  def clearEligibility() {
    for (layer <- eTraces; neuron <- layer; i <- 0 until neuron.size)
      neuron(i) = 0.0
  }
  
  def modifyEligibility() {
    for (layer <- eTraces; neuron <- layer; i <- 0 until neuron.size)
      neuron(i) = lambda*gamma*neuron(i)
  }
  
  def tuneUp(input: List[Double], output: Double) {
    var delta = 0.0
    def processLayer(in: Array[Double],					// layer inputs 
                     k: Int,							// current processing layer number
                     funcs: List[Double => Double],		// activation functions
                     derFuncs: List[Double => Double]	// derivatives of activation functions
    ): Array[Double] = {
      def updateParameters(k: Int, grads: Array[Double]) {
        //println("k= " + k + "; delta= " + delta)
        for (val i <- 0 until network(k).size; val j <- 0 until network(k)(i).size) {
          network(k)(i)(j) -= alpha*delta*eTraces(k)(i)(j)
          //println("w: " + k + " " + i + " " + j + " = " + network(k)(i)(j))
          eTraces(k)(i)(j) = gamma*lambda*eTraces(k)(i)(j) + grads(i)*in(j)
          //println("e: " + k + " " + i + " " + j + " = " + eTraces(k)(i)(j))
        }
      }
      
      val ys = for (neuron <- network(k)) yield (0.0 /: (neuron zip in)) ((x,y) => x + y._1 * y._2)
      val grads: Array[Double] = if (k == layersCount-1) {
        delta = (output - funcs.head(ys(0)))
        Array(delta*derFuncs.head(ys(0)))
      } else {
        val outs = Array(1.0) ++ (ys map funcs.head)
        var nGrads = processLayer(outs, k+1, funcs.tail, derFuncs.tail)
        for (j <- 0 until nGrads.size) 
          nGrads(j) = nGrads(j)*derFuncs.head(ys(j))
        nGrads
      }
      var semiGrads: List[Double] = Nil
      k match {
        case 0 =>
        case _ =>
          var res: List[Double] = Nil
          for (j <- 0 until network(k-1).size) {
            var tmp = 0.0
            for(p <- 0 until network(k).size) {
              tmp += network(k)(p)(j+1)*grads(p)
            }
            semiGrads = tmp :: semiGrads 
          }
      }
      updateParameters(k, grads)
      semiGrads.toArray
    }
    processLayer((1.0 :: input).toArray, 0, actFunctions, derFunctions)
  }
  
  def calculate(input: List[Double]): Double = {
    var in = 1.0 :: input
    var actFuncs = actFunctions 
    
    def calcLayer(layer: Array[Array[Double]], function: Double => Double): Array[Double] = {
      for (neuron <- layer) yield function((0.0 /: (neuron.toList zip in)) ((x,y) => x + y._1 * y._2))
    }
    
    for (val layer <- network) {
      in = 1.0 :: calcLayer(layer, actFuncs.head).toList
      actFuncs = actFuncs.tail
    }
    assert(in.tail.size == 1)
    in.tail.head
  }
}

object NeuralNetwork {
  import Math._
  val identityFunction= (x: Double) => x
  val identityDerivative = (x:Double) => 1.0
  val logisticFunction = (x:Double) => 1.0 / (1.0 + exp(-x))
  val logisticDerivative = (x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0)
  val logisticNegFunction = (x:Double) => 2*(logisticFunction(x) - 0.5)
  val logisticNegDerivative = (x:Double) => { val t = logisticFunction(x); 2.0*t*(1-t) }
//  val hyperbolicFunction = (x:Double) => 1.7159*tanh(0.6666*x)
//  val hyperbolicDerivative = (x: Double) => 1.7159*0.6666*(1.0 - pow(tanh(0.6666*x),2.0))
}