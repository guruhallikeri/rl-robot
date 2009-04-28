package edu.torax.reinforcement.framework

abstract class EligibilityNeuralNetwork (
  inputDimension: Int, 
  dimensions: Array[Int], 
  activations: Array[(Double => Double, Double => Double)])
extends NeuralNetwork(inputDimension, dimensions, activations) { 
  def gamma: Double
  def lambda: Double

  val eTraces: Array[Array[Array[Double]]] = createNetwork(() => 0.0)

  def clearEligibility() {
    for (layer <- eTraces; neuron <- layer; i <- 0 until neuron.size)
      neuron(i) = 0.0
  }
  
  def modifyEligibility() {
    for (layer <- eTraces; neuron <- layer; i <- 0 until neuron.size)
      neuron(i) = lambda*gamma*neuron(i)
  }
  
  override def tuneUp(input: List[Double], output: List[Double]) {
//    var delta = 0.0
//    def processLayer(in: Array[Double],					// layer inputs 
//                     k: Int,							// current processing layer number
//                     funcs: List[Double => Double],		// activation functions
//                     derFuncs: List[Double => Double]	// derivatives of activation functions
//    ): Array[Double] = {
//      def updateParameters(k: Int, grads: Array[Double]) {
//        //println("k= " + k + "; delta= " + delta)
//        for (val i <- 0 until network(k).size; val j <- 0 until network(k)(i).size) {
//          network(k)(i)(j) -= alpha*delta*eTraces(k)(i)(j)
//          //println("w: " + k + " " + i + " " + j + " = " + network(k)(i)(j))
//          eTraces(k)(i)(j) = gamma*lambda*eTraces(k)(i)(j) + grads(i)*in(j)
//          //println("e: " + k + " " + i + " " + j + " = " + eTraces(k)(i)(j))
//        }
//      }
//      
//      val ys = for (neuron <- network(k)) yield (0.0 /: (neuron zip in)) ((x,y) => x + y._1 * y._2)
//      val grads: Array[Double] = if (k == depth-1) {
//        delta = (output - funcs.head(ys(0)))
//        Array(delta*derFuncs.head(ys(0)))
//      } else {
//        val outs = Array(1.0) ++ (ys map funcs.head)
//        var nGrads = processLayer(outs, k+1, funcs.tail, derFuncs.tail)
//        for (j <- 0 until nGrads.size) 
//          nGrads(j) = nGrads(j)*derFuncs.head(ys(j))
//        nGrads
//      }
//      var semiGrads: List[Double] = Nil
//      k match {
//        case 0 =>
//        case _ =>
//          var res: List[Double] = Nil
//          for (j <- 0 until network(k-1).size) {
//            var tmp = 0.0
//            for(p <- 0 until network(k).size) {
//              tmp += network(k)(p)(j+1)*grads(p)
//            }
//            semiGrads = tmp :: semiGrads
//            semiGrads = semiGrads.reverse
//          }
//      }
//      updateParameters(k, grads)
//      semiGrads.toArray
//    }
//    processLayer((1.0 :: input).toArray, 0, actFunctions, derFunctions)
		throw new Exception("Not Implemented")
  }
}
