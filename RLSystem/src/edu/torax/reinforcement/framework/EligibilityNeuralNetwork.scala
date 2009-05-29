package edu.torax.reinforcement.framework

abstract class EligibilityNeuralNetwork (
  inputDimension: Int, 
  dimensions: Array[Int], 
  activations: Array[NeuralNetwork.ActFunc])
extends NeuralNetwork(inputDimension, dimensions, activations) {
  require(dimensions(depth-1) == 1)
  def gamma: Double
  def lambda: Double

  val traces = createNetwork(() => 0.0)
	override def tuneUp(input: List[Double], output: List[Double]) {
	  val eta = alpha
	  val lam = lambda
	  val gam = gamma
	  var delta = Double.NaN 
	  def processLayer(in: Array[Double], layer: Int): Array[Double] = {
	    val indFields = (for (i <- 0 until dimensions(layer)) 
        yield (0.0 /: (network(layer)(i) zip in)) ((x,y) => x + y._1 * y._2)).toArray 

      val grads = if (layer == depth-1) {
        (indFields zip output.toArray) map  (x => activations(layer).deriv(x._1)*(x._2 - activations(layer).func(x._1)))
	  	} else {
	  	  (processLayer(Array(1.0) ++ (indFields map (x => activations(layer).func(x))), layer+1) zip indFields) map 
        	(x => x._1 * activations(layer).deriv(x._2)) 
	  	}
      
	    val curLayer = network(layer)
	    val curTraces = traces(layer) 
      val semiGrads = if (layer == 0) null else {
        val res = new Array[Double](dimensions(layer-1))
        for (j <- 0 until dimensions(layer-1)) {
          var sum = 0.0
          for (k <- 0 until dimensions(layer)) {
            sum += curLayer(k)(j+1)*grads(k)
          }
          res(j) = sum
        }
        res
      }

      if (layer == depth-1)
        delta = output.head - activations(depth-1).func(indFields(0))

      val I = curLayer(0).size
      for (j <- 0 until dimensions(layer)) {
        for (i <- 0 until I) {
          curTraces(j)(i) = gam*lam*curTraces(j)(i) - grads(j)*in(i)  
          curLayer(j)(i) += eta*delta*curTraces(j)(i)
         // println(" +++ " + eta*grads(j)*in(i) + " ! " + eta + " ! " + grads(j) + " ! " + in(i))
        }
      }
      
      semiGrads
	  }
   
	  processLayer((1.0 :: input).toArray, 0)
	  ()
	}
 
  def modifyEligibility() {
    val lam = lambda
    val gam = gamma
    for (layer <- traces; neuron <- layer; i <-0 until neuron.size) {
      neuron(i) = lam*gam*neuron(i)
    }
  }
  def clearEligibility() {
    for (layer <- traces; neuron <- layer; i <-0 until neuron.size) {
      neuron(i) = 0.0
    }
  }
  
  def toXML: xml.Elem = throw new Exception("Not implemented method!")
}