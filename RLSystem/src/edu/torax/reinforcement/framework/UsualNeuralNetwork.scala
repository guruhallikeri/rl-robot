package edu.torax.reinforcement.framework

abstract class UsualNeuralNetwork (
  inputDimension: Int, 
  dimensions: Array[Int], 
  activations: Array[NeuralNetwork.ActFunc])
extends NeuralNetwork(inputDimension, dimensions, activations) {
	override def tuneUp(input: List[Double], output: List[Double]) {
	  val eta = alpha
   
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
      
      val I = curLayer(0).size
      for (j <- 0 until dimensions(layer)) {
        for (i <- 0 until I) {
          curLayer(j)(i) += eta*grads(j)*in(i)
         // println(" +++ " + eta*grads(j)*in(i) + " ! " + eta + " ! " + grads(j) + " ! " + in(i))
        }
      }
      
      semiGrads
	  }
   
	  processLayer((1.0 :: input).toArray, 0)
	  ()
	}
 
	def toXML: xml.Elem =
	  <UsualNeuralNetwork>
	  	<activations size={activations.size.toString}>
      	{ activations map (_.toXML) }
      </activations>
	  	<layers size={network.size.toString}>
	  		{ network map ( layerToXml(_) ) }
	  	</layers>
	  </UsualNeuralNetwork>
}