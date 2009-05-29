package edu.torax.reinforcement.framework

abstract class NeuralNetwork (
		val inputDimension: Int, 
		val dimensions: Array[Int], 
		val activations: Array[NeuralNetwork.ActFunc])
{
	require(dimensions.size == activations.size)
	val depth = dimensions.size
	val outputDimension = dimensions(depth-1)

	def alpha: Double
	def initializer(): Double
	def tuneUp(input: List[Double], output: List[Double]): Unit
	
	def toXML: xml.Elem
 
	val network: Array[Array[Array[Double]]] = createNetwork(initializer)

	protected def neuronToXml(neuron: Array[Double]) = 
	    <neuron inputs={neuron.size.toString}>
       	{ neuron map (x => <Double>{x.toString}</Double>) }
      </neuron>

  protected def layerToXml(layer: Array[Array[Double]]) = 
	  <layer neuronsCount={ layer.size.toString }>
	   	{ layer map neuronToXml _}
	  </layer>

	protected def createNetwork(initFunc: (/*Int,Int,Int*/) => Double): Array[Array[Array[Double]]] = {
			def createLayer(layer: Int, dim: Int, prevDim: Int): Array[Array[Double]] = {
					var result = new Array[Array[Double]](dim)
					for (val i <- 0 until dim) {
						result(i) = new Array[Double](prevDim + 1)
						for (val j <- 0 to prevDim) {
							result(i)(j) = initFunc()//(layer, i, j)
						}
					}
					result
			}

			val result = new Array[Array[Array[Double]]](depth)
			for (i <- 0 until dimensions.size) {
				result(i) = createLayer(i, dimensions(i), if (i==0) inputDimension else dimensions(i-1))
			}
			result
	} 

	def calculate(input: List[Double]): List[Double] = {
//		println(input.size + "   " + inputDimension)
	  require(input.size == inputDimension)
		def calcLayer(in: Array[Double], layer: Int): List[Double] = {
			val out = for (i <- 0 until dimensions(layer)) 
				yield activations(layer).func((0.0 /: (network(layer)(i) zip in)) ((x,y) => x + y._1 * y._2)) 
			if (layer == depth - 1) {
				out.toList
			} else {
				calcLayer(Array(1.0) ++ out, layer + 1)
			}
		}

		calcLayer((1.0 :: input).toArray, 0)
	}
		}

object NeuralNetwork {
  
  case class ActFunc (name: String, func: Double => Double, deriv: Double => Double) {
    def toXML = <actFunc name={name}/>
  }
  
  object ActFunc {
    def fromXML(elem: xml.Elem) = (elem \ "@name").text match {
      case "identity" => identity
      case "logistic" => logistic
      case "logisticNeg" => logisticNeg
      case "tanh" => tanh
      case _ => throw new Exception("Unknown type of activation function!")
    }
  }
  
	import Math._
	val identityFunction= (x: Double) => x
	val identityDerivative = (x:Double) => 1.0
	val identity = ActFunc("identity", identityFunction, identityDerivative)

	val logisticFunction = (x:Double) => 1.0 / (1.0 + exp(-x))
	val logisticDerivative = (x:Double) => exp(-x) / pow(1.0 + exp(-x), 2.0)
	val logistic = ActFunc("logistic", logisticFunction, logisticDerivative)

	val logisticNegFunction = (x:Double) => 2*(logisticFunction(x) - 0.5)
	val logisticNegDerivative = (x:Double) => { val t = logisticFunction(x); 2.0*t*(1-t) }
	val logisticNeg = ActFunc("logisticNeg", logisticNegFunction, logisticNegDerivative)

	private val tanhA = 1.7159
	private val tanhB = 2.0/3.0
	val tanhFunction = (x:Double) => tanhA*java.lang.Math.tanh(tanhB*x)
	val tanhDerivative = (x: Double) => { 
		val phiX = tanhFunction(x)
		tanhB/tanhA*(tanhA-phiX)*(tanhA+phiX)
	}
	val tanh = ActFunc("tanh", tanhFunction, tanhDerivative)
}