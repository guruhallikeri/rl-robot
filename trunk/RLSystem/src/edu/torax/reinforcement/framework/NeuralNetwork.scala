package edu.torax.reinforcement.framework

abstract class NeuralNetwork (
		val inputDimension: Int, 
		val dimensions: Array[Int], 
		val activations: Array[NeuralNetwork.ActFunc],
		val alpha: GeneralFunction,
		val initializer: NeuralNetwork.Initializer
) {
	require(dimensions.size == activations.size)
	val depth = dimensions.size
	val outputDimension = dimensions(depth-1)

	def tuneUp(input: List[Double], output: List[Double]): Unit
	def toXML: xml.Elem
 
	val network: Array[Array[Array[Double]]] = createNetwork(initializer)

	protected def neuronToXml(neuron: Array[Double]) = <neuron>{ neuron map (x => <double>{x.toString}</double>) }</neuron>
  protected def layerToXml(layer: Array[Array[Double]]) = <layer>{ layer map neuronToXml _}</layer>
  protected def dimensionsToXML = <dimensions>{ (Array(inputDimension) ++ dimensions) map (x => <int>x.toString</int>) }</dimensions>

  protected def neuronFromXML(elem: xml.NodeSeq) = ((elem \ "double") map (x => x.text.toDouble)).toArray
  protected def layerFromXML(elem: xml.NodeSeq) = ((elem \ "neuron") map (x => neuronFromXML(x))).toArray
  protected def networkFromXML(elem: xml.NodeSeq) = ((elem \ "layer") map (x => layerFromXML(x))).toArray
  protected def dimensionsFromXML(elem: xml.NodeSeq) = ((elem \ "int") map (x => x.text.toInt)).toArray
  protected def activationsFromXML(elem: xml.NodeSeq) = ((elem \ "actFunc") map (x => NeuralNetwork.ActFunc.fromXML(x))).toArray
  
	protected def createNetwork(initFunc: NeuralNetwork.Initializer): Array[Array[Array[Double]]] = {
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
  // ----------------------------- Activation Functions -------------------------------------
  case class ActFunc (name: String, func: Double => Double, deriv: Double => Double) {
    def toXML = <actFunc name={name}/>
  }
  
  object ActFunc {
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
   
  	def fromXML(elem: xml.NodeSeq) = (elem \ "@name").text match {
      case "identity" => identity
      case "logistic" => logistic
      case "logisticNeg" => logisticNeg
      case "tanh" => tanh
      case _ => throw new Exception("Unknown type of activation function!")
    }
  }
  
  // ---------------------------------- Weights Initializers ------------------------------------------
  abstract class Initializer(val name: String) {
    def apply(): Double
    def toXML: xml.Elem
  } 
  object Initializer {
    def fromXML(elem: xml.NodeSeq) = (elem \ "@name").text match {
      case "constant" => new ConstantInitializer(elem)
      case "uniformRandom" => new UniformRandomInitializer(elem)
      case unknown => throw new Exception("Unknown type of initializer: " + unknown)
    }
  }
  
  case class ConstantInitializer(value: Double) extends Initializer("constant") {
    def this(elem: xml.NodeSeq) = this((elem \ "@value").text.toDouble)
    def apply() = value
    def toXML = <initializer name={name} value={value.toString}/>
  }
  
  case class UniformRandomInitializer(min: Double, max: Double) extends Initializer("uniformRandom") {
    def this(elem: xml.NodeSeq) = this((elem \ "@min").text.toDouble, (elem \ "@max").text.toDouble)
    def apply() = min + Math.random*(max-min)
    def toXML = <initializer name={name} min={min.toString} max={max.toString}/>
  }
  
  // --------------------------------- General Deserialization -----------------------------------------
	def fromXML(elem: xml.NodeSeq): NeuralNetwork = elem(0).label match {
	  case "UsualNeuralNetwork" => new UsualNeuralNetwork(elem)
	  case unknown => throw new Exception("Unknown type of neural network: " + unknown)
	}
}