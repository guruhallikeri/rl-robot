package edu.torax.reinforcement.framework
import framework._

class UsualNNValueFunction[A <: Action, S <: State] (
  actionsCount: Int,										// how much different actions is there?
  stateDimension: Int,									// dimension of vector representation of state
  Alpha: () => Double,							
  Gamma: Double,							
  Lambda: Double,							
  init: () => Double,										// initializer of weights
  hiddenLayersDimensions: Array[Int], 	// number of neurons in each HIDDEN layer
  actFunction: NeuralNetwork.ActFunc,		// activation function for hidden layers 
  actFuncLast: NeuralNetwork.ActFunc		// activation function for output layer
) 
extends ValueFunction[A,S] {
  type Network = UsualNeuralNetwork
  private val nets = initNetworks()
  private def initNetworks(): Array[Network] = {
    val res = new Array[Network](actionsCount)
    for (i <- 0 until res.size) {
      res(i) = new Network (stateDimension, hiddenLayersDimensions ++ Array(1), 
                         Array.make(hiddenLayersDimensions.length, actFunction) ++ Array(actFuncLast)
      ) {	
        val gamma = Gamma
        val lambda = Lambda
        def alpha = Alpha()
        def initializer = init()
      }
    }
    res
  }
  
  def update(state: S, action: A, output: Double): Unit = {
    nets(action.number).tuneUp(state.encode, List(output))
  }
  
  private var iter = 0
  def apply(state: S, action: A): Double = {
    val r = nets(action.number).calculate(state.encode).head
//  	println("-- " + r)
    r
  }

  def beginEpisode() {}
  
  def toXML: xml.Elem =
    <UsualNNValueFunction 
    		actionsCount={actionsCount.toString}
  			stateDimension={stateDimension.toString}
     		gamma={Gamma.toString}
       	lamba={Lambda.toString}>
    	{ nets map (_.toXML) }
    </UsualNNValueFunction>
}
