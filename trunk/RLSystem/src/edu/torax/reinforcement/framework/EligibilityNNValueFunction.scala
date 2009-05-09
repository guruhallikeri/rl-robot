package edu.torax.reinforcement.framework

class EligibilityNNValueFunction[A <: Action, S <: State] (
  actionsCount: Int,	// how much different actions is there?
  stateDimension: Int,	// dimension of vector representation of state
  Alpha: () => Double,							
  Gamma: Double,							
  Lambda: Double,							
  init: () => Double,						// initializer of weights
  hiddenLayersDimensions: Array[Int], 		// number of neurons in each HIDDEN layer
  actFunction: (Double => Double,	Double => Double),		// activation function for hidden layers 
  actFuncLast: (Double => Double, Double => Double)			// activation function for output layer
) 
extends ValueFunction[A,S] {
  type Network = EligibilityNeuralNetwork
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
    for (i <- 0 until nets.size; if (i != action.number))
      nets(i).modifyEligibility()
  }
  
  private var iter = 0
  def apply(state: S, action: A): Double = {
    val r = nets(action.number).calculate(state.encode).head
    
//    iter += 1
//    if (iter % 997 == 0) {
    	println("-- " + r)
//    }
    r
  }

  def beginEpisode() {
    for (net <- nets) net.clearEligibility()
  }
}
