package edu.torax.reinforcement.framework
import framework._

class SeparateNNValueFunction[A <: Action, S <: State] (
  actionsCount: Int,	// how much different actions is there?
  stateDimension: Int,	// dimension of vector representation of state
  Alpha: Double,							
  Gamma: Double,							
  Lambda: Double,							
  init: () => Double,						// initializer of weights
  hiddenLayersDimensions: List[Int], 		// number of neurons in each HIDDEN layer
  actFunction: Double => Double,			// activation function for hidden layers 
  derFunction: Double => Double			// derivative of activation function in hidden layer
) 
extends ValueFunction[A,S] {
  private val nets = initNetworks()
  private def initNetworks(): Array[NeuralNetwork] = {
    val res = new Array[NeuralNetwork](actionsCount)
    for (i <- 0 until res.size) {
      res(i) = new NeuralNetwork (stateDimension :: hiddenLayersDimensions ::: List(1), 
                         List.make(hiddenLayersDimensions.length, actFunction) ::: List(NeuralNetwork.identityFunction),//List(NeuralNetwork.logisticNegFunction),//List((x:Double) => x),
                         List.make(hiddenLayersDimensions.length, derFunction) ::: List(NeuralNetwork.identityDerivative)//List(NeuralNetwork.logisticNegDerivative)//List((x:Double) => 1.0)
      ) {	
        val gamma = Gamma
        val lambda = Lambda
        val alpha = Alpha
        def initializer = init()
      }
    }
    res
  }
  
  def update(state: S, action: A, output: Double): Unit = {
    nets(action.number).tuneUp(state.encode, output)
    for (i <- 0 until nets.size; if (i != action.number))
      nets(i).modifyEligibility()
  }
  
  def apply(state: S, action: A): Double = {
    val r = nets(action.number).calculate(state.encode)
   // println("-- " + r)
    r
  }
  
  def beginEpisode() {
    for (net <- nets) net.clearEligibility()
  }
}