package edu.torax.reinforcement.framework

class SeparateNNValueFunction (actionsCount: Int,	// how much different actions is there?
                               stateDimension: Int,	// dimension of vector representation of state
                               Alpha: Double,							
                               Gamma: Double,							
                               Lambda: Double,							
                               init: () => Double,						// initializer of weights
                               hiddenLayersDimensions: List[Int], 		// number of neurons in each HIDDEN layer
                               actFunction: Double => Double,			// activation function for hidden layers 
                               derFunction: Double => Double			// derivative of activation function in hidden layer
) 
extends ValueFunction {
  private val nets = initNetworks()
  private def initNetworks(): Array[NeuralNetwork] = {
    val res = new Array[NeuralNetwork](actionsCount)
    for (i <- 0 until res.size) {
      res(i) = new NeuralNetwork (stateDimension :: hiddenLayersDimensions ::: List(1), 
                         List.make(hiddenLayersDimensions.length, actFunction) ::: List((x:Double) => x),
                         List.make(hiddenLayersDimensions.length, derFunction) ::: List((x:Double) => 1.0)
      ) {	
        val gamma = Gamma
        val lambda = Lambda
        val alpha = Alpha
        def initializer = init()
      }
    }
    res
  }
  
  def update(state: Environment#State, action: Environment#Action, delta: Double): Unit = {
    for (nn <- nets) nn.tuneUp(state.encode, delta)
  }
  def apply(state: Environment#State, action: Environment#Action): Double = {
    nets(action.number).calculate(state.encode)
  }
}
