package edu.torax.reinforcement.framework
import framework._

class UsualNNValueFunction[A <: Action, S <: State] (
  actionsCount: Int,										// how much different actions is there?
  stateDimension: Int,									// dimension of vector representation of state
  alpha: GeneralFunction,							
  init: NeuralNetwork.Initializer,			// initializer of weights
  hiddenLayersDimensions: Array[Int], 	// number of neurons in each HIDDEN layer
  actFunction: NeuralNetwork.ActFunc,		// activation function for hidden layers 
  actFuncLast: NeuralNetwork.ActFunc		// activation function for output layer
) extends ValueFunction[A,S] {
  type Network = UsualNeuralNetwork
  private val nets = initNetworks()
  private def initNetworks(): Array[Network] = {
    val res = new Array[Network](actionsCount)
    for (i <- 0 until res.size) {
      res(i) = new Network (stateDimension, hiddenLayersDimensions ++ Array(1), 
                         		Array.make(hiddenLayersDimensions.length, actFunction) ++ Array(actFuncLast), 
                         		alpha, init)
    }
    res
  }
  
  def update(state: S, action: A, output: Double): Unit = {
    nets(action.number).tuneUp(state.encode, List(output))
  }
  
  def apply(state: S, action: A): Double = nets(action.number).calculate(state.encode).head
  def beginEpisode() {}
  
  def toXML: xml.Elem =
    <UsualNNValueFunction 
    		actionsCount={actionsCount.toString}
  			stateDimension={stateDimension.toString}>
      { alpha.toXML }
      { init.toXML }
      <hiddenLayersDimensions>
      	{ hiddenLayersDimensions map (x => <int>{x.toString}</int> ) }
       </hiddenLayersDimensions>  
      <hiddenActFunc>{ actFunction.toXML }</hiddenActFunc>
      <outputActFunc>{ actFuncLast.toXML }</outputActFunc>
    	<networks>{ nets map (_.toXML) }</networks>
    </UsualNNValueFunction>
    
  // from XML
  def this(elem: xml.NodeSeq) = {
    this(
      (elem \ "@actionsCount").text.toInt,
      (elem \ "@stateDimension").text.toInt,
      GeneralFunction.fromXML(elem \ "generalFunction"),
      NeuralNetwork.Initializer.fromXML(elem \ "initializer"),
      ((elem \ "hiddenLayersDimensions" \ "int") map (x => x.text.toInt)).toArray,
      NeuralNetwork.ActFunc.fromXML(elem \ "hiddenActFunc" \ "actFunc"),
      NeuralNetwork.ActFunc.fromXML(elem \ "outputActFunc" \ "actFunc")
    )
    var i=0
    for (val node <- (elem \ "networks" \ "UsualNeuralNetwork")) {
      nets(i) = NeuralNetwork.fromXML(node).asInstanceOf[Network]
      i += 1
    }
    if (i != actionsCount) 
      throw new Exception("Neural networks amount not sufficient! Needed: " + actionsCount + ", but found: " + i)
  }
}
