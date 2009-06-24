package edu.torax.reinforcement.framework

abstract class QActor[A <: Action, S <: State] (
  valueFunction: ValueFunction[A,S],
  val gamma: Double,
  listener: Actor.Event => Unit
) extends Actor[A,S](valueFunction, listener) {
  
  protected def processStep(): Boolean = {
    // returns false if we reached terminal state
    val state = environment.state
    val (action, actVal) = chooseAction(state, learning)
    val (newState, reward) = environment.doAction(action)
    
    var maxNextReward = Double.NegativeInfinity
    for (i <- 0 until environment.actionsCount) {
      maxNextReward = maxNextReward max valueFunction(newState, environment.prepareAction(i))
    }
    if (learning) {
      val wanted = reward + gamma * maxNextReward
      valueFunction.update(state, action, wanted)
    }
    !environment.isTerminated
  }
}
