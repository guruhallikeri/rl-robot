package edu.torax.reinforcement.framework

abstract class SarsaActor (
  valueFunction: ValueFunction,
  val gamma: Double,
  listener: Actor.Event => Unit
) extends Actor(valueFunction, listener) {
  
  private var state: State = null;
  private var action: Action = null;
  
  protected def processStep(): Boolean = {
    // returns false if we reached terminal state
    assert(state != null)
    assert(action != null)
    
    val (newState, reward) = environment.doAction(action)
    val (newAction, newActionValue) = chooseAction(newState, learning)
    if (learning) {
      val delta = reward + gamma * newActionValue - valueFunction(state, action)
      valueFunction.update(state, action, delta)
    }
    state = newState
    action = newAction
    !environment.isTerminal(state)
  }
  
  protected def prepareEpisode(): Unit = {
    state = environment.state
    action = chooseAction(state, learning)._1
  }

}
