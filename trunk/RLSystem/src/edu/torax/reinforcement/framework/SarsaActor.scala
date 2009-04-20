package edu.torax.reinforcement.framework

abstract class SarsaActor[A <: Action, S <: State] (
  valueFunction: ValueFunction[A,S],
  val gamma: Double,
  listener: Actor.Event => Unit
) extends Actor[A,S](valueFunction, listener) {
  protected var state: S
  protected var action: A
  
  protected def processStep(): Boolean = {
    // returns false if we reached terminal state
    assert(state != null)
    assert(action != null)
    
    val (newState, reward) = environment.doAction(action)
    val (newAction, newActionValue) = chooseAction(newState, learning)
    if (learning) {
      val wanted = reward + gamma * newActionValue
      //println(wanted + " -=-=-=-=-=-=- ")
      valueFunction.update(state, action, wanted)
    }
    state = newState
    action = newAction
    !environment.isTerminated
  }
  
  protected def prepareEpisode(): Unit = {
    state = environment.state
    valueFunction.beginEpisode()
    action = chooseAction(state, learning)._1
  }

}
