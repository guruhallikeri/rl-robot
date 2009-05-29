package edu.torax.reinforcement.framework

abstract class SarsaActor[A <: Action, S <: State] (
  valueFunction: ValueFunction[A,S],
  val gamma: Double,
  listener: Actor.Event => Unit
) extends Actor[A,S](valueFunction, listener) {
  
  protected def processStep(): Boolean = {
    // returns false if we reached terminal state
    val state = environment.state
    val (action, actVal) = chooseAction(state, learning)
    val (newState, reward) = environment.doAction(action)
    val (newAction, newActionValue) = chooseAction(newState, false)
    if (learning) {
      val wanted = reward + gamma * newActionValue
//      val wanted = actVal*0.7 + 0.3*(reward + gamma * newActionValue)
      //println(wanted + " -=-=-=-=-=-=- ")
      valueFunction.update(state, action, wanted)
    }
    !environment.isTerminated
  }
  
  protected def prepareEpisode(): Unit = {
    valueFunction.beginEpisode()
  }

}
