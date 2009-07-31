package edu.torax.reinforcement.framework

abstract class Actor[A <: Action, S <: State] (
  val valueFunction: ValueFunction[A,S],
  var listener: Actor.Event => Unit ) 
{
  private var env: Environment[A,S] = null
  def environment = env
  private def environment_=(environ: Environment[A,S]) { env = environ }
  
  protected def processStep(): Boolean	// returns false if we reached terminal state
  protected def prepareEpisode(): Unit = {
    valueFunction.beginEpisode()
  }
  
  protected def chooseAction(state: S, inLearning: Boolean): (A, Double)
  
  def doStep() {
    if (processStep()) {
      listener(Actor.StepFinished(this))
    } else {
      listener(Actor.StepFinished(this))
      listener(Actor.EpisodeFinished(this))
    }
  }

  private var isLearning: Boolean = true
  def learning = isLearning
  private def learning_=(isLearning: Boolean) { this.isLearning = isLearning }
  
  def beginEpisode(env: Environment[A,S], isLearning: Boolean) {
    environment = env
    learning = isLearning

    listener(Actor.EpisodeStarted(this))
    prepareEpisode()
  }
}

object Actor {
  sealed abstract class Event
  case class EpisodeStarted[A <: Action, S <: State](actor: Actor[A,S]) extends Event
  case class EpisodeFinished[A <: Action, S <: State](actor: Actor[A,S]) extends Event
  case class StepFinished[A <: Action, S <: State](actor: Actor[A,S]) extends Event
}