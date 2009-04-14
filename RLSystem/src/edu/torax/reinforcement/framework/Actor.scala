package edu.torax.reinforcement.framework

abstract class Actor (
  val valueFunction: ValueFunction,
  listener: Actor.Event => Unit ) 
{
  private var env: Environment = null
  def environment = env
  private def environment_=(environ: Environment) { env = environ }
  
  protected def processStep(): Boolean	// returns false if we reached terminal state
  protected def prepareEpisode(): Unit
  
  def chooseAction(state: Environment#State, inLearning: Boolean): (Environment#Action, Double)
  
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
  
  def beginEpisode(env: Environment, isLearning: Boolean) {
    environment = env
    learning = isLearning

    listener(Actor.EpisodeStarted(this))
    prepareEpisode()
    listener(Actor.EpisodeFinished(this))
  }
}

object Actor {
  sealed abstract class Event
  case class EpisodeStarted(actor: Actor) extends Event
  case class EpisodeFinished(actor: Actor) extends Event
  case class StepFinished(actor: Actor) extends Event
}