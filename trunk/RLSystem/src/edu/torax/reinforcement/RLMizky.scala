package edu.torax.reinforcement
import edu.torax.reinforcement.framework.tests.NeuralNetworkTests
import edu.torax.reinforcement.framework._
import Math._

object RLMizky {
  
  val gamma = 0.9
  val alpha = 0.1
  val lambda = 0.85
  val greedyEps = 0.1
  
  val valueFunc = createValueFunction 
  var env: Environment = createEnvironment
  val actor: Actor = createActor

  private def createValueFunction = null
  private def createEnvironment = null
  private def createActor = {
    val actor = new SarsaActor(valueFunc, gamma, processMessage) with EpsGreedyPolicy { val eps = greedyEps }
    actor
  }
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted =>
      println("Episode Started")
      
    case ev: Actor.EpisodeFinished =>
      println("Episode Finished")
      
    case ev: Actor.StepFinished =>
      println("Step Finished")
  }
  
  def main(args : Array[String]) : Unit = {
    
    
  }
}
