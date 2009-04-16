package edu.torax.reinforcement
import framework._
import Math._
import robot._
import acrobot._

object RLMizky {
  
  val gamma = 0.9
  val alpha = 0.1
  val lambda = 0.85
  val greedyEps = 0.1
  
  val valueFunc = createValueFunction 
  var env = createEnvironment
  val actor = createActor

  private def createValueFunction: ValueFunction[RobotAction, RobotState] = null
  private def createEnvironment = null
  private def createActor = {
    new SarsaActor(valueFunc, gamma, processMessage) with EpsGreedyPolicy[RobotAction,RobotState] {
      val eps = greedyEps
      protected var action: RobotAction = null
      protected var state: RobotState = null
    }
  }
  
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
      println("Episode Started")
      
    case ev: Actor.EpisodeFinished[_,_] =>
      println("Episode Finished")
      
    case ev: Actor.StepFinished[_,_] =>
      println("Step Finished")
  }
  
  def main(args : Array[String]) : Unit = {
    
    
  }
}
