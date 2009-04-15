package edu.torax.reinforcement.robot
import framework._
import gutils._
import robot._ 

class RobotEnvironment extends Environment[RobotAction, RobotState] {
  private val actions: Array[RobotAction] = Array(RobotForwardAction,RobotLeftForwardAction,RobotRightForwardAction,RobotTurnLeftAction,RobotTurnRightAction)
  
  def actionsCount: Int = actions.size
  def prepareAction(n: Int): RobotAction = actions(n)

  val obstacles = createObstacles
  val model = createModel
  
  def state: RobotState = throw new Exception("Not Implemented Method!")

  def doAction(action: RobotAction): (RobotState, Double) = {
    action.execute(model)
    (state, reward())
  }
  
  private var terminated = false
  def isTerminated: Boolean = terminated
  
  // returns reward after last action and sets terminated to true if we reached final state
  private def reward(): Double = {
    terminated = true
    throw new Exception("Not Implemented Method!")
  }
  
  // creates an array of polygonal convex obstacles
  private def createObstacles: Array[RobotObstacle] = {
    throw new Exception("Not Implemented Method!")
  }
  
  // creates a robot model within current environment and places it at random so that
  // robot model doesn't overlap with obstacles
  private def createModel: RobotModel = {
    throw new Exception("Not Implemented Method!")
  }
}

object RobotEnvironment
{
  val turnAngle = 15.0
  val robotWidth = 5.0
  val robotHeight = 8.0
}
