package edu.torax.reinforcement.robot
import framework._
import gutils._
import robot._ 

class RobotEnvironment (
  val width: Double,				// width of the environment (room)
  val height: Double,				// height of the environment (room)
  val timeOut: Int,					// number of moves till time out
  val turnAngle: Double,			// what angle to turn left or right in one action
  val moveDistance: Double,			// what distance to move in one action
  val visionAngle: Double,			// the central angle of vision sector
  obstacleGenerator: (RobotEnvironment) => Array[RobotObstacle],	// generates initial configuration of obstacles
  modelGenerator: (RobotEnvironment) => RobotModel,					// generates model and places it somehow
  val goalPosition: (RobotEnvironment) => Vector					// gives position of the goal in environment
) extends Environment[RobotAction, RobotState] {
  private val actions: Array[RobotAction] = Array(RobotForwardAction,RobotLeftForwardAction,RobotRightForwardAction,RobotTurnLeftAction,RobotTurnRightAction)
  
  def actionsCount: Int = actions.size
  def prepareAction(n: Int): RobotAction = actions(n)

  val obstacles = obstacleGenerator(this)
  val model = modelGenerator(this)
  
  def state: RobotState = RobotState(this) 

  private var stepsDone = 0
  def doAction(action: RobotAction): (RobotState, Double) = {
    action.execute(model, turnAngle, moveDistance)
    stepsDone += 1
    val curState = state
    goalReached = if (checkGoalReached(curState)) true else goalReached
    modelCrashed = if (checkModelCrashed(curState)) true else modelCrashed
    terminated = if (checkIsTerminated(curState)) true else terminated
    (curState, reward(curState))
  }
  
  private var terminated = false
  def isTerminated: Boolean = terminated
  // if goal reached, time out or crash happened - tells that episode is terminated
  private def checkIsTerminated(curState: RobotState): Boolean = {
    val st = state
    timedOut || goalReached || modelCrashed
  }
  
  private def timedOut: Boolean = stepsDone > timeOut
  
  private var goalReached = false
  def isGoalReached = goalReached
  private def checkGoalReached(curState: RobotState): Boolean = curState.goalDistance < RobotEnvironment.MaxDistanceToGoal

  private var modelCrashed = false
  def isModelCrashed = modelCrashed
  private def checkModelCrashed(curState: RobotState): Boolean = {
    val minDist = (Double.PositiveInfinity /: curState.ranges) ((x,y) => x min y)
    minDist < RobotEnvironment.MaxDistanceToGoal
    throw new Exception("Check for crashing to the bounds")
  }
  
  private def reward(curState: RobotState): Double = {
    if (goalReached)
      1.0
    else if (modelCrashed)
      0.5*Math.exp(-2.0*curState.goalDistance/width)
    else if (timedOut)
      0.3 + 0.5*Math.exp(-2.0*curState.goalDistance/width)
    else 0.0
  }
}

object RobotEnvironment {
  val MaxDistanceToGoal = 1.0	// max distance from model to goal so that we say model is at the goal 
}