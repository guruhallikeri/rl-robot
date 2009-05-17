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
  val visionAngle: Double			// the central angle of vision sector
) extends Environment[RobotAction, RobotState] {
  private val actions: Array[RobotAction] = 
    Array(RobotForwardAction,RobotLeftForwardAction,RobotRightForwardAction,
          RobotTurnLeftAction,RobotTurnRightAction)

  val envBounds = List(Segment(0,0,0,height), Segment(0,height,width,height), 
                       Segment(width,height,width,0), Segment(0,0,width,0))
  

  def actionsCount: Int = actions.size
  def prepareAction(n: Int): RobotAction = actions(n)

  var obstacles = generateObstacles //new Array[RobotObstacle](0)
  var model = createModel
  var goal = generateGoal
  
  private var curState = RobotState(this)
  def state = curState

  private var stepsDone = 0
  def doAction(action: RobotAction): (RobotState, Double) = {
    action.execute(model, turnAngle, moveDistance)
    stepsDone += 1
    curState = RobotState(this)
    goalReached = if (checkGoalReached) true else goalReached
    modelCrashed = if (checkModelCrashed) true else modelCrashed
    terminated = if (checkIsTerminated) true else terminated
    (curState, reward)
  }
  
  private var terminated = false
  def isTerminated: Boolean = terminated
  // if goal reached, time out or crash happened - tells that episode is terminated
  private def checkIsTerminated: Boolean = timedOut || goalReached || modelCrashed
  
  def timedOut: Boolean = stepsDone > timeOut
  
  private var goalReached = false
  def isGoalReached = goalReached
  
  private def checkGoalReached: Boolean = curState.goalDistance < RobotEnvironment.MaxDistanceToGoal

  private var modelCrashed = false
  def isModelCrashed = modelCrashed
  
  private def checkModelCrashed: Boolean = {
    val modelBoundBox = model.boundBox
    val minObsDist = (Double.PositiveInfinity /: obstacles) ((x,y) => x min (y distanceTo modelBoundBox))
    val minBoundsDist = (Double.PositiveInfinity /: envBounds) ((x,y) => x min Polygon.distanceBetween(modelBoundBox, y))
    Math.min(minObsDist, minBoundsDist) < RobotEnvironment.MaxDistanceToObs
  }
  
  private def reward: Double = {
    if (goalReached) {
      //println("Goal reached!")
      1.0
  	} else if (modelCrashed) {
      //println("Model crashed!")
      -1.0 + 0.5*Math.exp(-2.0*curState.goalDistance/width)
    } else if (timedOut) {
      //println("Episode timed out!")
      -0.7 + 0.5*Math.exp(-2.0*curState.goalDistance/width)
    } else { 
      //-0.05
      -0.01
    }
  }
  
  private def createModel: RobotModel = {
 		//println("Robot creation trial")
 		val x = 2.0 + Math.random*(width - 4.0)
 		val y = 2.0 + Math.random*(height - 4.0)
 		val dx = Math.random - 0.5
 		val dy = Math.random - 0.5
 		val model = new SimpleRobotModel(x, y, dx, dy, 1.0, 1.0)
 		//println(x + " ---   " + y + " ---   " + dx + " ---   " + dy)
 		val modelBoundBox = model.boundBox
 		if (obstacles exists (x => (x distanceTo modelBoundBox) < RobotEnvironment.MaxDistanceToObs)) {
 			createModel
 		} else {
 			model
 		}
  }

  private def generateObstacles: Array[RobotObstacle] = {
    import RobotEnvironment._
    
    val N = obsMinNumber + (Math.random * (obsMaxNumber - obsMinNumber)).toInt
    //println("Obstacle count: " + N)
    var obs: List[RobotObstacle] = Nil
    for (i <- 0 until N) {
      val obstacle = PolygonalRobotObstacle.generate(obs, width, height, obsGap, obsMinRadius, obsMaxRadius, 0)
      if (obstacle != null) {
        obs = obstacle :: obs
      }
    }
    obs.toArray
  }
  
  def generateGoal: Vector = {
    import RobotEnvironment._
    
 		val goal = Vector(obsGap + Math.random*(width - 2*obsGap), obsGap + Math.random*(height - 2*obsGap))
 		val dst = Math.min((Double.PositiveInfinity /: obstacles) {(d, obs) => d min (obs distanceTo goal) },
                      Polygon.distanceBetween(model.boundBox, goal))
 		if (dst < RobotEnvironment.MaxDistanceToObs) {
 			generateGoal
 		} else {
 			goal
 		}
  }
  
  def makeClone = {
    val env = new RobotEnvironment(width, height, timeOut, turnAngle, moveDistance, visionAngle)
    env.obstacles = this.obstacles map (x => x.makeClone)
    env.model = this.model.makeClone 
    env.goal = this.goal.clone
    env.goalReached = this.goalReached
    env.stepsDone = this.stepsDone
    env.modelCrashed = this.modelCrashed
    env.curState = RobotState(env)
    env
  }
}

object RobotEnvironment {
  val MaxDistanceToGoal = 1.0	// max distance from model to goal so that we say model is at the goal
  val MaxDistanceToObs = 0.01
  val obsMinNumber = 4
  val obsMaxNumber = 10
  val obsMinRadius = 1.5
  val obsMaxRadius = 4.0
  val obsGap = 1.3
}