package edu.torax.reinforcement.robot
import framework._
import gutils._
import robot._ 

object RobotEnvironment {
  private val actions: Array[RobotAction] = 
    Array(RobotForwardAction,RobotLeftForwardAction,RobotRightForwardAction,
          RobotTurnLeftAction,RobotTurnRightAction)
  val actionsCount = actions.size
  
//  private var minDst = 3.0
//  private var maxDst = 50.0
//  private var timeRange = 300000
//  private var iterNumber = 0
}

class RobotEnvironment (
  val settings: RobotSessionSettings
) extends Environment[RobotAction, RobotState] {
	import RobotEnvironment._
	val actionsCount = RobotEnvironment.actionsCount
 
  val width: Double = settings.envWidth								// width of the environment (room)
  val height: Double = settings.envHeight							// height of the environment (room)
  val timeOut: Int = settings.envTimeOut							// number of moves till time out
  val turnAngle: Double = settings.envTurnAngle				// what angle to turn left or right in one action
  val moveDistance: Double = settings.envMoveDistance	// what distance to move in one action
  val visionAngle: Double	= settings.envVisionAngle		// the central angle of vision sector
  
  val envBounds = List(Segment(0,0,0,height), Segment(0,height,width,height), 
                       Segment(width,height,width,0), Segment(0,0,width,0))
  

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
  
  private def checkGoalReached: Boolean = curState.goalDistance < settings.maxDistanceToGoal

  private var modelCrashed = false
  def isModelCrashed = modelCrashed
  
  private def checkModelCrashed: Boolean = {
    val modelBoundBox = model.boundBox
    val minObsDist = (Double.PositiveInfinity /: obstacles) ((x,y) => x min (y distanceTo modelBoundBox))
    val minBoundsDist = (Double.PositiveInfinity /: envBounds) ((x,y) => x min Polygon.distanceBetween(modelBoundBox, y))
    Math.min(minObsDist, minBoundsDist) < settings.maxDistanceToObs
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
      -0.005
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
 		if (obstacles exists (x => (x distanceTo modelBoundBox) < settings.maxDistanceToObs)) {
 			createModel
 		} else {
 			model
 		}
  }

  private def generateObstacles: Array[RobotObstacle] = {
    import settings._
    
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
    import settings._
    
 		val goal = Vector(obsGap + Math.random*(width - 2*obsGap), obsGap + Math.random*(height - 2*obsGap))
 		val obsDst = (Double.PositiveInfinity /: obstacles) {(d, obs) => d min (obs distanceTo goal) }
    val modelDst = Polygon.distanceBetween(model.boundBox, goal)
//    import RobotEnvironment._
//    val shapingDst = minDst + iterNumber * maxDst / timeRange
 		if (Math.min(obsDst, modelDst) < maxDistanceToObs /*|| modelDst > shapingDst*/ ) {
 			generateGoal
 		} else {
// 		  RobotEnvironment.iterNumber += 1
 			goal
 		}
  }
  
  def makeClone = {
    val env = new RobotEnvironment(settings)
    env.obstacles = this.obstacles map (x => x.makeClone)
    env.model = this.model.makeClone 
    env.goal = this.goal.clone
    env.goalReached = this.goalReached
    env.stepsDone = this.stepsDone
    env.modelCrashed = this.modelCrashed
    env.curState = RobotState(env)
    env
  }
  
  def toXML: xml.Elem = 
    <RobotEnvironment>
    	<obstacles>{obstacles map (_.toXML)}</obstacles>
      <model>{model.toXML}</model>
      <goal>{goal.toXML}</goal>
      <stepsDone>{stepsDone}</stepsDone>
      <terminated>{terminated}</terminated>
      <goalReached>{goalReached}</goalReached>
      <modelCrashed>{modelCrashed}</modelCrashed>
    </RobotEnvironment>
     
  def this(node: xml.NodeSeq, settings: RobotSessionSettings) = {
    this(settings)
    obstacles = ((node \ "obstacles" \ "RobotObstacle") map (PolygonalRobotObstacle.fromXML(_))).toArray
    model = RobotModel.fromXML(node \ "model" \ "RobotModel")
    goal = Vector.fromXML(node \ "goal" \ "vector")
    stepsDone = (node \ "stepsDone").text.toInt
    terminated = (node \ "terminated").text.toBoolean
    goalReached = (node \ "goalReached").text.toBoolean
    modelCrashed = (node \ "modelCrashed").text.toBoolean
    curState = RobotState(this)
  }
}
