package edu.torax.reinforcement
import framework._

class RobotNavigationEnvironment extends framework.Environment {
//  type Action = RobotNavigationEnvironment.Action
//  type State = RobotNavigationEnvironment.State

  import RobotNavigationEnvironment._
  private val actions: Array[Action] = Array(RobotForwardAction,RobotLeftForwardAction,RobotRightForwardAction,RobotTurnLeftAction,RobotTurnRightAction)
  
  def actionsCount: Int = actions.size
  def prepareAction(n: Int): Action = actions(n)

  val obstacles: Array[Obstacle] = createObstacles
  val model: RobotModel = createModel
  
  def state: State = throw new Exception("Not Implemented Method!")

  def doAction(action: Action): (State, Double) = {
   // action.execute(model)
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
  private def createObstacles: Array[Obstacle] = {
    throw new Exception("Not Implemented Method!")
  }
  
  // creates a robot model within current environment and places it at random so that
  // robot model doesn't overlap with obstacles
  private def createModel: RobotModel = {
    throw new Exception("Not Implemented Method!")
  }
}

object RobotNavigationEnvironment
{
  class RobotState extends State {
    def encode: List[Double] = {
      throw new Exception("Not Implemented Method!")
    }
  }
  
  val turnAngle = 15.0
  abstract class RobotAction extends Action {
    def execute(model: RobotModel): Unit
  }
  case object RobotForwardAction extends RobotAction{
    val encode = List(1.0, 0.0, 0.0, 0.0, 0.0)
    val number = 0
    def execute(model: RobotModel) { model move 1.0 } 
  }
  case object RobotLeftForwardAction extends RobotAction{
    val encode = List(0.0, 1.0, 0.0, 0.0, 0.0)
    val number = 1
    def execute(model: RobotModel) { model turn turnAngle; model move 1.0 }
  }
  case object RobotRightForwardAction extends RobotAction{
    val encode = List(0.0, 0.0, 1.0, 0.0, 0.0)
    val number = 2
    def execute(model: RobotModel) { model turn -turnAngle; model move 1.0 }
  }
  case object RobotTurnRightAction extends RobotAction{
    val encode = List(0.0, 0.0, 0.0, 1.0, 0.0)
    val number = 3
    def execute(model: RobotModel) { model turn -turnAngle }
  }
  case object RobotTurnLeftAction extends RobotAction{
    val encode = List(0.0, 0.0, 0.0, 0.0, 1.0)
    val number = 4
    def execute(model: RobotModel) { model turn turnAngle }
  }

  trait RobotModel {
    def position: (Double, Double)
    def direction: (Double, Double)
    def turn(angle: Double): Unit		// -180.0 <= angle <= 180.0
    def move(force: Double): Unit		// -1.0 <= force <= 1.0 	
  }

  class SimpleRobotModel(startX: Double, startY: Double, startDx: Double, startDy: Double) extends RobotModel {
    import GUtils._
    
    private var pos: Vector = (startX, startY)			// current position
    private var dir: Vector = (startDx, startDy)		// current moving direction
    def position: (Double, Double) = pos
    def direction: (Double, Double) = dir
    
    def turn(angle: Double): Unit = {		// -180.0 <= angle <= 180.0
      dir = dir rotate Math.toRadians(angle)
    }
    def move(force: Double): Unit = {		// -1.0 <= force <= 1.0
      assert(Math.abs(force) <= 1.0)
      pos = pos + dir * force
    }
  }
  
  abstract class Obstacle {
    
  }
}
