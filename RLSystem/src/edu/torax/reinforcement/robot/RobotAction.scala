package edu.torax.reinforcement.robot
import reinforcement.framework.Action

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
  def execute(model: RobotModel) { model turn RobotEnvironment.turnAngle; model move 1.0 }
}

case object RobotRightForwardAction extends RobotAction{
  val encode = List(0.0, 0.0, 1.0, 0.0, 0.0)
  val number = 2
  def execute(model: RobotModel) { model turn -RobotEnvironment.turnAngle; model move 1.0 }
}

case object RobotTurnRightAction extends RobotAction{
  val encode = List(0.0, 0.0, 0.0, 1.0, 0.0)
  val number = 3
  def execute(model: RobotModel) { model turn -RobotEnvironment.turnAngle }
}

case object RobotTurnLeftAction extends RobotAction{
  val encode = List(0.0, 0.0, 0.0, 0.0, 1.0)
  val number = 4
  def execute(model: RobotModel) { model turn RobotEnvironment.turnAngle }
}	
		