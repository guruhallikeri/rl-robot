package edu.torax.reinforcement.robot
import reinforcement.framework.Action

abstract class RobotAction extends Action {
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double): Unit
}

case object RobotForwardAction extends RobotAction{
  val encode = List(1.0, 0.0, 0.0, 0.0, 0.0)
  val number = 0
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double) { 
    model move moveDistance 
  } 
}

case object RobotLeftForwardAction extends RobotAction{
  val encode = List(0.0, 1.0, 0.0, 0.0, 0.0)
  val number = 1
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double) { 
    model turn turnAngle; model move moveDistance 
  }
}

case object RobotRightForwardAction extends RobotAction{
  val encode = List(0.0, 0.0, 1.0, 0.0, 0.0)
  val number = 2
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double) { 
    model turn -turnAngle; model move moveDistance 
  }
}

case object RobotTurnRightAction extends RobotAction{
  val encode = List(0.0, 0.0, 0.0, 1.0, 0.0)
  val number = 3
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double) { 
    model turn -turnAngle 
  }
}

case object RobotTurnLeftAction extends RobotAction{
  val encode = List(0.0, 0.0, 0.0, 0.0, 1.0)
  val number = 4
  def execute(model: RobotModel, turnAngle: Double, moveDistance: Double) { 
    model turn turnAngle 
  }
}	
		