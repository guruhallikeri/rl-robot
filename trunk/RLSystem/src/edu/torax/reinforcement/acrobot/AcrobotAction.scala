package edu.torax.reinforcement.acrobot
import framework.Action

abstract class AcrobotAction extends Action {
  def encode: List[Double] = throw new Exception("Should not be called!")
  def doAction(model: AcrobotModel): Unit
}

case object AcrobotTorquePlusAction extends AcrobotAction {
  val number = 0
  def doAction(model: AcrobotModel): Unit = { model.torque(1.0); println("+1.0") }
}

case object AcrobotTorqueMinusAction extends AcrobotAction {
  val number = 1
  def doAction(model: AcrobotModel): Unit = { model.torque(-1.0); println("-1.0") }
}

case object AcrobotTorqueZeroAction extends AcrobotAction {
  val number = 2
  def doAction(model: AcrobotModel): Unit = { model.torque(0.0); println("0.0") }
}