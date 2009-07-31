package edu.torax.reinforcement.acrobot
import framework.Environment

class AcrobotEnvironment extends Environment[AcrobotAction, AcrobotState] {
  import Math._
  
  private val actions = Array(AcrobotTorquePlusAction, AcrobotTorqueMinusAction, AcrobotTorqueZeroAction)
  val actionsCount: Int = actions.size
  def prepareAction(n: Int): AcrobotAction = actions(n)
  
  val model = new AcrobotModel
  def state: AcrobotState = new AcrobotState(model)
  
  private var steps = 0
  val maxStepsAllowed = 20000
  def timedOut = steps > maxStepsAllowed
  
  private val reward = -1.0
  def doAction(action: AcrobotAction): (AcrobotState, Double) = {
    action.doAction(model)
    steps += 1
    (state, if (terminalState) 0.0 else if (timedOut) reward else reward)
  }
  
  private def terminalState = {
    val l1 = AcrobotModel.l1
    val l2 = AcrobotModel.l2
    val angle1 = model.theta1
    val angle2 = model.theta2
    val x = l1 * sin(angle1)
    val y = -l1 * cos(angle1)
    val handx = x + l2*sin(angle1 + angle2)
    val handy = y - l2*cos(angle1 + angle2)
    handy > l1-1e-6 || y > 0.95*l1
  }
  def isTerminated: Boolean = terminalState || timedOut

}
