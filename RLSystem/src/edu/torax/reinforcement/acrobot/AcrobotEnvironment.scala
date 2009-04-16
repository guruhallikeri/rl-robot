package edu.torax.reinforcement.acrobot
import framework.Environment

class AcrobotEnvironment extends Environment[AcrobotAction, AcrobotState] {
  import Math._
  
  private val actions = Array(AcrobotTorquePlusAction, AcrobotTorqueMinusAction, AcrobotTorqueZeroAction)
  val actionsCount: Int = actions.size
  def prepareAction(n: Int): AcrobotAction = actions(n)
  
  val model = new AcrobotModel
  def state: AcrobotState = new AcrobotState(model)
  
  private val reward = -1.0
  def doAction(action: AcrobotAction): (AcrobotState, Double) = {
    action.doAction(model)
    (state, if (isTerminated) 0.0 else reward)
    
  }
  
//(defun acrobot-cm-angle (state)
//  (let* ((q1 (first state))
//         (q2 (third state))
//         (m1 acrobot-mass1)
//         (m2 acrobot-mass2)
//         (l1 acrobot-length1)
//         (lc1 acrobot-length-center-of-mass1)
//         (lc2 acrobot-length-center-of-mass2)
//         (x- (sin q2))
//         (x (/ (* m2 x-)
//               (+ m1 m2)))
//         (y- (+ l1 lc2 (cos q2)))
//         (y (/ (+ (* m1 lc1) (* m2 y-))
//               (+ m1 m2))))
//    (+ q1 (atan (/ x y)))))
  private def acrobotCMAngle = {
    import AcrobotModel._
    val q1 = model.theta1
    val q2 = model.theta2
    val xm = sin(q2)
    val x = m2*xm/(m1 + m2)
    val ym = l1 + lc2 + cos(q2)
    val y = (m1*lc1 + m2*ym)/(m1+m2)
    q1 + atan(x/y)
  }
  
//  (let* ((angle1 (first state))
//         (angle2 (third state))
//         (x (* acrobot-length1 (sin angle1)))
//         (y (- (* acrobot-length1 (cos angle1))))
//         (total-angle (+ angle1 angle2))
//         (handx (+ x (* acrobot-length2 (sin total-angle))))
//         (handy (+ y (- (* acrobot-length2 (cos total-angle))))))
//  (and ;(> handx 1)
//       ;(< handx 1.45)
//       (> handy 1)
//       )));(< handy 1.45))))
  private def terminalState = {
    val l1 = AcrobotModel.l1
    val l2 = AcrobotModel.l2
    val angle1 = model.theta1
    val angle2 = model.theta2
    val x = l1 * sin(angle1)
    val y = -l1 * cos(angle1)
    val handx = x + l2*sin(angle1 + angle2)
    val handy = y - l2*cos(angle1 + angle2)
    handy > (l1 - 1e-4)
  }
  def isTerminated: Boolean = terminalState

}
