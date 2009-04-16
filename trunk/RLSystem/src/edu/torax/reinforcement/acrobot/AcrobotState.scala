package edu.torax.reinforcement.acrobot
import framework.State

class AcrobotState(model: AcrobotModel) extends State {
  import AcrobotModel._
  def encode: List[Double] = List(
    (model.theta1 - minTheta1)/(maxTheta1 - minTheta1),
    (model.dotTheta1 - minDotTheta1)/(maxDotTheta1 - minDotTheta1),
    (model.theta2 - minTheta2)/(maxTheta2 - minTheta2),
    (model.dotTheta2 - minDotTheta2)/(maxDotTheta2 - minDotTheta2)
  )
}

object AcrobotState
{
  val dimensionality = 4
}