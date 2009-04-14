package edu.torax.reinforcement
import framework._

class RobotNavigationEnvironment extends Environment {
  override def actionsCount: Int = 0
  override def prepareAction(n: Int): this.Action = null
  override def state: State = null
  override def doAction(action: Action): (State, Double) = throw new Exception("Not Implemented Method!")
  override def isTerminal(state: State): Boolean = true
}

object RobotNavigationEnvironment
{
  class State {
    def encode: List[Double] = {
      throw new Exception("Not Implemented Method!")
    }
  }
  class Action {
    def encode: List[Double] = {
      throw new Exception("Not Implemented Method!")
    }
  }
}
