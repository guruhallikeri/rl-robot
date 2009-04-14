package edu.torax.reinforcement.framework

trait Environment {
  def state: State
  def doAction(action: Action): (State, Double)
  def isTerminal(state: State): Boolean
}
