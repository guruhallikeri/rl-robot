package edu.torax.reinforcement.framework

trait Environment[A <: Action, S <: State] {
  def actionsCount: Int
  def prepareAction(n: Int): A

  def state: S
  def doAction(action: A): (S, Double)
  def isTerminated: Boolean
}