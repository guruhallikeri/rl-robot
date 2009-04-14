package edu.torax.reinforcement.framework

trait ActionProvider {
  def actionsCount: Int
  def prepareAction(n: Int): Action
}
