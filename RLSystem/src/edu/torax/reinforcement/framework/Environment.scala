package edu.torax.reinforcement.framework

abstract class State {
  def encode: List[Double]		// Sparse encoding for NN
}
abstract class Action {
  def encode: List[Double]		// Sparse encoding of the state for input to NN 
  def number: Int				// Number of action (for SeparateNNValueFunction
}
trait Environment {
//  type State <: framework.State
//  type Action <: framework.Action 
  def actionsCount: Int
  def prepareAction(n: Int): Action

  def state: State
  def doAction(action: Action): (State, Double)
  def isTerminated: Boolean
}
