package edu.torax.reinforcement.framework

trait Environment {
  abstract class State {
    def encode: List[Double]	// Sparse encoding for NN
  }
  abstract class Action {
    def encode: List[Double]	// Sparse encoding of the state for input to NN 
    def number: Int				// Number of action (for SeparateNNValueFunction
  }

  def actionsCount: Int
  def prepareAction(n: Int): Action

  def state: State
  def doAction(action: Action): (State, Double)
  def isTerminal(state: State): Boolean
}
