package edu.torax.reinforcement.framework

trait Action {
  def encode: List[Double]		// Sparse encoding of the state for input to NN 
  def number: Int				// Number of action (for SeparateNNValueFunction
}
