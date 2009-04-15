package edu.torax.reinforcement.framework

trait State {
  def encode: List[Double]		// Sparse encoding for NN
}
