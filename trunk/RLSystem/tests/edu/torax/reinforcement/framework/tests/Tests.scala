package edu.torax.reinforcement.framework.tests
import Math._

object Tests {
  def main(args : Array[String]) : Unit = {
    (new NeuralNetworkTests).execute()
    println("All")

    val net = new NeuralNetwork (
      List(2, 3, 1), 
      List(NeuralNetwork.logisticNegFunction, NeuralNetwork.identityFunction), // (x:Double) => x),
      List(NeuralNetwork.logisticNegDerivative, NeuralNetwork.identityDerivative)
    ) {
      val gamma = 0.9
      val lambda = 0.85
      val alpha = 0.1
      def initializer = (Math.random - 0.5)*0.5
    }
    ()
    println("Output: " + net.calculate(List(0.6, 0.3)))
    for (i <- 0 to 0) {
      net.tuneUp(List(0.6, 0.3), -0.5);
      println("Output #" + i + ": " + net.calculate(List(0.6, 0.3)))
    }
    println("Output: " + net.calculate(List(0.5, 0.4)))
  } 
}
