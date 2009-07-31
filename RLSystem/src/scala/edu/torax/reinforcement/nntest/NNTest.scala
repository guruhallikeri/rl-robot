package edu.torax.reinforcement.nntest
import javax.swing._

object NNTest {
  def main(args : Array[String]) : Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run {
      	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
      	val form = new TestForm
      	()
      }
    })
  }
}