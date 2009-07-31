//package edu.torax.reinforcement.robot
//
//import javax.swing._
//import framework._
//
//case class RobotLearningProcess (actor: Actor[RobotAction,RobotState], iterations: Int, robotGUI: RobotGUI) 
//extends SwingWorker[Int, Int] {
//	// On the EDT
//	protected override def done() {
//		robotGUI.updateTitle(iterations, iterations)
//		robotGUI.envVisualizer.repaint
//	}
//	// On the EDT
//	protected override def process(progress: Int*) {
//		robotGUI.updateTitle(progress.last, iterations)
//		robotGUI.envVisualizer.repaint
//	}
//	// On a worker (background) thread
//	override def doInBackground(): Int = {
//	  for (i <- 0 until iterations) {
//	    actor.doStep()
//	    publish(i+1)
//	  }
//	  0
//	}
//}
