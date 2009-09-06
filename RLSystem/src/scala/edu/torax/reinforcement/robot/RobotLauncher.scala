package edu.torax.reinforcement.robot
import java.awt.event._
import java.awt._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

object RobotLauncher {
  def main(args : Array[String]) : Unit = {
    if (args.size != 0) {
	    SwingUtilities.invokeLater(new Runnable {
	      def run {
	    	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
	    	//val form = new nntest.NNTest
	    	val robotForm = new RobotGUI
	    	()
	      }
	    })
    } else {
      // --<argname>=<val1>,<val2>,...,<val3>
      var dropCnt = 0
      var maxThreads = 4
//      if (args(0) startsWith "--maxthreads=") {
//        dropCnt += 1
//        maxThreads = args(0).substring(12).toInt
//      }
      //val testSuites = (args drop dropCnt) map (x => new RobotTestSuite(xml.XML.loadFile(x), "testbed"))
      val testSuites = Array(new RobotTestSuite(xml.XML.loadFile("""exper_changed_state2.xml"""), "testbed2"))
      for (testSuite <- testSuites) {
        testSuite.execute()
      }
      println("..:: All Tests Completed! ::..")
    }
    ()
  }
}
