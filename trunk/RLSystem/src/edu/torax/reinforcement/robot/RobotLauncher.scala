package edu.torax.reinforcement.robot
import java.awt.event._
import java.awt._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

object RobotLauncher {
  def main(args : Array[String]) : Unit = {
    SwingUtilities.invokeLater(new Runnable {
      def run {
    	UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    	val robotForm = new RobotGUI
    	()
      }
    })
  }
}
