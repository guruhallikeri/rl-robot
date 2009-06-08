package edu.torax.reinforcement.robot

import java.awt.event._
import javax.swing._
import net.miginfocom.swing.MigLayout

import framework._
import gutils._

class RobotGUI extends JFrame("Robot Control (c) Andrii Nakryiko") {
  var session = new RobotSession(new RobotSessionSettings, processMessage _)
  
  def updateTitle(numer: Int, denom: Int) {
    if (numer == -1) {
    	setTitle("Robot Control - " + session.formatES + " " + session.formatRCT + " " + session.formatRCTPercentage)
    } else {
    	setTitle("Robot Control | <" + numer + "/" + denom + "> | " + 
                session.formatES + " " + session.formatRCT + " " + session.formatRCTPercentage)
    }
  }
  
  private var episodeStartStep = 0
  private def processMessage(event: Actor.Event): Unit = event match {
    case ev: Actor.EpisodeStarted[_,_] =>
    	println("Episode " + session.episodesCount + " Started")

    case ev: Actor.EpisodeFinished[_,_] =>
      println("Episode Finished with " + (session.stepsCount - episodeStartStep) + " steps")
    	episodeStartStep = session.stepsCount
      
    case ev: Actor.StepFinished[_,_] =>
  }

  // ----   Visual components creation ----
  private val robotPane = new JPanel(new MigLayout("", "[grow,fill][180px]", "[grow,fill]"))
  val envVisualizer = new RobotEnvironmentVisualizer
  envVisualizer.session = session
  
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setContentPane(robotPane);
  setSize(800, 600);

  robotPane.add(envVisualizer)
  
  private val controlPanel = new JPanel(new MigLayout("wrap 1", "[grow,fill]", "[]"))
  robotPane.add(controlPanel)
  
  private val debugButton = new RobotGUIButton("Debug information", controlPanel) {
    def action() {
      val env = session.environment
	    println("Environment obstacles:")
	    println(session.environment.obstacles.toList)
	    println("Model position: " + env.model.position + "; direction: " + env.model.direction)
	    println("Model bound box: " + env.model.boundBox)
	    println("Env state: " + env.state)
	    println("Env goal: " + env.goal)
     
	    println("")
	    println(new xml.PrettyPrinter(80, 2).format(session.toXML))
    }
  }

  private val resetButton = new RobotGUIButton("Reset environment", controlPanel) {
    def action() {
	  	session.recreateEnvironment
	  	envVisualizer.repaint
    }
  } 
  
  private val doStepButton = new RobotGUIButton("Do one step", controlPanel) {
  	def action() {
	    session.actor.doStep()
	    updateTitle(-1,-1)
	    envVisualizer.repaint()
	// 	RobotLearningProcess(actor, 1, RobotGUI.this).execute
    }
  }

  private def doNIterations(N: Int) {
    (new Thread(new Runnable { 
      def run {
        var i = 0
      	while (i < N) {
      	  session.actor.doStep()
      	  i += 1
      	  if (i % 1000 == 0) {
      	    updateTitle(i+1, N)
      	    envVisualizer.repaint
      	  }
      	}
      }
    })).start()
    updateTitle(-1,-1)
    envVisualizer.repaint
  }
  
  private val doThousandButton = new RobotGUIButton("Do 1'000 step", controlPanel) {
    def action() { doNIterations(1000) } }

  private val do100ThousandButton = new RobotGUIButton("Do 100'000 step", controlPanel) {
  	def action() { doNIterations(100000) } }

  private val do500ThousandButton = new RobotGUIButton("Do 500000 steps", controlPanel) {
    def action() { doNIterations(500000) } }
  
  private val doMillButton = new RobotGUIButton("Do 1 million steps", controlPanel) {
  	def action() { doNIterations(1000000) } }

  private val do2MillButton = new RobotGUIButton("Do 2 million steps", controlPanel) {
    def action() { doNIterations(2000000) } }

  private val do3MillButton = new RobotGUIButton("Do 3 million steps", controlPanel) {  
    def action() { doNIterations(3000000) } }

  private val screenStartButton = new RobotGUIButton("Start Screenshot Mode", controlPanel) {
  	def action() {
	    session.inScreenshotMode = true
	    envVisualizer.repaint
  	}
  }

  private val screenEndButton = new RobotGUIButton("End Screenshot Mode", controlPanel) {
  	def action() {
	    session.inScreenshotMode = false
	    envVisualizer.repaint
  	}
  }

  private var savedEnv: RobotEnvironment = null
  private val saveEnvButton = new RobotGUIButton("Save Environment", controlPanel) {
  	def action() {
	    savedEnv = session.environment.makeClone
  	}
  }

  private val loadEnvButton = new RobotGUIButton("Load Environment", controlPanel) {
  	def action() {
	    session.environment = savedEnv.makeClone
	    envVisualizer.repaint
  	}
  }
     
  private val sessionSaveButton = new RobotGUIButton("Save session", controlPanel) {
    def action() {
      val fc = new JFileChooser(".")
      
      if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
      	RobotXML.save(fc.getSelectedFile.getName, session.toXML)
      }
    }
  }

  private val sessionLoadButton = new RobotGUIButton("Load session", controlPanel) {
    def action() {
      val fc = new JFileChooser(".")
      if (fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
		    session = new RobotSession(xml.XML.loadFile(fc.getSelectedFile()), processMessage _)
      }
	    envVisualizer.session = session
	    envVisualizer.repaint
	    updateTitle(-1,-1)
    }
  }
  
  robotPane.requestFocusInWindow()
  setVisible(true);
}

abstract class RobotGUIButton(title: String, panel: JPanel) {
  def action(): Unit
  
  val button = new JButton(title)
  button.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) { action() } })
  panel.add(button)
}