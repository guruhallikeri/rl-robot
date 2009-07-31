package edu.torax.reinforcement.nntest

import net.miginfocom.swing.MigLayout
import javax.swing._
//import java.awt._
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Component
import java.awt.RenderingHints
import java.awt.Color
import java.awt.BasicStroke
import java.awt.event._
import framework._

class TestForm extends JFrame("Neural Network Testing (c) 2009 Andrii Nakryiko") {
  var cnt = 0
	val nn = new UsualNeuralNetwork(1, Array(6, 6, 1),
			Array(NeuralNetwork.ActFunc.logisticNeg, NeuralNetwork.ActFunc.logisticNeg, NeuralNetwork.ActFunc.identity),
			LinearDecreasingFunction(0.01, 0.5, 200000), NeuralNetwork.UniformRandomInitializer(-0.3, 0.3))
	private val pane = new JPanel(new MigLayout("", "[grow,fill][180px]", "[grow,fill]"))
	def sinn(x: Double) = 0.5 * (3.5*x*(1-x)*Math.sin(2.8*Math.Pi*x) - 1.7 * Math.sin(3.7*Math.Pi*x) * (x+0.05) * (1.5 - x) / (1.0 + Math.exp(-5.7*x + 0.1))) //Math.sin(3.0*Math.Pi*x)

	val nnViz = new NNVisualizer(sinn, nn, 300)

	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setContentPane(pane)
	setSize(700, 300)

	pane.add(nnViz)

	private val controlPanel = new JPanel(new MigLayout("wrap 1", "[grow,fill]", "[]"))
	pane.add(controlPanel)

	private val stepButton = new JButton("Do one step")
	stepButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
		doStep()
	}})
	controlPanel.add(stepButton)

	private val thousandButton = new JButton("Do thousand step")
	thousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
		for (i<-0 until 1000) doStep()
	}})
	controlPanel.add(thousandButton)

	private val tenThousandButton = new JButton("Do ten thousand step")
	tenThousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
		for (i<-0 until 10000) doStep()
	}})
	controlPanel.add(tenThousandButton)

	private val hundredThousandButton = new JButton("Do hundred thousand step")
	hundredThousandButton.addActionListener(new ActionListener { def actionPerformed(event: ActionEvent) {
		for (i<-0 until 100000) doStep()
	}})
	controlPanel.add(hundredThousandButton)

 pane.requestFocusInWindow()
	setVisible(true);

	def doStep() {
		val x = Math.random
		val y = sinn(x)
		nn.tuneUp(List(x), List(y))
		//	  val samplesCount = 50
		//		val ind = (for(i <- 0 until samplesCount) yield i).toArray
		//		val rnd = new util.Random
		//		for (i <- 0 until samplesCount) {
			//			val sp = i + rnd.nextInt(samplesCount - i)
			//			val tmp = ind(i)
			//			ind(i) = ind(sp)
			//			ind(sp) = tmp
			//		}
		//println(ind.toList)
		//		val in = (for(i <- 0 until samplesCount) yield List(ind(i)/(samplesCount+0.0))).toArray
		//		val out = (for(i <- 0 until samplesCount) yield List(sinn(ind(i)/(samplesCount+0.0)))).toArray
		//    println(" -- " + x + " -- " + y)
		//    println("Output was: " + nn.calculate(scala.List(x)).head)
		//    val was = (for (i <- 0 to 10) yield nn.calculate(List(i/10.0)).head).toList
		//		for (i <- 0 until samplesCount) {
			//			nn.tuneUp(in(i), out(i))
			//		}
		//    println(", but now is: " + nn.calculate(List(x)))
		//    val is = (for (i <- 0 to 10) yield nn.calculate(List(i/10.0)).head).toList
		//   println(" ;==; " + nn.network(0)(0)(0) + " -- " + nn.network(0)(0)(1))
		//    println(" -= Was: " + was)
		//   println(" -= Is: " + is)
		nnViz.repaint
	}
}
