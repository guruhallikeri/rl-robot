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

class NNVisualizer (val func: Double => Double, val nn: NeuralNetwork, val N: Int) extends Component {
  override def paint(gr: Graphics)
  {
		val g = gr.asInstanceOf[Graphics2D]
		val hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
		hints.add(new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED))
		g.setRenderingHints(hints)
  
		val offx = 5
    val offy = 5
    
    g.setColor(getBackground)
    g.fillRect(0, 0, size.width, size.height)
    
    val wid = size.width - 2*offx
    val hei = size.height - 2*offy
  
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
   	g.drawRect(offx-3, offy-3, wid + 6, hei + 6)

    val halfW = wid / 2.0
    val halfH = hei / 2.0

    var last = func(0.0)
    val step = 1.0 / N
    g.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
    for (i <- 1 to N) {
      val cur = func(i * step)
      g.drawLine(offx + (wid*(i-1)*step).toInt, offy + (halfH - last*halfH).toInt,
                 offx + (wid*i*step).toInt, offy + (halfH - cur*halfH).toInt)
      last = cur
    }

    var lastNN = nn.calculate(scala.List(0.0)).head
    g.setColor(Color.blue)
    for (i <- 1 to N) {
      val curNN = nn.calculate(scala.List(i * step)).head
      //println(i*step + " --- " + curNN + " --- " + lastNN)
      g.drawLine(offx + (wid*(i-1)*step).toInt, offy + (halfH - lastNN*halfH).toInt,
                 offx + (wid*i*step).toInt, offy + (halfH - curNN*halfH).toInt)
      lastNN = curNN
    }
  }
}
